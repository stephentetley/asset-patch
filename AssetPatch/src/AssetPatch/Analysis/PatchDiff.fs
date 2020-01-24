// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause


namespace AssetPatch.Analysis

module PatchDiff = 
    
    open System.IO

    open Giraffe.GiraffeViewEngine

    open AssetPatch.Base
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.Parser

    type AssocDiff<'Key, 'T> = 
        | AssocInLeft of 'Key * 'T
        | AssocInRight of 'Key * 'T
        | AssocSame of 'Key * 'T
        | AssocDiff of 'Key * 'T *'T


    let diffAssocLists (assocLeft: AssocList<string, string>)
                       (assocRight: AssocList<string, string>) : AssocDiff<string, string> list = 
        let keys : string list = 
            Array.append (AssocList.keys assocLeft) (AssocList.keys assocRight) 
                |> Array.toList
                |> List.sort 
                |> List.distinct
        let rec work ks cont = 
            match ks with
            | [] -> cont []
            | k1 :: rs -> 
                match (AssocList.tryFind k1 assocLeft, AssocList.tryFind k1 assocRight) with
                | None, None -> failwith "impossible"
                | Some(a), Some(b) -> 
                    if a = b then 
                        work rs (fun xs -> cont (AssocSame(k1, a) :: xs))
                    else work rs (fun xs -> cont (AssocDiff(k1, a, b) :: xs))
                | None, Some(b) -> 
                    work rs (fun xs -> cont (AssocInRight(k1, b) :: xs))
                | Some(a), None -> 
                    work rs (fun xs -> cont (AssocInLeft(k1, a) :: xs))
        work keys (fun xs -> xs)

    /// 'Key for Equi is EQUI field
    type RowDiff<'Key> = 
        | RowInLeft of 'Key
        | RowInRight of 'Key 
        | RowInBoth of 'Key * AssocDiff<string, string> list


    /// This will degenerate if the key field is not in the rows of the ChangeFile!
    let diffRows (sortKeyField: string) 
                    (rowsLeft: AssocList<string, string> list)
                    (rowsRight: AssocList<string, string> list) : RowDiff<string> list = 
        let detGetKey = Option.defaultValue "_ERROR_" << AssocList.tryFind sortKeyField
        let rows1 = List.sortBy detGetKey rowsLeft
        let rows2 = List.sortBy detGetKey rowsRight
        let rec work xs ys cont = 
            match xs, ys with
            | xs1, [] -> cont (List.map (RowInLeft << detGetKey) xs1)
            | [], ys1 -> cont (List.map (RowInRight << detGetKey) ys1)
            | x :: xs1, y :: ys1 -> 
                let keyLeft = detGetKey x
                match compare keyLeft (detGetKey y)  with
                | i when i = 0 -> 
                    // item is same row - check for changes 
                    let innerDiff = diffAssocLists x y 
                    work xs1 ys1 (fun vs -> cont(RowInBoth(keyLeft, innerDiff) :: vs))
                | i when i < 0 -> 
                    // ys is in front of xs, so accumulate head of xs, still consider all of ys
                    work xs1 ys (fun vs -> cont (RowInLeft(keyLeft) :: vs))
                | i when i > 0 -> 
                    // xs is in front of ys, so accumulate head of ys, still consider all of xs
                    work xs ys1 (fun vs -> cont (RowInRight(keyLeft) :: vs))
                | i -> failwithf "differenceL - Weird (impossible) pattern failure: %i" i
        work rows1 rows2 (fun xs -> xs)



    let diffChangeFiles (sortKeyField: string) 
                        (fileLeft: ChangeFile)
                        (fileRight: ChangeFile) : Result<RowDiff<string> list, string>  = 
        if fileLeft.Header.EntityType <> fileRight.Header.EntityType then
            Error "Mismatched file types"
        else if fileLeft.ColumnHeaders <> fileRight.ColumnHeaders then
            Error "Mismatched Attribute lists"
        else 
            diffRows sortKeyField (fileLeft.RowAssocs ()) (fileRight.RowAssocs ()) |> Ok

    // ************************************************************************
    // Report


    let private headerTable (tableTitle: string) (changeFile: ChangeFile): XmlNode = 
        let dateValue = changeFile.Header.DateTime.ToString(format="yyyyMMdd")
        let timeValue = changeFile.Header.DateTime.ToString(format="HHmmss")
        table [] [
            tbody [] [
                tr [] [
                    td [] [str tableTitle]
                ]
                tr [] [
                    td [] [str "Data Model:"]
                    td [] [changeFile.Header.DataModel.ToString() |> str]
                ]
                tr [] [
                    td [] [str "Entity Type:"]
                    td [] [changeFile.Header.EntityType.ToString() |> str]
                ]
                tr [] [
                    td [] [str "Variant:"]
                    td [] [changeFile.Header.Variant |> str]
                ]
                tr [] [
                    td [] [str "User:"]
                    td [] [changeFile.Header.User |> str]
                ]
                tr [] [
                    td [] [str "Date:"]
                    td [] [dateValue |> str]
                ]
                tr [] [
                    td [] [str "Time:"]
                    td [] [timeValue |> str]
                ]
            ]
        ]

    let assocDiffStr (diff: AssocDiff<string, string>): XmlNode option = 
        match diff with
        | AssocInLeft(name, value) -> sprintf "%s: missing in after '%s'" name value |> str |> Some
        | AssocInRight(name, value) -> sprintf "%s: missing in before '%s'" name value |> str |> Some
        | AssocSame(_,_) -> None
        | AssocDiff(name, v1, v2) -> sprintf "%s: '%s' => '%s'" name v1 v2 |> str |> Some

    let rowChangesTd (diffs: AssocDiff<string, string> list) : XmlNode = 
        let texts = 
            diffs 
                |> List.map assocDiffStr
                |> List.choose id

        td [] texts
        

    let rowDiffTr (diff1: RowDiff<string>) : XmlNode = 
        match diff1 with
        | RowInLeft key -> 
            tr [] [
                td [] [
                    sprintf "Error: Row %s missing after changes" key |> str
                ]
            ]
        | RowInRight key -> 
            tr [] [
                td [] [
                    sprintf "Error: Row %s added after changes" key |> str
                ]
            ]
        | RowInBoth(key, diffs) -> 
            tr [] [
                td [] [
                    sprintf "%s" key |> str
                ] 
                rowChangesTd diffs
            ]

    
    let private diffsTableStats (diffs: RowDiff<string> list): XmlNode = 
        p [] [sprintf "Row Count: %i" (List.length diffs) |> str]


    let private diffsTable (diffs: RowDiff<string> list): XmlNode = 
        table [] [
            tbody [] (List.map rowDiffTr diffs)
        ]


    let private diffReport (before: ChangeFile) (after: ChangeFile) (diffs: RowDiff<string> list): XmlNode = 
        html [] [
            head [] [
                title [] [str "Changes (before and after)"]
                link [ 
                    _rel "stylesheet"
                    _type "text/css"
                    _href "github.css"
                ]
            ]
            body [] [
                h1 [_id "changes-report"] [ str "Changes Report"]
                headerTable "Before" before
                headerTable "After" after
                diffsTableStats diffs
                diffsTable diffs
            ]        
        ]

    let reportRowDiffs (sortfield: string) (beforeFilePath: string) (afterFilePath: string) (outputPath: string) : Result<Unit, string> = 
        match readChangeFile beforeFilePath with
        | Error msg1 -> Error msg1
        | Ok beforeChanges -> 
            match readChangeFile afterFilePath with
            | Error msg2 -> Error msg2
            | Ok afterChanges ->  
                match diffChangeFiles sortfield beforeChanges afterChanges with
                | Error msg -> Error msg
                | Ok diffs -> 
                    diffReport beforeChanges afterChanges diffs
                        |> renderHtmlDocument
                        |> fun x -> File.WriteAllText(path = outputPath, contents = x)
                    Ok ()


