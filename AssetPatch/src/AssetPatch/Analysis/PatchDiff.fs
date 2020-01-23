// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause


namespace AssetPatch.Analysis

module PatchDiff = 

    open AssetPatch.Base
    open AssetPatch.Base.ChangeFile

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
                    work xs1 ys1 (fun vs -> RowInBoth(keyLeft, innerDiff) :: vs)
                | i when i < 0 -> 
                    // ys is in front of xs, so accumulate head of xs, still consider all of ys
                    work xs1 ys (fun vs -> RowInLeft(keyLeft) :: vs)
                | i when i > 0 -> 
                    // xs is in front of ys, so accumulate head of ys, still consider all of xs
                    work xs ys1 (fun vs -> RowInRight(keyLeft) :: vs)
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

