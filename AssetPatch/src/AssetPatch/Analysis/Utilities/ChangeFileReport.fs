// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Anaylsis.Utilities

module ChangeFileReport =

    open System.IO 

    open Giraffe.GiraffeViewEngine

    open AssetPatch.Base
    open AssetPatch.Base.Aiw.ChangeFile
    open AssetPatch.Base.Aiw.Acronyms
    open AssetPatch.Base.Aiw.ChangeFileParser
    

    let private cellValue (src : string) : XmlNode = 
        match src with
        | null | "" -> str ""
        | _ -> str src

    let private linkToTop : XmlNode = 
        a [_href "#top"] [str  "Back to top"]


    let private fileType (source : FileType) : XmlNode = 
        let value = 
            match source with
            | Download -> "Download" 
            | Upload -> "Upload"
        value |> str 

    let private dataModel (source : DataModel) : XmlNode = 
        match source with
        | U1 -> "U1" |> str

    let private entityType (source : EntityType) : XmlNode = 
        let name = 
            match source with
            | FuncLoc -> "FUNCLOC"
            | ClassFloc -> "CLASSFLOC"
            | ValuaFloc -> "VALUAFLOC"
            | Iflotx -> "IFLOTX"
            | Equi -> "EQUI"
            | ClassEqui -> "CLASSEQUI" 
            | ValuaEqui -> "VALUAEQUI"
            | Eqmltxt -> "EQMLTXT"
        name |> str
    
    let private variant (name : string) : XmlNode = 
        name |> str



    let private headerTable (source : FileHeader) : XmlNode = 
        let makeRow (name : string) (value : XmlNode) : XmlNode = 
            tr [] [
                td [] [name |> str]
                td [] [value]
            ]
        table [] [
            tr [] [
                td [_colspan "2"] [fileType source.FileType]
            ]
            makeRow "Data Model:"     (dataModel source.DataModel)
            makeRow "Entity Type:"    (entityType source.EntityType)
            makeRow "Variant:"        (variant source.Variant)
            makeRow "User:"           (str source.User)
            makeRow "Date:"           (source.DateTime.ToString(format="yyyyMMdd") |> str)
            makeRow "Time:"           (source.DateTime.ToString(format="HHmmss") |> str)
        ]
        
       
    let private dataAssocTable (entityType : EntityType) 
                       (source : AssocList<string, string>) : XmlNode = 
        let headings =
            tr [] [
                th [] [str "Index"]
                th [] [str "Field"]
                th [] [str "Description"]
                th [] [str "Value"]
            ]
        let makeRow (ix :int) (name, value) : XmlNode =
            tr [] [
                td [] [ ix + 1 |> string |> str ]
                td [] [ name |> str]
                td [] [ decodeAcronym entityType name 
                            |> Option.defaultValue "" |> str ]
                td [] [value |> str]
            ]
        table [] [
            headings
            yield! (List.mapi makeRow (AssocList.toList source))    
        ]

    let private dataRows (patch : AiwChangeFile) : XmlNode list = 
        let rowAssocs = patch.RowAssocs ()
        let rowCount = rowAssocs.Length
        let makeTable ix (rowAssoc : AssocList<string, string>) = 
            [
                h2 [] [sprintf "Row %i of %i" (ix+1) rowCount |> str]
                dataAssocTable patch.Header.EntityType rowAssoc
                p [] [linkToTop]
            ]
        List.mapi makeTable rowAssocs |> List.concat



    let patchToHtml (patch : AiwChangeFile) : XmlNode =
        let titleText = sprintf "Patch Summary (%s)" (patch.Header.EntityType.ToString())
        html [] [
            head [] [
                title [] [str titleText]
                link [ 
                    _rel "stylesheet"
                    _type "text/css"
                    _href "github.css"
                ]
            ]
            body [] [
                h1 [] [str "Patch Report"]
                headerTable patch.Header
                yield! dataRows patch
            ]
        ]

    let private genHtml (htmlFileName : string) (patch : AiwChangeFile) : Result<unit, string> = 
        let html = patchToHtml patch |> renderHtmlDocument
        File.WriteAllText(path = htmlFileName, contents = html) |> Ok


    
    let changeFileReport (pathToCssSytlesheet : string) 
                         (outputDirectory : string) 
                         (inputPatch : string)  : Result<unit, string> = 
        let outputHtmlFile = 
            Path.GetFileName(inputPatch) 
                |> fun s -> Path.ChangeExtension(path = s, extension = "html")
                |> fun s -> Path.Combine(outputDirectory, s)
        match readAiwChangeFile inputPatch with
        | Result.Error msg -> Result.Error msg
        | Result.Ok ans -> 
            let cssDest = 
                Path.GetFileName(pathToCssSytlesheet) 
                    |> fun s -> Path.Combine(outputDirectory, s)
            if not <| File.Exists(cssDest) then
                File.Copy(sourceFileName =pathToCssSytlesheet, destFileName =  cssDest)
            else ()
            genHtml outputHtmlFile ans
            
        