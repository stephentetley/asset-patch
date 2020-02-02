// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Uxl


module PatchWriter =
    
    open System

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.Uxl.Base
    open AssetPatch.TemplatePatcher.Uxl.PatchTypes

    //let private makeCsvFile (rows : AssocList<string, string> list) : UxlCompilerMonad<CsvFile> = 
    //    compile {
    //        let! user = asks (fun x -> x.UserName)
    //        let timestamp = DateTime.Now
    //        let! headerRow = getHeaderRow rows
    //        let! header = makeHeader entityType user variantName timestamp 
    //        return { 
    //            Header = header
    //            Selection = None
    //            HeaderDescriptions = getHeaderDescriptions entityType headerRow |> Some
    //            HeaderRow = headerRow
    //            DataRows = List.map DataRow.FromAssocList rows 
    //        }          
    //    }