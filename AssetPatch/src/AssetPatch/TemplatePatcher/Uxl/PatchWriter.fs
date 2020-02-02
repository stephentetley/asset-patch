// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Uxl


module PatchWriter =
    
    open System

    open AssetPatch.Base
    open AssetPatch.Base.CsvFile
    open AssetPatch.TemplatePatcher.Base.CompilerMonad
    open AssetPatch.TemplatePatcher.Uxl.Base
    open AssetPatch.TemplatePatcher.Uxl.PatchTypes

    let private makeCsvFile (rows : AssocList<string, string> list) : UxlCompilerMonad<CsvFileWithHeaders> = 
        match Seq.tryHead rows with
        | None -> throwError "makeCsv - empty"
        | Some row1 ->
            let headers = AssocList.keys row1
            let hs = List.ofArray headers
            let csvRows = List.map (AssocList.values << AssocList.select hs) rows |> List.toArray
            mreturn { Headers = headers; Rows = csvRows }

    // ************************************************************************
    // New FuncLocs file

    /// Render a list of new FuncLocs into a ChangeFile
    let private makeNewFuncLocsFile (rows : MmopNewFuncLoc list) : UxlCompilerMonad<CsvFileWithHeaders> = 
        rows
            |> List.sortBy (fun row -> row.FunctionalLocation.ToString()) 
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 