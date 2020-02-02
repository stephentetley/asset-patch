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

    let private writeChangesAsCsv (changeFile : CsvFileWithHeaders)  
                                        (outputPath: string) : UxlCompilerMonad<unit> =
        liftAction (fun () -> CsvFile.writeCsvFile csvDefaults changeFile outputPath)

    // ************************************************************************
    // Change Request Details file
    /// Render a list of new FuncLocs into a ChangeFile
    
    let changeRequestKey (row: MmopChangeRequest): string = 
        let part1 = Option.defaultValue "ZZZZZ" <| Option.map (fun floc -> floc.ToString()) row.FunctionalLocation
        let part2 = Option.defaultValue "ZZZZZ" row.EquipmentId
        part1 + "#" + part2
    
    let private makeNewChangeRequestsFile (rows : MmopChangeRequest list) : UxlCompilerMonad<CsvFileWithHeaders> = 
        rows
            |> List.sortBy changeRequestKey
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write a list of new FuncLocs to Csv
    let writeNewChangeRequestsFile (changeRequests : MmopChangeRequest list)
                                    (outPath: string) : UxlCompilerMonad<unit> = 
        compile { 
            match changeRequests with
            | [] -> return ()
            | _ -> 
                let! changes = makeNewChangeRequestsFile changeRequests
                do! writeChangesAsCsv changes outPath
                return ()
            }

    // ************************************************************************
    // New FuncLocs file

    /// Render a list of new FuncLocs into a Csv for the `Functional Location Data` tab
    let private makeNewFuncLocsFile (rows : MmopNewFuncLoc list) : UxlCompilerMonad<CsvFileWithHeaders> = 
        rows
            |> List.sortBy (fun row -> row.FunctionalLocation.ToString()) 
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write a list of new FuncLocs to Csv
    let writeNewFuncLocsFile (funcLocs : MmopNewFuncLoc list)
                                (outPath: string) : UxlCompilerMonad<unit> = 
        compile { 
            match funcLocs with
            | [] -> return ()
            | _ -> 
                let! changes = makeNewFuncLocsFile funcLocs
                do! writeChangesAsCsv changes outPath
                return ()
            }
