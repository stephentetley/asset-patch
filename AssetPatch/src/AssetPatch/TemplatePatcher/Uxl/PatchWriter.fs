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
    // Change Request Details csv

    /// Render a list of new FuncLocs to Csv for the `Change Request Details` tab
    let private makeMmopChangeRequestsFile (rows : MmopChangeRequest list) : UxlCompilerMonad<CsvFileWithHeaders> = 
        rows
            |> List.sortBy (fun (x: MmopChangeRequest) -> x.SortKey)
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write a list of new FuncLocs to Csv
    let writeMmopChangeRequestsFile (changeRequests : MmopChangeRequest list)
                                    (outPath: string) : UxlCompilerMonad<unit> = 
        compile { 
            match changeRequests with
            | [] -> return ()
            | _ -> 
                let! changes = makeMmopChangeRequestsFile changeRequests
                do! writeChangesAsCsv changes outPath
                return ()
            }

    // ************************************************************************
    // New FuncLocs csv

    /// Render a list of new FuncLocs to Csv for the `Functional Location Data` tab
    let private makeMmopFuncLocsFile (rows : MmopNewFuncLoc list) : UxlCompilerMonad<CsvFileWithHeaders> = 
        rows
            |> List.sortBy (fun row -> row.FunctionalLocation.ToString()) 
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write a list of new FuncLocs to Csv
    let writeMmopFuncLocsFile (source : MmopNewFuncLoc list)
                                (outPath: string) : UxlCompilerMonad<unit> = 
        compile { 
            match source with
            | [] -> return ()
            | _ -> 
                let! changes = makeMmopFuncLocsFile source
                do! writeChangesAsCsv changes outPath
                return ()
            }

    // ************************************************************************
    // New FuncLocs multilingual csv file

    /// Render a list of new FuncLocs into a Csv for the `FL-Multilingual Text` tab
    let private makeMmopFLMultilingualTextsFile (rows : MmopNewFlocMultilingualText list) : UxlCompilerMonad<CsvFileWithHeaders> = 
        rows
            |> List.sortBy (fun row -> row.FunctionalLocation.ToString()) 
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write a list of new FuncLocs to Csv
    let writeMmopFLMultilingualTextsFile (source : MmopNewFlocMultilingualText list)
                                        (outPath: string) : UxlCompilerMonad<unit> = 
        compile { 
            match source with
            | [] -> return ()
            | _ -> 
                let! changes = makeMmopFLMultilingualTextsFile source
                do! writeChangesAsCsv changes outPath
                return ()
            }

    // ************************************************************************
    // New FuncLocs class/characteristics csv file

    /// Render a list of new FuncLocs into a Csv for the `FL-Classification` tab
    let private makeMmopFLClassificationsFile (rows : MmopFlocClassification list) : UxlCompilerMonad<CsvFileWithHeaders> = 
        rows
            |> List.sortBy (fun row -> row.FunctionalLocation.ToString()) 
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write a list of new FuncLocs to Csv
    let writeMmopFLClassificationsFile (source : MmopFlocClassification list)
                                        (outPath: string) : UxlCompilerMonad<unit> = 
        compile { 
            match source with
            | [] -> return ()
            | _ -> 
                let! changes = makeMmopFLClassificationsFile source
                do! writeChangesAsCsv changes outPath
                return ()
            }

    // ************************************************************************
    // New Equipment csv file

    /// Render a list of new FuncLocs into a Csv for the `Equipment Data` tab
    let private makeMmopNewEquiFile (rows : MmopNewEqui list) : UxlCompilerMonad<CsvFileWithHeaders> = 
        rows
            |> List.sortBy (fun row -> row.EquiId) 
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write a list of new FuncLocs to Csv
    let writeMmopNewEquisFile (source : MmopNewEqui list)
                                (outPath: string) : UxlCompilerMonad<unit> = 
        compile { 
            match source with
            | [] -> return ()
            | _ -> 
                let! changes = makeMmopNewEquiFile source
                do! writeChangesAsCsv changes outPath
                return ()
            }

    // ************************************************************************
    // New Equipment multilingual csv file

    /// Render a list of new equipment into a Csv for the `EQ-Multilingual Text` tab
    let private makeMmopEQMultilingualTextsFile (rows : MmopNewEquiMultilingualText list) : UxlCompilerMonad<CsvFileWithHeaders> = 
        rows
            |> List.sortBy (fun row -> row.EquiId) 
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write a list of new FuncLocs to Csv
    let writeMmopEQMultilingualTextsFile (source : MmopNewEquiMultilingualText list)
                                        (outPath: string) : UxlCompilerMonad<unit> = 
        compile { 
            match source with
            | [] -> return ()
            | _ -> 
                let! changes = makeMmopEQMultilingualTextsFile source
                do! writeChangesAsCsv changes outPath
                return ()
            }

    // ************************************************************************
    // New Equipment class/characteristics csv file

    /// Render a list of new FuncLocs into a Csv for the `FL-Classification` tab
    let private makeMmopEQClassificationsFile (rows : MmopEquiClassification list) : UxlCompilerMonad<CsvFileWithHeaders> = 
        rows
            |> List.sortBy (fun row -> row.EquiId) 
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write a list of new FuncLocs to Csv
    let writeMmopEQClassificationsFile (source : MmopEquiClassification list)
                                        (outPath: string) : UxlCompilerMonad<unit> = 
        compile { 
            match source with
            | [] -> return ()
            | _ -> 
                let! changes = makeMmopEQClassificationsFile source
                do! writeChangesAsCsv changes outPath
                return ()
            }