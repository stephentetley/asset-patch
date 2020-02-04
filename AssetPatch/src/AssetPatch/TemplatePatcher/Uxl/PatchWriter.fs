// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Uxl


module PatchWriter =
    
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

    /// Render data for the `Change Request Details` tab
    let private makeChangeRequestDetails (rows : ChangeRequestDetails list) : UxlCompilerMonad<CsvFileWithHeaders> = 
        rows
            |> List.sortBy (fun (x: ChangeRequestDetails) -> x.SortKey)
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write data for the `Change Request Details` tab
    let writeChangeRequestDetails (changeRequests : ChangeRequestDetails list)
                                    (outPath: string) : UxlCompilerMonad<unit> = 
        compile { 
            match changeRequests with
            | [] -> return ()
            | _ -> 
                let! changes = makeChangeRequestDetails changeRequests
                do! writeChangesAsCsv changes outPath
                return ()
            }


    // ************************************************************************
    // FuncLocs csv

    /// Render data for the `Functional Location Data` tab
    let private makeFunctionalLocationData (rows : FunctionalLocationData list) : UxlCompilerMonad<CsvFileWithHeaders> = 
        rows
            |> List.sortBy (fun row -> row.FunctionalLocation.ToString()) 
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write data for the `Functional Location Data` tab
    let writeFunctionalLocationData (source : FunctionalLocationData list)
                                (outPath: string) : UxlCompilerMonad<unit> = 
        compile { 
            match source with
            | [] -> return ()
            | _ -> 
                let! changes = makeFunctionalLocationData source
                do! writeChangesAsCsv changes outPath
                return ()
            }

    // ************************************************************************
    // FuncLocs multilingual text csv

    /// Render data for the `FL-Multilingual Text` tab
    let private makeFlocMultilingualText (rows : FlocMultilingualText list) : UxlCompilerMonad<CsvFileWithHeaders> = 
        rows
            |> List.sortBy (fun row -> row.FunctionalLocation.ToString()) 
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write data for the `FL-Multilingual Text` tab
    let writeFlocMultilingualText (source : FlocMultilingualText list)
                                    (outPath: string) : UxlCompilerMonad<unit> = 
        compile { 
            match source with
            | [] -> return ()
            | _ -> 
                let! changes = makeFlocMultilingualText source
                do! writeChangesAsCsv changes outPath
                return ()
            }

    // ************************************************************************
    // FuncLocs class/characteristics csv

    /// Render data for the `FL-Classification` tab
    let private makeFlocClassification (rows : FlocClassification list) : UxlCompilerMonad<CsvFileWithHeaders> = 
        rows
            |> List.sortBy (fun row -> row.FunctionalLocation.ToString()) 
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write data for the `FL-Classification` tab
    let writeFlocClassification (source : FlocClassification list)
                                        (outPath: string) : UxlCompilerMonad<unit> = 
        compile { 
            match source with
            | [] -> return ()
            | _ -> 
                let! changes = makeFlocClassification source
                do! writeChangesAsCsv changes outPath
                return ()
            }

    // ************************************************************************
    // Equipment csv

    /// Render data for the `Equipment Data` tab
    let private makeEquipmentData (rows : EquimentData list) : UxlCompilerMonad<CsvFileWithHeaders> = 
        rows
            |> List.sortBy (fun row -> row.EquipmentId) 
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write data for the `Equipment Data` tab
    let writeEquipmentData (source : EquimentData list)
                                (outPath: string) : UxlCompilerMonad<unit> = 
        compile { 
            match source with
            | [] -> return ()
            | _ -> 
                let! changes = makeEquipmentData source
                do! writeChangesAsCsv changes outPath
                return ()
            }

    // ************************************************************************
    // Equipment multilingual csv

    /// Render data for the `EQ-Multilingual Text` tab
    let private makeEquiMultilingualText (rows : EquiMultilingualText list) : UxlCompilerMonad<CsvFileWithHeaders> = 
        rows
            |> List.sortBy (fun row -> row.EquipmentId) 
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write data for the `EQ-Multilingual Text` tab
    let writeEquiMultilingualText (source : EquiMultilingualText list)
                                    (outPath: string) : UxlCompilerMonad<unit> = 
        compile { 
            match source with
            | [] -> return ()
            | _ -> 
                let! changes = makeEquiMultilingualText source
                do! writeChangesAsCsv changes outPath
                return ()
            }

    // ************************************************************************
    // Equipment class/characteristics csv

    /// Render data for the `FL-Classification` tab
    let private makeEquiClassification (rows : EquiClassification list) : UxlCompilerMonad<CsvFileWithHeaders> = 
        rows
            |> List.sortBy (fun row -> row.EquipmentId) 
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write data for the `FL-Classification` tab
    let writeEquiClassification (source : EquiClassification list)
                                (outPath: string) : UxlCompilerMonad<unit> = 
        compile { 
            match source with
            | [] -> return ()
            | _ -> 
                let! changes = makeEquiClassification source
                do! writeChangesAsCsv changes outPath
                return ()
            }