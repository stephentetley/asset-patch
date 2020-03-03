// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace AssetPatch.EdcPatcher


module AiwPatcher =
    
    open System.IO

    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.Base.GenerateMonad
    open AssetPatch.TemplatePatcher.Aiw.Base
    open AssetPatch.TemplatePatcher.Aiw.FileTypes
    open AssetPatch.TemplatePatcher.Aiw.Emitter
    open AssetPatch.TemplatePatcher.Aiw.Generate

    open AssetPatch.EdcPatcher.InputData
    open AssetPatch.EdcPatcher.EdcTemplate


    type AiwOptions = 
        { UserName : string 
          OutputDirectory : string
          WorkListPath : string
          }

    
    /// Note - we need to be able to create floc patches at different
    /// levels in the tree (according to what already exists).
    let private phase1ProcessRow (row : WorkListRow) : AiwGenerate<FlocCreateData> = 
        let rootPath = FuncLocPath.Create row.``S4 Root FuncLoc``
        match rootPath.Level with
        | 1 -> applyFunction        (makeEDC row) rootPath >>= functionalLocationEmitMmopCreate
        | 2 -> applyProcessGroup    (makeLQD row) rootPath >>= functionalLocationEmitMmopCreate
        | 3 -> applyProcess         (makeRGM row) rootPath >>= functionalLocationEmitMmopCreate
        | 4 -> applySystem          (makeSYS row) rootPath >>= functionalLocationEmitMmopCreate
        | x when x > 4 && x < 8 -> mreturn (FlocCreateData.Empty ())
        | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (rootPath.ToString()) x)



    let runAiwEdcPatcherPhase1 (opts : AiwOptions) : Result<unit, string> = 
        let userEnv : AiwEnv = { UserName = opts.UserName; EquiIndices = None }
        runGenerate userEnv
            <| generate { 
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)             
                let! worklist = liftAction <| fun _ -> readWorkList opts.WorkListPath                
                let! flocCreateData = mapM phase1ProcessRow worklist |>> FlocCreateData.Concat               
                do! writeFlocCreateData opts.OutputDirectory "edc_patch" flocCreateData                
                return ()
            }


    //let private phase2ProcessRow (row : WorkListRow) : AiwCompilerMonad<Phase2Data> = 
    //    let rootPath = FuncLocPath.Create row.``S4 Root FuncLoc``
    //    match rootPath.Level with
    //    | 1 -> applyFlocTemplate1 (rootPath, row) makeEDC >>= functionEmitPhase2
    //    | 2 -> applyFlocTemplate1 (rootPath, row) makeLQD >>= processGroupEmitPhase2
    //    | 3 -> applyFlocTemplate1 (rootPath, row) makeRGM >>= processEmitPhase2
    //    | 4 -> applyFlocTemplate1 (rootPath, row) makeSYS >>= systemEmitPhase2
    //    | x when x > 4 && x < 8 -> applyFlocTemplate1 (rootPath, row) makeLevelTransmitter >>= equipmentEmitPhase2
    //    | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (rootPath.ToString()) x)


    ///// Phase 2 generates ClassEqui and ValuaEqui patches 
    ///// with materialized Equipment numbers
    //let runAiwEdcPatcherPhase2 (opts : AiwOptions) 
    //                        (equipmentDownloadPath : string) : Result<unit, string> = 
    //    match readEquiDownload equipmentDownloadPath with
    //    | Error msg -> Error msg
    //    | Ok equiMap -> 
    //        let userEnv : AiwEnv = { UserName = opts.UserName; EquiIndices = Some equiMap }
    //        runCompiler userEnv 
    //            <| compile {
    //                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)             
    //                let! worklist = liftAction <| fun _ -> readWorkList opts.WorkListPath
    //                let! phase2Data = mapM phase2ProcessRow worklist |>> Phase2Data.Concat
    //                do! writePhase2Data opts.OutputDirectory "edc_patch" phase2Data
    //                return ()
    //            }

    