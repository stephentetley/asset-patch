// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace AssetPatch.OutstationPatcher


module AiwPatcher =
    
    open System.IO

    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.Base.GenerateMonad
    open AssetPatch.TemplatePatcher.Aiw.Base
    open AssetPatch.TemplatePatcher.Aiw.Emitter
    open AssetPatch.TemplatePatcher.Aiw.Generate

    open AssetPatch.OutstationPatcher.InputData
    open AssetPatch.OutstationPatcher.OutstationTemplate


    type AiwOptions = 
        { UserName : string 
          WorkListPath : string
          OutputDirectory : string
        }

    
    /// Note - we need to be able to create floc patches at different
    /// levels in the tree (according to what already exists).
    let private phase1ProcessRow (path : FuncLocPath, row : WorkListRow) : AiwGenerate<FlocCreateData> = 
        match path.Level with
        | 1 -> applyFunction (makeCAA row) path     >>= functionalLocationEmitMmopCreate
        | 2 -> applyProcessGroup (makeNET row) path >>= functionalLocationEmitMmopCreate
        | 3 -> applyProcess (makeTEL row) path      >>= functionalLocationEmitMmopCreate
        | 4 -> applySystem (makeSYS row) path       >>= functionalLocationEmitMmopCreate
        | 5 -> mreturn (FlocCreateData.Empty ())

        | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (path.ToString()) x)

    let runAiwOutstationPatcherPhase1 (opts : AiwOptions) : Result<unit, string> = 
        let userEnv : AiwEnv = { UserName = opts.UserName; EquiIndices = None }
        runGenerate userEnv
            <| generate { 
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)
                let! worklist = 
                    liftAction (fun _ -> readWorkList opts.WorkListPath) 
                        |>> List.map (fun row -> (FuncLocPath.Create row.``S4 Root FuncLoc``, row))
                
                let! flocCreateData = mapM phase1ProcessRow worklist |>> FlocCreateData.Concat               
                do! writeFlocCreateData opts.OutputDirectory "outstation_patch" flocCreateData
                return ()
            }

    //let private phase2ProcessRow (path : FuncLocPath, row : WorkListRow) : AiwCompilerMonad<Phase2Data> = 
    //    match path.Level with
    //    | 1 -> applyFlocTemplate1 (path, row) makeCAA >>= functionEmitPhase2
    //    | 2 -> applyFlocTemplate1 (path, row) makeNET >>= processGroupEmitPhase2
    //    | 3 -> applyFlocTemplate1 (path, row) makeTEL >>= processEmitPhase2
    //    | 4 -> applyFlocTemplate1 (path, row) makeSYS >>= systemEmitPhase2
    //    | 5 -> 
    //        applyFlocTemplate1 (path, row) makeTelemetryOustation >>= fun eq1 -> 
    //        applyFlocTemplate1 (path, row) makeModem >>= fun eq2 ->     
    //        equipmentEmitPhase2 eq1 >>= fun d1 -> 
    //        equipmentEmitPhase2 eq2 >>= fun d2 -> 
    //        mreturn (Phase2Data.Concat [d1; d2])

    //    | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (path.ToString()) x)


    ///// Phase 2 materializes ClassEqui and ValuaEqui patches
    ///// Equipment download must have *EQUI, TXTMI & TPLN_EILO
    //let runAiwOutstationPatcherPhase2 (opts : AiwOptions) 
    //                                    (equipmentDownloadPath : string)  : Result<unit, string> = 
    //    match readEquiDownload equipmentDownloadPath with
    //    | Error msg -> Error msg
    //    | Ok equiMap -> 
    //        let userEnv : AiwEnv = { UserName = opts.UserName; EquiIndices = Some equiMap }
    //        runCompiler userEnv 
    //            <| compile { 
    //                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)
    //                let! worklist = 
    //                    liftAction (fun _ -> readWorkList opts.WorkListPath)
    //                        |>> List.map (fun row -> (FuncLocPath.Create row.``S4 Root FuncLoc``, row))
    //                let! phase2Data = mapM phase2ProcessRow worklist |>> Phase2Data.Concat
    //                do! writePhase2Data opts.OutputDirectory "outstation_patch" phase2Data
    //                return ()
    //            }

    