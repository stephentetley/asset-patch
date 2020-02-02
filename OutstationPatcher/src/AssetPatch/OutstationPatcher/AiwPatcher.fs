// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace AssetPatch.OutstationPatcher


module AiwPatcher =
    
    open System.IO

    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.Base.CompilerMonad
    open AssetPatch.TemplatePatcher.Aiw.Base
    open AssetPatch.TemplatePatcher.Aiw.PatchTypes
    open AssetPatch.TemplatePatcher.Aiw.EmitPhase1
    open AssetPatch.TemplatePatcher.Aiw.EmitPhase2
    open AssetPatch.TemplatePatcher.Aiw.PatchCompiler

    open AssetPatch.OutstationPatcher.InputData
    open AssetPatch.OutstationPatcher.OutstationTemplate


    type AiwOptions = 
        { UserName : string 
          WorkListPath : string
          OutputDirectory : string
        }

    let private makeCompilerOptions (opts : AiwOptions) : CompilerOptions = 
        { UserName = opts.UserName }


    
    /// Note - we need to be able to create floc patches at different
    /// levels in the tree (according to what already exists).
    let private phase1ProcessRow (path : FuncLocPath, row : WorkListRow) : AiwCompilerMonad<Phase1FlocData * NewEqui list> = 
        match path.Level with
        | 1 -> applyFlocTemplate1 (path, row) makeCAA >>= functionEmitPhase1
        | 2 -> applyFlocTemplate1 (path, row) makeNET >>= processGroupEmitPhase1
        | 3 -> applyFlocTemplate1 (path, row) makeTEL >>= processEmitPhase1
        | 4 -> applyFlocTemplate1 (path, row) makeSYS >>= systemEmitPhase1
        | 5 -> applyFlocTemplate1 (path, row) makeTelemetryOustation >>= fun eq1 -> 
               applyFlocTemplate1 (path, row) makeModem >>= fun eq2 -> 
               equipmentEmitNewEquis eq1 >>= fun equiPatches1 -> 
               equipmentEmitNewEquis eq2 >>= fun equiPatches2 -> 
               mreturn (Phase1FlocData.Empty (), equiPatches1 @ equiPatches2)

        | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (path.ToString()) x)

    let runAiwOutstationPatcherPhase1 (opts : AiwOptions) : Result<unit, string> = 
        let compilerOpts : CompilerOptions = makeCompilerOptions opts           
        runCompiler compilerOpts None
            <| compile { 
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)
                let! worklist = 
                    liftAction (fun _ -> readWorkList opts.WorkListPath) 
                        |>> List.map (fun row -> (FuncLocPath.Create row.``S4 Root FuncLoc``, row))
                let! (flocData, equis) = unzipMapM phase1ProcessRow worklist
                do! writePhase1All opts.OutputDirectory "outstation_patch" (Phase1FlocData.Concat flocData) (List.concat equis)
                return ()
            }

    let private phase2ProcessRow (path : FuncLocPath, row : WorkListRow) : AiwCompilerMonad<Phase2Data> = 
        match path.Level with
        | 1 -> applyFlocTemplate1 (path, row) makeCAA >>= functionEmitPhase2
        | 2 -> applyFlocTemplate1 (path, row) makeNET >>= processGroupEmitPhase2
        | 3 -> applyFlocTemplate1 (path, row) makeTEL >>= processEmitPhase2
        | 4 -> applyFlocTemplate1 (path, row) makeSYS >>= systemEmitPhase2
        | 5 -> 
            applyFlocTemplate1 (path, row) makeTelemetryOustation >>= fun eq1 -> 
            applyFlocTemplate1 (path, row) makeModem >>= fun eq2 ->     
            equipmentEmitPhase2 eq1 >>= fun d1 -> 
            equipmentEmitPhase2 eq2 >>= fun d2 -> 
            mreturn (Phase2Data.Concat [d1; d2])

        | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (path.ToString()) x)


    /// Phase 2 materializes ClassEqui and ValuaEqui patches
    /// Equipment download must have *EQUI, TXTMI & TPLN_EILO
    let runAiwOutstationPatcherPhase2 (opts : AiwOptions) 
                                        (equipmentDownloadPath : string)  : Result<unit, string> = 
        match readEquiDownload equipmentDownloadPath with
        | Error msg -> Error msg
        | Ok equiMap -> 
            let compilerOpts : CompilerOptions = makeCompilerOptions opts  
            runCompiler compilerOpts (Some equiMap)
                <| compile { 
                    do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)
                    let! worklist = 
                        liftAction (fun _ -> readWorkList opts.WorkListPath)
                            |>> List.map (fun row -> (FuncLocPath.Create row.``S4 Root FuncLoc``, row))
                    let! phase2Data = mapM phase2ProcessRow worklist |>> Phase2Data.Concat
                    do! writePhase2Data opts.OutputDirectory "outstation_patch" phase2Data
                    return ()
                }

    