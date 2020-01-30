// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace AssetPatch.OutstationPatcher

[<AutoOpen>]
module OutstationPatcher =
    
    open System.IO

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.PatchTypes
    open AssetPatch.TemplatePatcher.CompilerMonad
    open AssetPatch.TemplatePatcher.EmitPhase1
    open AssetPatch.TemplatePatcher.EmitPhase2
    open AssetPatch.TemplatePatcher.PatchCompiler

    open AssetPatch.OutstationPatcher.InputData
    open AssetPatch.OutstationPatcher.OutstationTemplate


    type OsPatcherOptions = 
        { UserName : string 
          OutputDirectory : string
          WorkListPath : string
        }

    let private makeCompilerOptions (opts : OsPatcherOptions) : CompilerOptions = 
        { UserName = opts.UserName }

    let internal makeOutputDirectory (dirName : string) : unit = 
        if not <| Directory.Exists(dirName) then
            Directory.CreateDirectory(dirName) |> ignore
        else ()
    
    /// Note - we need to be able to create floc patches at different
    /// levels in the tree (according to what already exists).
    let private phase1ProcessRow (path : FuncLocPath, row : WorkListRow) : CompilerMonad<Phase1FlocData * NewEqui list> = 
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

    let runOutstationPatcherPhase1 (opts : OsPatcherOptions) : Result<unit, string> = 
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

    let private phase2ProcessRow (path : FuncLocPath, row : WorkListRow) : CompilerMonad<Phase2Data> = 
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
    let runOutstationPatcherPhase2 (opts : OsPatcherOptions) 
                            (equipmentDownloadPath : string)  : Result<unit, string> = 
        let compilerOpts : CompilerOptions = makeCompilerOptions opts  
        runCompiler compilerOpts (Some equipmentDownloadPath)
            <| compile { 
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)
                let! worklist = 
                    liftAction (fun _ -> readWorkList opts.WorkListPath)
                        |>> List.map (fun row -> (FuncLocPath.Create row.``S4 Root FuncLoc``, row))
                let! phase2Data = mapM phase2ProcessRow worklist |>> Phase2Data.Concat
                do! writePhase2Data opts.OutputDirectory "outstation_patch" phase2Data
                return ()
            }

    