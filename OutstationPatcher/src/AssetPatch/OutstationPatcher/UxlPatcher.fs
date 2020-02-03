// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace AssetPatch.OutstationPatcher

module UxlPatcher =

    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.Base.CompilerMonad
    open AssetPatch.TemplatePatcher.Uxl.Base
    open AssetPatch.TemplatePatcher.Uxl.Emitter
    open AssetPatch.TemplatePatcher.Uxl.PatchCompiler
    open AssetPatch.OutstationPatcher.InputData
    open AssetPatch.OutstationPatcher.OutstationTemplate

    type UxlOptions = 
        { ProcessRequester : string
          WorkListPath : string
          OutputDirectory : string
        }

    let private makeCompilerOptions (opts : UxlOptions) : CompilerOptions = 
        { UserName = "ASSET DATA" }

    /// Note - we need to be able to create floc patches at different
    /// levels in the tree (according to what already exists).
    let private processRow (path : FuncLocPath, row : WorkListRow) : UxlCompilerMonad<MmopCreateData> = 
        match path.Level with
        | 1 -> applyFlocTemplate1 (path, row) makeCAA >>= functionEmitMmopCreate
        //| 2 -> applyFlocTemplate1 (path, row) makeNET >>= processGroupEmitPhase1
        //| 3 -> applyFlocTemplate1 (path, row) makeTEL >>= processEmitPhase1
        //| 4 -> applyFlocTemplate1 (path, row) makeSYS >>= systemEmitPhase1
        //| 5 -> applyFlocTemplate1 (path, row) makeTelemetryOustation >>= fun eq1 -> 
        //       applyFlocTemplate1 (path, row) makeModem >>= fun eq2 -> 
        //       equipmentEmitNewEquis eq1 >>= fun equiPatches1 -> 
        //       equipmentEmitNewEquis eq2 >>= fun equiPatches2 -> 
        //       mreturn (Phase1FlocData.Empty (), equiPatches1 @ equiPatches2)

        | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (path.ToString()) x)

    let runUxlOutstationPatcher (opts : UxlOptions) : Result<unit, string> = 
        let compilerOpts : CompilerOptions = makeCompilerOptions opts           
        runUxlCompiler compilerOpts
            <| compile { 
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)
                let! worklist = 
                    liftAction (fun _ -> readWorkList opts.WorkListPath) 
                        |>> List.map (fun row -> (FuncLocPath.Create row.``S4 Root FuncLoc``, row))
                let mmopCreateData = failwith "TODO" // mapM phase1ProcessRow worklist
                do! writeMmopCreateData opts.OutputDirectory "outstation_patch" mmopCreateData
                return ()
            }
