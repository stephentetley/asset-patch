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
        { ProcessRequester: string
          ChangeRequestDescription: string
          WorkListPath: string
          OutputDirectory: string
        }


    /// Note - we need to be able to create floc patches at different
    /// levels in the tree (according to what already exists).
    let private processRow (path : FuncLocPath, row : WorkListRow) : UxlCompilerMonad<MmopCreateData> = 
        match path.Level with
        | 1 -> applyFlocTemplate1 (path, row) makeCAA >>= functionEmitMmopCreate
        | 2 -> applyFlocTemplate1 (path, row) makeNET >>= processGroupEmitMmopCreate
        | 3 -> applyFlocTemplate1 (path, row) makeTEL >>= processEmitMmopCreate
        | 4 -> applyFlocTemplate1 (path, row) makeSYS >>= systemEmitMmopCreate
        | 5 -> applyFlocTemplate1 (path, row) makeTelemetryOustation >>= fun eq1 -> 
               // applyFlocTemplate1 (path, row) makeModem >>= fun eq2 -> 
               equipmentEmitMmopCreate eq1 >>= fun equiPatches1 -> 
               // equipmentEmitMmopCreate eq2 >>= fun equiPatches2 -> 
               // mreturn (MmopCreateData.Concat [equiPatches1; equiPatches2])
               mreturn equiPatches1

        | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (path.ToString()) x)

    let runUxlOutstationPatcher (opts : UxlOptions) : Result<unit, string> = 
        let userEnv = 
            { defaultUxlEnv opts.ChangeRequestDescription with ProcessRequester = opts.ProcessRequester }
        runCompiler userEnv
            <| compile { 
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)
                let! worklist = 
                    liftAction (fun _ -> readWorkList opts.WorkListPath) 
                        |>> List.map (fun row -> (FuncLocPath.Create row.``S4 Root FuncLoc``, row))
                let! mmopCreateData = mapM processRow worklist |>> MmopCreateData.Concat
                do! writeMmopCreateData opts.OutputDirectory "outstation_patch" mmopCreateData
                return ()
            }
