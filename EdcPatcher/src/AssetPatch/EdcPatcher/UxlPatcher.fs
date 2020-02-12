// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace AssetPatch.EdcPatcher


module UxlPatcher =
    
    open System.IO

    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.Base.CompilerMonad
    open AssetPatch.TemplatePatcher.Uxl.Base
    open AssetPatch.TemplatePatcher.Uxl.Emitter
    open AssetPatch.TemplatePatcher.Uxl.PatchCompiler

    open AssetPatch.EdcPatcher.InputData
    open AssetPatch.EdcPatcher.EdcTemplate


    type UxlOptions = 
        { ProcessRequester: string
          ChangeRequestDescription: string
          WorkListPath: string
          OutputDirectory: string
        }

    
    /// Note - we need to be able to create floc patches at different
    /// levels in the tree (according to what already exists).
    let private processRow (row : WorkListRow) : UxlCompilerMonad<MmopCreateData> = 
        let rootPath = FuncLocPath.Create row.``S4 Root FuncLoc``
        match rootPath.Level with
        | 1 -> applyFlocTemplate1 (rootPath, row) makeEDC >>= functionEmitMmopCreate
        | 2 -> applyFlocTemplate1 (rootPath, row) makeLQD >>= processGroupEmitMmopCreate
        | 3 -> applyFlocTemplate1 (rootPath, row) makeRGM >>= processEmitMmopCreate
        | 4 -> applyFlocTemplate1 (rootPath, row) makeSYS >>= systemEmitMmopCreate
        | x when x > 4 && x < 8 -> 
            applyFlocTemplate1 (rootPath, row) makeLevelTransmitter >>= fun eq1 -> 
            equipmentEmitMmopCreate eq1 >>= fun equiPatches ->
            mreturn equiPatches
        | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (rootPath.ToString()) x)



    let runUxlEdcPatcher (opts : UxlOptions) : Result<unit, string> = 
        let userEnv = 
            { defaultUxlEnv opts.ChangeRequestDescription with ProcessRequester = opts.ProcessRequester }
        runCompiler userEnv
            <| compile { 
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)             
                let! worklist = 
                    liftAction (fun _ -> readWorkList opts.WorkListPath) 
                        
                let! mmopCreateData = mapM processRow worklist |>> MmopCreateData.Concat
                do! writeMmopCreateData opts.OutputDirectory "outstation_patch" mmopCreateData
                return ()
            }


    