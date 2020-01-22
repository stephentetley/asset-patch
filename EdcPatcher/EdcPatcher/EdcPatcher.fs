// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace EdcPatcher

module EdcPatcher =
    
    open System.IO

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.CompilerMonad
    open AssetPatch.TemplatePatcher.EmitCommon
    open AssetPatch.TemplatePatcher.Emitter
    open AssetPatch.TemplatePatcher.PatchCompiler

    open EdcPatcher.InputData
    open EdcPatcher.EdcTemplate


    type EdcPatcherOptions = 
        { UserName : string 
          OutputDirectory : string
          WorkListPath : string
          }

    let private makeCompilerOptions (opts : EdcPatcherOptions) : CompilerOptions = 
        { UserName = opts.UserName }

    let internal makeOutputDirectory (dirName : string) : unit = 
        if not <| Directory.Exists(dirName) then
            Directory.CreateDirectory(dirName) |> ignore
        else ()
    
    /// Note - we need to be able to create floc patches at different
    /// levels in the tree (according to what already exists).
    let private phase1ProcessRow (row : WorkListRow) : CompilerMonad<Phase1Data> = 
        let rootPath = FuncLocPath.Create row.``S4 Root FuncLoc``
        match rootPath.Level with
        | 1 -> applyFlocTemplate1 (rootPath, row) makeEDC >>= function1EmitPhase1
        | 2 -> applyFlocTemplate1 (rootPath, row) makeLQD >>= processGroup1EmitPhase1
        | 3 -> applyFlocTemplate1 (rootPath, row) makeRGM >>= process1EmitPhase1
        | 4 -> applyFlocTemplate1 (rootPath, row) makeSYS >>= system1EmitPhase1
        | x when x > 4 && x < 8 -> applyFlocTemplate1 (rootPath, row) makeLevelTransmitter >>= equipment1EmitPhase1
        | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (rootPath.ToString()) x)

    let private phase2ProcessRow (row : WorkListRow) : CompilerMonad<Phase2Data> = 
        let rootPath = FuncLocPath.Create row.``S4 Root FuncLoc``
        match rootPath.Level with
        | 1 -> applyFlocTemplate1 (rootPath, row) makeEDC >>= function1EmitPhase2
        | 2 -> applyFlocTemplate1 (rootPath, row) makeLQD >>= processGroup1EmitPhase2
        | 3 -> applyFlocTemplate1 (rootPath, row) makeRGM >>= process1EmitPhase2
        | 4 -> applyFlocTemplate1 (rootPath, row) makeSYS >>= system1EmitPhase2
        | x when x > 4 && x < 8 -> applyFlocTemplate1 (rootPath, row) makeLevelTransmitter >>= equipment1EmitPhase2
        | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (rootPath.ToString()) x)


    let runEdcPatcherPhase1 (opts : EdcPatcherOptions) : Result<unit, string> = 
        let compilerOpts : CompilerOptions = makeCompilerOptions opts           
        runCompiler compilerOpts None
            <| compile { 
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)             
                let! worklist = liftAction <| fun _ -> readWorkList opts.WorkListPath
                let! phase1Data = mapM phase1ProcessRow worklist |>> Phase1Data.Concat
                do! writePhase1Data opts.OutputDirectory "edc_patch" phase1Data
                return ()
            }

    /// Phase 2 generates ClassEqui and ValuaEqui patches 
    /// with materialized Equipment numbers
    let runEdcPatcherPhase2 (opts : EdcPatcherOptions) 
                            (equipmentDownloadPath : string) : Result<unit, string> = 
        let compilerOpts : CompilerOptions = makeCompilerOptions opts  
        runCompiler compilerOpts (Some equipmentDownloadPath)
            <| compile { 
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)             
                let! worklist = liftAction <| fun _ -> readWorkList opts.WorkListPath
                let! phase2Data = mapM phase2ProcessRow worklist |>> Phase2Data.Concat
                do! writePhase2Data opts.OutputDirectory "edc_patch" phase2Data
                return ()
            }

    