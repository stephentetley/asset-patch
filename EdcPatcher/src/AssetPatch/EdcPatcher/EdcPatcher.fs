// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace AssetPatch.EdcPatcher

[<AutoOpen>]
module EdcPatcher =
    
    open System.IO

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.Base.CompilerMonad
    open AssetPatch.TemplatePatcher.Aiw.Base
    open AssetPatch.TemplatePatcher.Aiw.PatchTypes
    open AssetPatch.TemplatePatcher.Aiw.EmitPhase1
    open AssetPatch.TemplatePatcher.Aiw.EmitPhase2
    open AssetPatch.TemplatePatcher.Aiw.PatchCompiler

    open AssetPatch.EdcPatcher.InputData
    open AssetPatch.EdcPatcher.EdcTemplate


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
    let private phase1ProcessRow (row : WorkListRow) : AiwCompilerMonad<Phase1FlocData * NewEqui list> = 
        let rootPath = FuncLocPath.Create row.``S4 Root FuncLoc``
        match rootPath.Level with
        | 1 -> applyFlocTemplate1 (rootPath, row) makeEDC >>= functionEmitPhase1
        | 2 -> applyFlocTemplate1 (rootPath, row) makeLQD >>= processGroupEmitPhase1
        | 3 -> applyFlocTemplate1 (rootPath, row) makeRGM >>= processEmitPhase1
        | 4 -> applyFlocTemplate1 (rootPath, row) makeSYS >>= systemEmitPhase1
        | x when x > 4 && x < 8 -> 
            applyFlocTemplate1 (rootPath, row) makeLevelTransmitter >>= fun eq1 -> 
            equipmentEmitNewEquis eq1 >>= fun equiPatches ->
            mreturn (Phase1FlocData.Empty (), equiPatches)
        | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (rootPath.ToString()) x)

    let private phase2ProcessRow (row : WorkListRow) : AiwCompilerMonad<Phase2Data> = 
        let rootPath = FuncLocPath.Create row.``S4 Root FuncLoc``
        match rootPath.Level with
        | 1 -> applyFlocTemplate1 (rootPath, row) makeEDC >>= functionEmitPhase2
        | 2 -> applyFlocTemplate1 (rootPath, row) makeLQD >>= processGroupEmitPhase2
        | 3 -> applyFlocTemplate1 (rootPath, row) makeRGM >>= processEmitPhase2
        | 4 -> applyFlocTemplate1 (rootPath, row) makeSYS >>= systemEmitPhase2
        | x when x > 4 && x < 8 -> applyFlocTemplate1 (rootPath, row) makeLevelTransmitter >>= equipmentEmitPhase2
        | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (rootPath.ToString()) x)


    let runEdcPatcherPhase1 (opts : EdcPatcherOptions) : Result<unit, string> = 
        let compilerOpts : CompilerOptions = makeCompilerOptions opts           
        runCompiler compilerOpts None
            <| compile { 
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)             
                let! worklist = liftAction <| fun _ -> readWorkList opts.WorkListPath
                let! (flocData, equis) = unzipMapM phase1ProcessRow worklist 
                do! writePhase1All opts.OutputDirectory "edc_patch" (Phase1FlocData.Concat flocData) (List.concat equis)
                return ()
            }

    /// Phase 2 generates ClassEqui and ValuaEqui patches 
    /// with materialized Equipment numbers
    let runEdcPatcherPhase2 (opts : EdcPatcherOptions) 
                            (equipmentDownloadPath : string) : Result<unit, string> = 
        match readEquiDownload equipmentDownloadPath with
        | Error msg -> Error msg
        | Ok equiMap -> 
            let compilerOpts : CompilerOptions = makeCompilerOptions opts  
            runCompiler compilerOpts (Some equiMap)
                <| compile { 
                    do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)             
                    let! worklist = liftAction <| fun _ -> readWorkList opts.WorkListPath
                    let! phase2Data = mapM phase2ProcessRow worklist |>> Phase2Data.Concat
                    do! writePhase2Data opts.OutputDirectory "edc_patch" phase2Data
                    return ()
                }

    