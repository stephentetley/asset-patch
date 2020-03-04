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

    // ************************************************************************
    // Floc Create

    /// Note - we need to be able to create floc patches at different
    /// levels in the tree (according to what already exists).
    let private flocCreateProcessRow (row : WorkListRow) : AiwGenerate<FlocCreateData> = 
        let rootPath = FuncLocPath.Create row.``S4 Root FuncLoc``
        match rootPath.Level with
        | 1 -> applyFunction        (makeEDC row) rootPath >>= flocEmitFlocCreateData
        | 2 -> applyProcessGroup    (makeLQD row) rootPath >>= flocEmitFlocCreateData
        | 3 -> applyProcess         (makeRGM row) rootPath >>= flocEmitFlocCreateData
        | 4 -> applySystem          (makeSYS row) rootPath >>= flocEmitFlocCreateData
        | x when x > 4 && x < 8 -> mreturn (FlocCreateData.Empty ())
        | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (rootPath.ToString()) x)



    let runAiwEdcPatcherCreateFlocPhase (opts : AiwOptions) : Result<unit, string> = 
        let userEnv : AiwEnv = { UserName = opts.UserName; EquiIndices = None }
        runGenerate userEnv
            <| generate { 
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)             
                let! worklist = liftAction <| fun _ -> readWorkList opts.WorkListPath                
                let! flocCreateData = mapM flocCreateProcessRow worklist |>> FlocCreateData.Concat               
                do! writeFlocCreateData opts.OutputDirectory "edc_patch" flocCreateData                
                return ()
            }

    // ************************************************************************
    // Equi Create

    /// Note - we need to be able to create floc patches at different
    /// levels in the tree (according to what already exists).
    let private equiCreateProcessRow (row : WorkListRow) : AiwGenerate<EquiCreateData> = 
        let rootPath = FuncLocPath.Create row.``S4 Root FuncLoc``
        match rootPath.Level with
        | 1 -> applyFunction        (makeEDC row) rootPath >>= flocEmitEquiCreateData
        | 2 -> applyProcessGroup    (makeLQD row) rootPath >>= flocEmitEquiCreateData
        | 3 -> applyProcess         (makeRGM row) rootPath >>= flocEmitEquiCreateData
        | 4 -> applySystem          (makeSYS row) rootPath >>= flocEmitEquiCreateData
        | x when x > 4 && x < 8 -> 
            applyEquipment (makeLevelTransmitter row) None rootPath >>= equiEmitEquiCreateData
        | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (rootPath.ToString()) x)



    let runAiwEdcPatcherCreateEquiPhase (opts : AiwOptions) : Result<unit, string> = 
        let userEnv : AiwEnv = { UserName = opts.UserName; EquiIndices = None }
        runGenerate userEnv
            <| generate { 
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)             
                let! worklist = liftAction <| fun _ -> readWorkList opts.WorkListPath                
                let! equiCreateData = mapM equiCreateProcessRow worklist |>> EquiCreateData.Concat               
                do! writeEquiCreateData opts.OutputDirectory "edc_patch" equiCreateData                
                return ()
            }


    // ************************************************************************
    // Equi Classifications


    let private equiEquiAttributesProcessRow (row : WorkListRow) : AiwGenerate<EquiAttributes> = 
        let rootPath = FuncLocPath.Create row.``S4 Root FuncLoc``
        match rootPath.Level with
        | 1 -> applyFunction        (makeEDC row) rootPath >>= flocEmitEquiAttributes
        | 2 -> applyProcessGroup    (makeLQD row) rootPath >>= flocEmitEquiAttributes
        | 3 -> applyProcess         (makeRGM row) rootPath >>= flocEmitEquiAttributes
        | 4 -> applySystem          (makeSYS row) rootPath >>= flocEmitEquiAttributes
        | x when x > 4 && x < 8 -> 
            applyEquipment (makeLevelTransmitter row) None rootPath >>= equiEmitEquiAttributes
        | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (rootPath.ToString()) x)


    /// Phase 2 generates ClassEqui and ValuaEqui patches 
    /// with materialized Equipment numbers
    let runAiwEdcPatcherAnnotateEquiPhase (opts : AiwOptions) 
                                            (equipmentDownloadPath : string) : Result<unit, string> = 
        match readEquiDownload equipmentDownloadPath with
        | Error msg -> Error msg
        | Ok equiMap -> 
            let userEnv : AiwEnv = { UserName = opts.UserName; EquiIndices = Some equiMap }
            runGenerate userEnv 
                <| generate {
                    do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)             
                    let! worklist = liftAction <| fun _ -> readWorkList opts.WorkListPath
                    let! classData = mapM equiEquiAttributesProcessRow worklist |>> EquiAttributes.Concat
                    do! writeEquiAttributes opts.OutputDirectory "edc_patch" classData
                    return ()
                }

    