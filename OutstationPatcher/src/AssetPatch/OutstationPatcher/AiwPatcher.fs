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
    let private flocCreateProcessRow (path : FuncLocPath, row : WorkListRow) : AiwGenerate<FlocCreateData> = 
        match path.Level with
        | 1 -> applyFunction        (makeCAA row) path >>= flocEmitFlocCreateData
        | 2 -> applyProcessGroup    (makeNET row) path >>= flocEmitFlocCreateData
        | 3 -> applyProcess         (makeTEL row) path >>= flocEmitFlocCreateData
        | 4 -> applySystem          (makeSYS row) path >>= flocEmitFlocCreateData
        | 5 -> mreturn (FlocCreateData.Empty ())

        | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (path.ToString()) x)

    let runAiwOutstationPatcherCreateFlocPhase (opts : AiwOptions) : Result<unit, string> = 
        let userEnv : AiwEnv = { UserName = opts.UserName; EquiIndices = None }
        runGenerate userEnv
            <| generate { 
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)
                let! worklist = 
                    liftAction (fun _ -> readWorkList opts.WorkListPath) 
                        |>> List.map (fun row -> (FuncLocPath.Create row.``S4 Root FuncLoc``, row))
                
                let! flocCreateData = mapM flocCreateProcessRow worklist |>> FlocCreateData.Concat               
                do! writeFlocCreateData opts.OutputDirectory "outstation_patch" flocCreateData
                return ()
            }

    // ************************************************************************
    // Equi Create

    let private equiCreateProcessRow (path : FuncLocPath, row : WorkListRow) : AiwGenerate<EquiCreateData> = 
        match path.Level with
        | 1 -> applyFunction        (makeCAA row) path >>= flocEmitEquiCreateData
        | 2 -> applyProcessGroup    (makeNET row) path >>= flocEmitEquiCreateData
        | 3 -> applyProcess         (makeTEL row) path >>= flocEmitEquiCreateData
        | 4 -> applySystem          (makeSYS row) path >>= flocEmitEquiCreateData
        | 5 -> 
            applyEquipment (makeTelemetryOustation row) None path >>= fun eq1 -> 
            applyEquipment (makeModem row)              None path >>= fun eq2 ->     
            equiEmitEquiCreateData eq1 >>= fun d1 -> 
            equiEmitEquiCreateData eq2 >>= fun d2 -> 
            mreturn (EquiCreateData.Concat [d1; d2])

        | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (path.ToString()) x)

    let runAiwOutstationPatcherCreateEquiPhase (opts : AiwOptions) : Result<unit, string> = 
        let userEnv : AiwEnv = { UserName = opts.UserName; EquiIndices = None }
        runGenerate userEnv
            <| generate { 
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)
                let! worklist = 
                    liftAction (fun _ -> readWorkList opts.WorkListPath) 
                        |>> List.map (fun row -> (FuncLocPath.Create row.``S4 Root FuncLoc``, row))
                
                let! equiCreateData = mapM equiCreateProcessRow worklist |>> EquiCreateData.Concat               
                do! writeEquiCreateData opts.OutputDirectory "outstation_patch" equiCreateData
                return ()
            }

    // ************************************************************************
    // Equi Classifications


    let private equiCreateClassificationsProcessRow (row : WorkListRow) : AiwGenerate<EquiCreateClassifications> = 
        let path = FuncLocPath.Create row.``S4 Root FuncLoc``
        match path.Level with
        | 1 -> applyFunction        (makeCAA row) path >>= flocEmitEquiCreateClassifications
        | 2 -> applyProcessGroup    (makeNET row) path >>= flocEmitEquiCreateClassifications
        | 3 -> applyProcess         (makeTEL row) path >>= flocEmitEquiCreateClassifications
        | 4 -> applySystem          (makeSYS row) path >>= flocEmitEquiCreateClassifications
        | x when x > 4 && x < 8 -> 
            applyEquipment (makeTelemetryOustation row) None path >>= fun eq1 -> 
            applyEquipment (makeModem row)              None path >>= fun eq2 ->     
            equiEmitEquiCreateClassifications eq1 >>= fun d1 -> 
            equiEmitEquiCreateClassifications eq2 >>= fun d2 -> 
            mreturn (EquiCreateClassifications.Concat [d1; d2])
        | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (path.ToString()) x)


    /// Phase 2 generates ClassEqui and ValuaEqui patches 
    /// with materialized Equipment numbers
    let runAiwOutstationPatcherAnnotateEquiPhase (opts : AiwOptions) 
                                                (equipmentDownloadPath : string) : Result<unit, string> = 
        match readEquiDownload equipmentDownloadPath with
        | Error msg -> Error msg
        | Ok equiMap -> 
            let userEnv : AiwEnv = { UserName = opts.UserName; EquiIndices = Some equiMap }
            runGenerate userEnv 
                <| generate {
                    do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)             
                    let! worklist = liftAction <| fun _ -> readWorkList opts.WorkListPath
                    let! classData = mapM equiCreateClassificationsProcessRow worklist |>> EquiCreateClassifications.Concat
                    do! writeEquiCreateClassifactions opts.OutputDirectory "edc_patch" classData
                    return ()
                }


    