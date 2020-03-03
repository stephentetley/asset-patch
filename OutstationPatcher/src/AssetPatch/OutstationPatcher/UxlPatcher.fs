// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace AssetPatch.OutstationPatcher

module UxlPatcher =

    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.Base.GenerateMonad
    open AssetPatch.TemplatePatcher.Uxl.Base
    open AssetPatch.TemplatePatcher.Uxl.Emitter
    open AssetPatch.TemplatePatcher.Uxl.Generate
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
    let private processRow (path : FuncLocPath, row : WorkListRow) : UxlGenerate<MmopCreateData> = 
        match path.Level with
        | 1 -> applyFunction        (makeCAA row) path >>= functionalLocationEmitMmopCreate
        | 2 -> applyProcessGroup    (makeNET row) path >>= functionalLocationEmitMmopCreate
        | 3 -> applyProcess         (makeTEL row) path >>= functionalLocationEmitMmopCreate
        | 4 -> applySystem          (makeSYS row) path >>= functionalLocationEmitMmopCreate
        | 5 -> applyEquipment       (makeTelemetryOustation row) None path >>= equipmentEmitMmopCreate
        | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (path.ToString()) x)

    let runUxlOutstationPatcher (opts : UxlOptions) : Result<unit, string> = 
        let userEnv = 
            { defaultUxlEnv opts.ChangeRequestDescription with ProcessRequester = opts.ProcessRequester }
        runGenerate userEnv
            <| generate { 
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)
                let! worklist = 
                    liftAction (fun _ -> readWorkList opts.WorkListPath) 
                        |>> List.map (fun row -> (FuncLocPath.Create row.``S4 Root FuncLoc``, row))
                let! mmopCreateData = mapM processRow worklist |>> MmopCreateData.Concat
                do! writeMmopCreateData opts.OutputDirectory "outstation_patch" mmopCreateData
                return ()
            }
