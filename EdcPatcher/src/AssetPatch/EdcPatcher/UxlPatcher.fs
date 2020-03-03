// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace AssetPatch.EdcPatcher


module UxlPatcher =
    
    open System.IO

    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.Base.GenerateMonad
    open AssetPatch.TemplatePatcher.Uxl.Base
    open AssetPatch.TemplatePatcher.Uxl.Emitter
    open AssetPatch.TemplatePatcher.Uxl.Generate

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
    let private processRow (row : WorkListRow) : UxlGenerate<MmopCreateData> = 
        let rootPath = FuncLocPath.Create row.``S4 Root FuncLoc``
        match rootPath.Level with
        | 1 -> applyFunction        (makeEDC row) rootPath  >>= functionalLocationEmitMmopCreate
        | 2 -> applyProcessGroup    (makeLQD row) rootPath  >>= functionalLocationEmitMmopCreate
        | 3 -> applyProcess         (makeRGM row) rootPath  >>= functionalLocationEmitMmopCreate
        | 4 -> applySystem          (makeSYS row) rootPath  >>= functionalLocationEmitMmopCreate
        | x when x > 4 && x < 8 -> 
            applyEquipment (makeLevelTransmitter row) None rootPath >>= equipmentEmitMmopCreate
        | x -> throwError (sprintf "Cannot process floc %s, level %i not valid" (rootPath.ToString()) x)



    let runUxlEdcPatcher (opts : UxlOptions) : Result<unit, string> = 
        let userEnv = 
            { defaultUxlEnv opts.ChangeRequestDescription with ProcessRequester = opts.ProcessRequester }
        runGenerate userEnv
            <| generate { 
                do! liftAction (fun () -> makeOutputDirectory opts.OutputDirectory)             
                let! worklist = 
                    liftAction (fun _ -> readWorkList opts.WorkListPath) 
                        
                let! mmopCreateData = mapM processRow worklist |>> MmopCreateData.Concat
                do! writeMmopCreateData opts.OutputDirectory "outstation_patch" mmopCreateData
                return ()
            }


    