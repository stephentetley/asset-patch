// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Uxl


module GenerateAttributes =

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.Base.Template
    open AssetPatch.TemplatePatcher.Base.GenerateMonad
    open AssetPatch.TemplatePatcher.Uxl.Base
    open AssetPatch.TemplatePatcher.Uxl.Emitter
    open AssetPatch.TemplatePatcher.Uxl.Generate

    type UxlOptions = 
        { ProcessRequester: string
          ChangeRequestDescription: string
          FilePrefix: string
          OutputDirectory : string
        }

    // ************************************************************************
    // Func Loc

    let private genFuncLocAttributes1 (funcLoc: FuncLocPath) 
                                        (classes: FlocClass list): UxlGenerate<MmopCreateData> = 
        generate {
            let! s4classes = mapM (fun x -> applyFlocClass x funcLoc) classes |>> List.concat
            let! attrs = flocClassificationsEmitMmopCreate s4classes
            return attrs
        }


    let private genFuncLocAttributes (items: FuncLocPath list) 
                                        (classes: FlocClass list): UxlGenerate<MmopCreateData> = 
        generate {
            let! xss = mapM (fun floc -> genFuncLocAttributes1 floc classes) items
            return MmopCreateData.Concat xss
        }


    let generateFuncLocAttibutes (opts: UxlOptions) 
                                (inputList: FuncLocPath list)
                                (classTemplates: FlocClass list) : Result<unit, string> = 
        let uxlEnv : UxlEnv = 
            { defaultUxlEnv opts.ChangeRequestDescription with ProcessRequester = opts.ProcessRequester }
        runGenerate uxlEnv
            <| generate { 
                    let! mmopCreateData = genFuncLocAttributes inputList classTemplates
                    do! writeMmopCreateData opts.OutputDirectory opts.FilePrefix mmopCreateData
                    return ()
                }

    // ************************************************************************
    // Equipment

    let private genEquiAttributes1 (equiId: EquipmentId) 
                                        (classes: EquiClass list): UxlGenerate<MmopCreateData> = 
        generate {
            let! s4classes = mapM (fun x -> applyEquiClass x equiId) classes |>> List.concat
            let! attrs = equiClassificationsEmitMmopCreate s4classes
            return attrs
        }


    let private genEquiAttributes (equiIds: EquipmentId list) 
                                        (classes: EquiClass list): UxlGenerate<MmopCreateData> = 
        generate {
            let! xss = mapM (fun equiId -> genEquiAttributes1 equiId classes) equiIds
            return MmopCreateData.Concat xss
        }


    let genUxlEquiAttibutes (opts: UxlOptions) 
                                (inputList: EquipmentId list)
                                (classTemplates: EquiClass list) : Result<unit, string> = 
        let uxlEnv : UxlEnv = 
            { defaultUxlEnv opts.ChangeRequestDescription with ProcessRequester = opts.ProcessRequester }
        runGenerate uxlEnv
            <| generate { 
                    let! mmopCreateData = genEquiAttributes inputList classTemplates
                    do! writeMmopCreateData opts.OutputDirectory opts.FilePrefix mmopCreateData
                    return ()
                }
