// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.AddAttributesPatcher


module AiwAddAttributesPatcher =

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.Base.Template
    open AssetPatch.TemplatePatcher.Base.GenerateMonad
    open AssetPatch.TemplatePatcher.Aiw.Base
    open AssetPatch.TemplatePatcher.Aiw.Emitter
    open AssetPatch.TemplatePatcher.Aiw.Generate

    type AiwOptions = 
        { UserName : string 
          FilePrefix: string
          OutputDirectory : string
        }

    // ************************************************************************
    // Add Floc attributes

    let private genFuncLocAttributes1 (funcLoc: FuncLocPath) 
                                        (classes: FlocClass list): AiwGenerate<FlocAttributes> = 
        generate {
            let! s4classes = mapM (fun x -> applyFlocClass x funcLoc) classes |>> List.concat
            let! attrs = flocClassEmitFlocAttributes s4classes
            return attrs
        }

    let private genFuncLocAttributes (items: FuncLocPath list) 
                                        (classes: FlocClass list): AiwGenerate<FlocAttributes> = 
        generate {
            let! attrs = mapM (fun floc -> genFuncLocAttributes1 floc classes) items
            return FlocAttributes.Concat attrs
        }

    let generateFuncLocAttibutes (opts: AiwOptions) 
                                    (inputList: FuncLocPath list)
                                    (classTemplates: FlocClass list) : Result<unit, string> = 
        let aiwEnv : AiwEnv = 
            { UserName = opts.UserName
              EquiIndices = None
            }
        runGenerate aiwEnv
            <| generate { 
                    let! attrs = genFuncLocAttributes inputList classTemplates
                    do! writeFlocAttributes opts.OutputDirectory opts.FilePrefix attrs
                    return ()
                }

    // ************************************************************************
    // Add Equipment attributes

    let private genEquipmentAttributes1 (equiId: string) 
                                        (classes: EquiClass list): AiwGenerate<EquiAttributes> = 
        generate {
            let! s4classes = mapM (fun x -> applyEquiClass x equiId) classes |>> List.concat
            let! attrs = equiClassEmitEquiAttributes s4classes
            return attrs
        }

    
    let private genEquipmentAttributes (items: EquipmentId list) 
                                        (classes: EquiClass list): AiwGenerate<EquiAttributes> = 
        generate {
            let! attrs = mapM (fun item -> genEquipmentAttributes1 item classes) items
            return EquiAttributes.Concat attrs
        }

    let generateEquipmentAttibutes (opts: AiwOptions) 
                                    (inputList: EquipmentId list)
                                    (classTemplates: EquiClass list) : Result<unit, string> = 
        let aiwEnv : AiwEnv = 
            { UserName = opts.UserName
              EquiIndices = None
            }
        runGenerate aiwEnv
            <| generate { 
                    let! attrs = genEquipmentAttributes inputList classTemplates
                    do! writeEquiAttributes opts.OutputDirectory opts.FilePrefix attrs
                    return ()
                }
