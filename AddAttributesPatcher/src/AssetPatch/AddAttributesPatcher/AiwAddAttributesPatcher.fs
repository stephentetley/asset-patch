// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.AddAttributesPatcher


module AiwAddAttributesPatcher =

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.Base.CompilerMonad
    open AssetPatch.TemplatePatcher.Base.Template
    open AssetPatch.TemplatePatcher.Aiw.Base
    open AssetPatch.TemplatePatcher.Aiw.EmitNewAttributes
    open AssetPatch.TemplatePatcher.Aiw.PatchCompiler

    type AiwOptions = 
        { UserName : string 
          FilePrefix: string
          OutputDirectory : string
        }

    // ************************************************************************
    // Add Floc attributes

    let private genFuncLocAttributes1 (funcLoc: FuncLocPath) 
                                        (classes: Classification list): AiwCompilerMonad<FlocAttributes> = 
        compile {
            let! s4classes = mapM (evalTemplate funcLoc) classes
            let! attrs = generateFlocAttributes funcLoc s4classes
            return attrs
        }

    let private genFuncLocAttributes (items: FuncLocPath list) 
                                        (classes: Classification list): AiwCompilerMonad<FlocAttributes> = 
        compile {
            let! attrs = mapM (fun floc -> genFuncLocAttributes1 floc classes) items
            return FlocAttributes.Concat attrs
        }

    let generateFuncLocAttibutes (opts: AiwOptions) 
                                    (inputList: FuncLocPath list)
                                    (classTemplates: Classification list) : Result<unit, string> = 
        let aiwEnv : AiwEnv = 
            { UserName = opts.UserName
              EquiIndices = None
            }
        runCompiler aiwEnv
            <| compile { 
                    let! attrs = genFuncLocAttributes inputList classTemplates
                    do! writeFlocAttributes opts.OutputDirectory opts.FilePrefix attrs
                    return ()
                }

    // ************************************************************************
    // Add Equipment attributes

    let private genEquipmentAttributes1 (equiId: string) 
                                        (classes: Classification list): AiwCompilerMonad<EquiAttributes> = 
        compile {
            let! s4classes = mapM evalTemplateNoFloc classes
            let! attrs = generateEquiAttributes equiId s4classes
            return attrs
        }


    type EquipmentId = string
      
    
    let private genEquipmentAttributes (items: EquipmentId list) 
                                        (classes: Classification list): AiwCompilerMonad<EquiAttributes> = 
        compile {
            let! attrs = mapM (fun item -> genEquipmentAttributes1 item classes) items
            return EquiAttributes.Concat attrs
        }

    let generateEquipmentAttibutes (opts: AiwOptions) 
                                    (inputList: EquipmentId list)
                                    (classTemplates: Classification list) : Result<unit, string> = 
        let aiwEnv : AiwEnv = 
            { UserName = opts.UserName
              EquiIndices = None
            }
        runCompiler aiwEnv
            <| compile { 
                    let! attrs = genEquipmentAttributes inputList classTemplates
                    do! writeEquiAttributes opts.OutputDirectory opts.FilePrefix attrs
                    return ()
                }
