// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.AddAttributesPatcher

[<AutoOpen>]
module AddAttributesPatcher =

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.Base.CompilerMonad
    open AssetPatch.TemplatePatcher.Base.Template
    open AssetPatch.TemplatePatcher.Aiw.Base
    open AssetPatch.TemplatePatcher.Aiw.EmitNewAttributes
    open AssetPatch.TemplatePatcher.Aiw.PatchCompiler

    type AddAttributesPatcherOptions = 
        { UserName : string 
          FilePrefix: string
          OutputDirectory : string
        }


    let private makeCompilerOptions (opts : AddAttributesPatcherOptions) : CompilerOptions = 
        { UserName = opts.UserName }

    // ************************************************************************
    // Add Floc attributes

    let private genFuncLocAttributes1 (funcLoc: FuncLocPath) 
                                        (classes: Class list): AiwCompilerMonad<FlocAttributes> = 
        compile {
            let! s4classes = mapM (evalTemplate funcLoc) classes
            let! attrs = generateFlocAttributes funcLoc s4classes
            return attrs
        }

    let private genFuncLocAttributes (items: FuncLocPath list) (classes: Class list): AiwCompilerMonad<FlocAttributes> = 
        compile {
            let! attrs = mapM (fun floc -> genFuncLocAttributes1 floc classes) items
            return FlocAttributes.Concat attrs
        }

    let generateFuncLocAttibutes (opts: AddAttributesPatcherOptions) 
                                    (inputList: FuncLocPath list)
                                    (classTemplates: Class list) : Result<unit, string> = 
        runCompiler (makeCompilerOptions opts) None
            <| compile { 
                    let! attrs = genFuncLocAttributes inputList classTemplates
                    do! writeFlocAttributes opts.OutputDirectory opts.FilePrefix attrs
                    return ()
                }

    // ************************************************************************
    // Add Equipment attributes

    let private genEquipmentAttributes1 (equiId: string) 
                                        (classes: Class list): AiwCompilerMonad<EquiAttributes> = 
        compile {
            let! s4classes = mapM evalTemplateNoFloc classes
            let! attrs = generateEquiAttributes equiId s4classes
            return attrs
        }


    type EquipmentId = string
      
    
    let private genEquipmentAttributes (items: EquipmentId list) (classes: Class list): AiwCompilerMonad<EquiAttributes> = 
        compile {
            let! attrs = mapM (fun item -> genEquipmentAttributes1 item classes) items
            return EquiAttributes.Concat attrs
        }

    let generateEquipmentAttibutes (opts: AddAttributesPatcherOptions) 
                                    (inputList: EquipmentId list)
                                    (classTemplates: Class list) : Result<unit, string> = 
        runCompiler (makeCompilerOptions opts) None
            <| compile { 
                    let! attrs = genEquipmentAttributes inputList classTemplates
                    do! writeEquiAttributes opts.OutputDirectory opts.FilePrefix attrs
                    return ()
                }
