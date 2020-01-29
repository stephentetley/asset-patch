// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.AddAttributesPatcher

[<AutoOpen>]
module AddAttributesPatcher =

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.CompilerMonad
    open AssetPatch.TemplatePatcher.Template
    open AssetPatch.TemplatePatcher.EmitNewAttributes
    open AssetPatch.TemplatePatcher.PatchCompiler

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
                                        (classes: Class list): CompilerMonad<FlocAttributes> = 
        compile {
            let! s4classes = mapM (evalTemplate funcLoc) classes
            let! attrs = generateFlocAttributes funcLoc s4classes
            return attrs
        }

    let private genFuncLocAttributes (items: FuncLocPath list) (classes: Class list): CompilerMonad<FlocAttributes> = 
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

    let private genEquipmentAttributes1 (funcLoc: FuncLocPath) 
                                        (equiId: string) 
                                        (classes: Class list): CompilerMonad<EquiAttributes> = 
        compile {
            let! s4classes = mapM (evalTemplate funcLoc) classes
            let! attrs = generateEquiAttributes equiId s4classes
            return attrs
        }


    type EquiRow = 
      { FunctionalLocation: FuncLocPath
        EquipmentId: string
      }

    let makeEquiRow (funcloc: string) (equiId: string): EquiRow = 
        { FunctionalLocation = FuncLocPath.Create funcloc
          EquipmentId = equiId
        }
    
    let private genEquipmentAttributes (items: EquiRow list) (classes: Class list): CompilerMonad<EquiAttributes> = 
        compile {
            let! attrs = mapM (fun item -> genEquipmentAttributes1 item.FunctionalLocation item.EquipmentId classes) items
            return EquiAttributes.Concat attrs
        }

    let generateEquipmentAttibutes (opts: AddAttributesPatcherOptions) 
                                    (inputList: EquiRow list)
                                    (classTemplates: Class list) : Result<unit, string> = 
        runCompiler (makeCompilerOptions opts) None
            <| compile { 
                    let! attrs = genEquipmentAttributes inputList classTemplates
                    do! writeEquiAttributes opts.OutputDirectory opts.FilePrefix attrs
                    return ()
                }
