// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Uxl


module Emitter =
    
    open AssetPatch.TemplatePatcher.Base.TemplateHierarchy
    open AssetPatch.TemplatePatcher.Base.CompilerMonad
    
    open AssetPatch.TemplatePatcher.Uxl.Base
    open AssetPatch.TemplatePatcher.Uxl.PatchTypes

    type MmopCreateData = 
        { ChangeRequests : MmopChangeRequest list
          NewFuncLocs: MmopNewFuncLoc list
        }

        member x.IsEmpty
            with get () : bool = 
                x.ChangeRequests.IsEmpty && x.NewFuncLocs.IsEmpty
    


    ///// Recursive version of equipmentToNewEqui1
    //let makeNewEqui (source : S4Equipment) : MmopNewEqui list = 
    //        let rec work kids cont = 
    //            match kids with
    //            | [] -> cont []
    //            | (x :: xs) -> 
    //                let v1 = makeNewEqui1 x
    //                work kids (fun vs -> cont(v1 :: vs))
    //        let ans1 = makeNewEqui1 source
    //        work source.SuboridnateEquipment (fun xs -> ans1 :: xs)


    let equipmentEmitMmopCreate (source : S4Equipment) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            return! throwError "TODO"
        }

    let equipmentListEmitMmopCreate (source : S4Equipment list) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            return! throwError "TODO"
        }

    let componentEmitMmopCreate (source : S4Component) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            return! throwError "TODO"
        }

    let componentListEmitMmopCreate (source : S4Component list) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            return! throwError "TODO"
        }

    let itemEmitMmopCreate (source : S4Item) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            return! throwError "TODO"
        }

    let itemListEmitMmopCreate (source : S4Item list) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            return! throwError "TODO"
        }

    let assemblyEmitMmopCreate (source : S4Assembly) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            return! throwError "TODO"
        }

    let assemblyListEmitMmopCreate (source : S4Assembly list) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            return! throwError "TODO"
        }

    let systemEmitMmopCreate (source : S4System) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            return! throwError "TODO"
        }

    let systemListEmitMmopCreate (source : S4System list) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            return! throwError "TODO"
        }

    let processEmitMmopCreate (source : S4Process) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            return! throwError "TODO"
        }

    let processListEmitMmopCreate (source : S4Process list) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            return! throwError "TODO"
        }

    let processGroupEmitMmopCreate (source : S4ProcessGroup) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            return! throwError "TODO"
        }

    let processGroupListEmitMmopCreate (source : S4ProcessGroup list) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            return! throwError "TODO"
        }

    let functionEmitMmopCreate (source : S4Function) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            return! throwError "TODO"
        }

    let functionListEmitMmopCreate (source : S4Function list) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            return! throwError "TODO"
        }

    let siteEmitMmopCreate (source : S4Site) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            return! throwError "TODO"
        }

    let siteListEmitMmopCreate (source : S4Site list) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            return! throwError "TODO"
        }