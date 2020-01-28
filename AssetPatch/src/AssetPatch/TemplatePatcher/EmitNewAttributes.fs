// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher


// Add Classes / Values to existing equipment of funclocs

module EmitNewAttributes =
    
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.PatchTypes
    open AssetPatch.TemplatePatcher.TemplateHierarchy
    open AssetPatch.TemplatePatcher.CompilerMonad    
    open AssetPatch.TemplatePatcher.EmitPhase1
    open AssetPatch.TemplatePatcher.EmitPhase2

    type FlocAttributes = 
        { ClassFlocs : NewClassFloc list
          ValuaFlocs : NewValuaFloc list
        }

        member x.IsEmpty 
            with get () : bool = 
                x.ClassFlocs.IsEmpty && x.ClassFlocs.IsEmpty

        static member Concat (source : FlocAttributes list) : FlocAttributes = 
            let add (r1 : FlocAttributes) (acc : FlocAttributes) = 
                { ClassFlocs = r1.ClassFlocs @ acc.ClassFlocs
                  ValuaFlocs = r1.ValuaFlocs @ acc.ValuaFlocs
                }
            List.foldBack add source { ClassFlocs = []; ValuaFlocs = [] }

    


    type EquiAttributes = 
        { ClassEquis : NewClassEqui list
          ValuaEquis : NewValuaEqui list
        }

        member x.IsEmpty 
            with get () : bool = 
                x.ClassEquis.IsEmpty && x.ValuaEquis.IsEmpty

        static member Concat (source : EquiAttributes list) : EquiAttributes = 
            let add (r1 : EquiAttributes) (acc : EquiAttributes) = 
                { ClassEquis = r1.ClassEquis @ acc.ClassEquis
                  ValuaEquis = r1.ValuaEquis @ acc.ValuaEquis
                }
            List.foldBack add source { ClassEquis = []; ValuaEquis = []}

    // ************************************************************************
    // Translation

    let makeFlocAttributes (flocPath : FuncLocPath)
                            (classes : S4Class list) : FlocAttributes = 
        let make1 (clazz: S4Class) : FlocAttributes = 
            let (x, vals) = makeClassAndValuaFlocPatches flocPath clazz
            in { ClassFlocs = [x]; ValuaFlocs = vals }
        classes |> List.map make1 |> FlocAttributes.Concat


    /// equiId may be a dollar number
    let makeEquiAttributes (equiId : string) 
                            (classes : S4Class list) : EquiAttributes = 
        let collect xs = List.foldBack (fun (c1, vs1)  (cs,vs) -> (c1 ::cs, vs1 @ vs)) xs ([],[])
        let (cs, vs) = List.map (makeClassAndValuaEquiPatches equiId) classes |> collect
        { ClassEquis = cs
          ValuaEquis = vs
        } 


    // ************************************************************************
    // User API

    let generateFlocAttributes (flocPath : FuncLocPath) 
                                    (classes : S4Class list) : CompilerMonad<FlocAttributes> = 
        makeFlocAttributes flocPath classes |> mreturn

    let generateEquiAttributes (equiId : string) 
                                (classes : S4Class list) : CompilerMonad<EquiAttributes> = 
        makeEquiAttributes equiId classes |> mreturn

