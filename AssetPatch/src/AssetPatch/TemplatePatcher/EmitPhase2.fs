// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher



module EmitPhase2 =
    
    open AssetPatch.Base.ChangeFile
    open AssetPatch.TemplatePatcher.PatchTypes
    open AssetPatch.TemplatePatcher.TemplateHierarchy
    open AssetPatch.TemplatePatcher.CompilerMonad

    type Phase2Data = 
        { ClassEquis : NewClassEqui list
          ValuaEquis : NewValuaEqui list
          Eqmltxts : NewEqmltxt list
        }

        member x.IsEmpty 
            with get () : bool = 
                x.ClassEquis.IsEmpty && x.ValuaEquis.IsEmpty && x.Eqmltxts.IsEmpty

        static member Concat (source : Phase2Data list) : Phase2Data = 
            let add (r1 : Phase2Data) (acc : Phase2Data) = 
                { ClassEquis = r1.ClassEquis @ acc.ClassEquis
                  ValuaEquis = r1.ValuaEquis @ acc.ValuaEquis
                  Eqmltxts   = r1.Eqmltxts @ acc.Eqmltxts
                }
            List.foldBack add source { ClassEquis = []; ValuaEquis = []; Eqmltxts = []}

    // ************************************************************************
    // Translation

    let private makeNewValuaEqui (equiId : string) 
                                (count : int) 
                                (charac : S4Characteristic) : NewValuaEqui = 
        { EquipmentId = equiId
          ClassType = IntegerString.OfString "002"
          CharacteristicID = charac.Name
          ValueCount = count
          Value = charac.Value
        }
    
    
    let private makeNewClassEqui (equiId : string)
                                    (s4Class : S4Class) : NewClassEqui = 
        { EquipmentId = equiId
          Class = s4Class.ClassName
          Status = 1
        }

    let private makeNewValuaEquis (equiId : string)
                        (characteristics : S4Characteristic list) : NewValuaEqui list =  

        let makeGrouped (chars : S4Characteristic list) : NewValuaEqui list = 
            chars |> List.mapi (fun i x -> makeNewValuaEqui equiId (i+1) x)

        let chars1 = sortedCharacteristics characteristics
        List.map makeGrouped chars1 |> List.concat
        


    /// equiId may be a dollar number
    let private makeClassAndValuaFlocPatches (equiId : string)
                                    (clazz : S4Class) : NewClassEqui * NewValuaEqui list = 
        let ce = makeNewClassEqui equiId clazz
        let vs = makeNewValuaEquis equiId clazz.Characteristics
        (ce, vs)


    /// equiId may be a dollar number
    let makePhase2EquiData1 (equiId : string) 
                                     (equiDescription: string)
                                     (memoLine: string)
                                     (classes : S4Class list) : Phase2Data = 
        let collect xs = List.foldBack (fun (c1, vs1)  (cs,vs) -> (c1 ::cs, vs1 @ vs)) xs ([],[])
        let (cs, vs) = List.map (makeClassAndValuaFlocPatches equiId) classes |> collect
        let eqmltxt = 
                { EquipmentId = equiId
                  Description = equiDescription
                  LongText = memoLine
                  MoreTextExists = false }
        { ClassEquis = cs
          ValuaEquis = vs
          Eqmltxts = [eqmltxt]
        } 

    /// Recursive version of equipmentToNewEqui1
    let equipmentToPhase2EquiData (source : S4Equipment) : CompilerMonad<Phase2Data> = 
            let rec work (kids : S4Equipment list) cont = 
                match kids with
                | [] -> cont []
                | (x :: xs) -> 
                    compile {
                        let! equiNum = getEquiNumber x.Description x.FuncLoc
                        let v1 = makePhase2EquiData1 equiNum x.Description x.MemoLine x.Classes
                        return! work kids (fun vs -> let ans = (v1 :: vs) in cont ans)
                    }

            compile {
                let! equiNum = getEquiNumber source.Description source.FuncLoc
                let equiResult1 = makePhase2EquiData1 equiNum source.Description source.MemoLine source.Classes
                let! kids = work source.SuboridnateEquipment (fun x -> mreturn x)
                return (equiResult1 :: kids |> Phase2Data.Concat)
            }
    

    /// This recurses to sub-equipment
    let equipmentsToPhase2EquiData (source : S4Equipment list) : CompilerMonad<Phase2Data> = 
        compile { 
            let! xss = mapM equipmentToPhase2EquiData source 
            return (Phase2Data.Concat xss)
        }


    // ************************************************************************
    // User API

    let equipmentEmitPhase2 (source : S4Equipment) : CompilerMonad<Phase2Data> = 
        equipmentsToPhase2EquiData [source]

    let equipmentListEmitPhase2 (source : S4Equipment list) : CompilerMonad<Phase2Data> = 
        equipmentsToPhase2EquiData source

    let componentEmitPhase2 (source : S4Component) : CompilerMonad<Phase2Data> = 
        equipmentsToPhase2EquiData source.Equipment

    let componentListEmitPhase2 (source : S4Component list) : CompilerMonad<Phase2Data> = 
        mapM componentEmitPhase2 source |>> Phase2Data.Concat

    let itemEmitPhase2 (source : S4Item) : CompilerMonad<Phase2Data> = 
        compile { 
            let! x1 = equipmentsToPhase2EquiData source.Equipment
            let! x2 = componentListEmitPhase2 source.Components
            return Phase2Data.Concat [x1; x2]
        }


    let itemListEmitPhase2 (source : S4Item list) : CompilerMonad<Phase2Data> = 
        mapM itemEmitPhase2 source |>> Phase2Data.Concat

    let assemblyEmitPhase2 (source : S4Assembly) : CompilerMonad<Phase2Data> = 
        compile { 
            let! x1 = equipmentsToPhase2EquiData source.Equipment
            let! x2 = itemListEmitPhase2 source.Items
            return Phase2Data.Concat [x1; x2]
        }


    let assemblyListEmitPhase2 (source : S4Assembly list) : CompilerMonad<Phase2Data> = 
        mapM assemblyEmitPhase2 source |>> Phase2Data.Concat
            

    let systemEmitPhase2 (source : S4System) : CompilerMonad<Phase2Data> = 
        compile { 
            let! x1 = equipmentsToPhase2EquiData source.Equipment
            let! x2 = assemblyListEmitPhase2 source.Assemblies
            return Phase2Data.Concat [x1; x2]
        }

    let systemListEmitPhase2 (source : S4System list) : CompilerMonad<Phase2Data> = 
        mapM systemEmitPhase2 source |>> Phase2Data.Concat
            
    let processEmitPhase2 (source : S4Process) : CompilerMonad<Phase2Data> = 
        systemListEmitPhase2 source.Systems

    let processListEmitPhase2 (source : S4Process list) : CompilerMonad<Phase2Data> = 
        mapM processEmitPhase2 source |>> Phase2Data.Concat 
        
    let processGroupEmitPhase2 (source : S4ProcessGroup) : CompilerMonad<Phase2Data> = 
        processListEmitPhase2 source.Processes

    let processGroupListEmitPhase2 (source : S4ProcessGroup list) : CompilerMonad<Phase2Data> = 
        mapM processGroupEmitPhase2 source |>> Phase2Data.Concat

    let functionEmitPhase2 (source : S4Function) : CompilerMonad<Phase2Data> = 
        processGroupListEmitPhase2 source.ProcessGroups

    let functionListEmitPhase2 (source : S4Function list) : CompilerMonad<Phase2Data> = 
        mapM functionEmitPhase2 source |>> Phase2Data.Concat

    let siteEmitPhase2 (source : S4Site) : CompilerMonad<Phase2Data> = 
        functionListEmitPhase2 source.Functions

    let siteListEmitPhase2 (source : S4Site list) : CompilerMonad<Phase2Data> = 
        mapM siteEmitPhase2 source |>> Phase2Data.Concat

