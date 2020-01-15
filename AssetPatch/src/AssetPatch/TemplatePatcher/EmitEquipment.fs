// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher


module EmitEquipment =

    open System.IO

    open AssetPatch.Base
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.CompilerMonad
    open AssetPatch.TemplatePatcher.PatchTypes
    open AssetPatch.TemplatePatcher.TemplateHierarchy
    open AssetPatch.TemplatePatcher.PatchWriter
    open AssetPatch.TemplatePatcher.EmitCommon
    

    let private characteristicToNewValuaEqui1 (equiId : string) 
                                                (count : int) 
                                                (charac : S4Characteristic) : CompilerMonad<NewValuaEqui> = 
        mreturn { 
            EquipmentId = equiId
            ClassType = IntegerString.OfString "002"
            CharacteristicID = charac.Name
            ValueCount = count
            Value = charac.Value
        }
    
    
    let private classToNewClassEqui (equiId : string)
                                    (s4Class : S4Class) : CompilerMonad<NewClassEqui> = 
        mreturn { 
            EquipmentId = equiId
            Class = s4Class.ClassName
            Status = 1
        }



    let private characteristicsToNewValuaEquis (equiId : string)
                                               (characteristics : S4Characteristic list) : CompilerMonad<NewValuaEqui list> =  

        let makeGrouped (chars : S4Characteristic list) : CompilerMonad<NewValuaEqui list> = 
            foriM chars (fun i x -> characteristicToNewValuaEqui1 equiId (i+1) x)

        compile {
            let chars = sortedCharacteristics characteristics
            return! mapM makeGrouped chars |>> List.concat
        }

    /// equiId may be a dollar number
    let private classToProperties (equiId : string)
                                    (clazz : S4Class) : CompilerMonad<NewClassEqui * NewValuaEqui list> = 
           compile {
               let! ce = classToNewClassEqui equiId clazz
               let! vs = characteristicsToNewValuaEquis equiId clazz.Characteristics
               return (ce, vs)
           }

    
    let private equipmentToNewEqui1 (equipment : S4Equipment) : CompilerMonad<NewEqui> = 
        let commonProps : CommonProperties = 
            { CompanyCode = equipment.FlocProperties.CompanyCode 
              ControllingArea = equipment.FlocProperties.ControllingArea 
              PlantCode = equipment.FlocProperties.MaintenancePlant
              UserStatus = equipment.FlocProperties.ObjectStatus
            }
        let startupDate = equipment.FlocProperties.StartupDate
        compile {
            return { 
                Description = equipment.Description
                FuncLoc = equipment.FuncLoc
                Category = equipment.Category
                ObjectType = equipment.ObjectType
                Manufacturer = 
                    Option.defaultValue "TO BE DETERMINED" equipment.Manufacturer
                Model = Option.defaultValue "TO BE DETERMINED" equipment.Model
                SerialNumber = Option.defaultValue "" equipment.SerialNumber
                StartupDate = equipment.FlocProperties.StartupDate
                ConstructionYear = 
                     Option.defaultValue (uint16 startupDate.Year) equipment.ConstructionYear
                ConstructionMonth = 
                    Option.defaultValue (uint8 startupDate.Month) equipment.ConstructionMonth
                MaintenancePlant = equipment.FlocProperties.MaintenancePlant
                Currency = equipment.FlocProperties.Currency
                CommonProps = commonProps
            }
        }
        
    /// Recursive version of equipmentToNewEqui1
    let equipmentToNewEqui (source : S4Equipment) : CompilerMonad<NewEqui list> = 
            let rec work kids cont = 
                match kids with
                | [] -> mreturn []
                | (x :: xs) -> 
                    compile {
                        let! v1 = equipmentToNewEqui1 x
                        return! work kids (fun vs -> let ans = (v1 :: vs) in cont ans)
                    }
            compile {
                let! equiResult1 = equipmentToNewEqui1 source
                let! kids = work source.SuboridnateEquipment id
                return (equiResult1 :: kids)
            }

    let equipmentsToPhase1EquiData (source : S4Equipment list) : CompilerMonad<Phase1EquiData> = 
        compile { 
            let! xss = mapM equipmentToNewEqui source 
            return { Equis = List.concat xss }
        }

    /// equiId may be a dollar number
    let equipmentToPhase2EquiData1 (equiId : string) 
                                     (classes : S4Class list) : CompilerMonad<Phase2EquiData> = 
        let collect xs = List.foldBack (fun (c1, vs1)  (cs,vs) -> (c1 ::cs, vs1 @ vs)) xs ([],[])
        compile { 
            let! (cs, vs) = mapM (classToProperties equiId) classes |>> collect
            return { 
                ClassEquis = cs
                ValuaEquis = vs
            }
        } 

    /// Recursive version of equipmentToNewEqui1
    let equipmentToPhase2EquiData (source : S4Equipment) : CompilerMonad<Phase2EquiData> = 
            let rec work (kids : S4Equipment list) cont = 
                match kids with
                | [] -> mreturn []
                | (x :: xs) -> 
                    compile {
                        match x.EquipmentId with 
                        | None -> return! throwError (sprintf "Missing equipment for %s '%s'" (source.FuncLoc.ToString()) source.Description)
                        | Some equiNum -> 
                            let! v1 = equipmentToPhase2EquiData1 equiNum x.Classes
                            return! work kids (fun vs -> let ans = (v1 :: vs) in cont ans)
                    }

            compile {
                match source.EquipmentId with
                | None -> return! throwError (sprintf "Missing equipment for %s '%s'" (source.FuncLoc.ToString()) source.Description)
                | Some equiNum -> 
                    let! equiResult1 = equipmentToPhase2EquiData1 equiNum source.Classes
                    let! kids = work source.SuboridnateEquipment id
                    return (equiResult1 :: kids |> Phase2EquiData.Concat)
            }
    

    /// This recurses to sub-equipment
    let equipmentsToPhase2EquiData (source : S4Equipment list) : CompilerMonad<Phase2EquiData> = 
        compile { 
            let! xss = mapM equipmentToPhase2EquiData source 
            return (Phase2EquiData.Concat xss)
        }


    

    