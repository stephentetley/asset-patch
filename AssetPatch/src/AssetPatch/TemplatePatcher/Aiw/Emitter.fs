// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Aiw




module Emitter =
    
    open System.IO
    
    open AssetPatch.Base.Aiw.ChangeFile
    open AssetPatch.TemplatePatcher.Base.Hierarchy
    open AssetPatch.TemplatePatcher.Aiw.FileTypes


    /// Write a file for each level of floc, update and approve in Workbench
    /// This is more robust than linking orphan flocs with a patch which might
    /// not work in practice due to DB validation.
    type FlocCreateData = 
        { NewFuncLocs: NewFuncLoc list
          NewFlocClasses: NewClassFloc list
          NewFlocCharacteristics: NewValuaFloc list
        }

        member x.IsEmpty
            with get () : bool = 
                x.NewFuncLocs.IsEmpty 
                    && x.NewFlocClasses.IsEmpty
                    && x.NewFlocCharacteristics.IsEmpty
                
        member x.RemoveDups(): FlocCreateData = 
            let funcLocDistinctKey (v: NewFuncLoc) : string =
                v.FunctionLocation.ToString()

            let flocClassDistinctKey (v: NewClassFloc) : string =
                v.FuncLoc.ToString() + "!!" + v.Class

            /// Allowed different instances of same field (e.g. AIB_AI2_REFERENCE), but not same instances of same field
            let flocCharDistinctKey (v: NewValuaFloc) : string = 
                v.FuncLoc.ToString() + "!!" + v.CharacteristicID + "!!" + v.Value.DescriptionValue

            { NewFuncLocs = x.NewFuncLocs |> List.distinctBy funcLocDistinctKey
              NewFlocClasses = x.NewFlocClasses |> List.distinctBy flocClassDistinctKey
              NewFlocCharacteristics = x.NewFlocCharacteristics |> List.distinctBy flocCharDistinctKey
            }

        static member Concat (source : FlocCreateData list) : FlocCreateData = 
            { NewFuncLocs               = source |> List.map (fun x -> x.NewFuncLocs) |> List.concat
              NewFlocClasses            = source |> List.map (fun x -> x.NewFlocClasses) |> List.concat
              NewFlocCharacteristics    = source |> List.map (fun x -> x.NewFlocCharacteristics) |> List.concat
            }

    // Note cannot have two equipments with the same name at the same floc
    type EquiCreateData =
        { NewEquipment: NewEqui list }
        
        member x.IsEmpty
            with get () : bool = x.NewEquipment.IsEmpty 

        member x.RemoveDups(): EquiCreateData = 
            let equiDistinctKey (v: NewEqui) : string = v.FuncLoc.ToString() + "!!" + v.Description
            { NewEquipment = x.NewEquipment |> List.distinctBy equiDistinctKey }

        static member Concat (source : EquiCreateData list) : EquiCreateData = 
            { NewEquipment = source |> List.map (fun x -> x.NewEquipment) |> List.concat }

    type EquiCreateClassifactions = 
        { NewEquiClasses: NewClassEqui list
          NewEquiCharacteristics: NewValuaEqui list
        }

        member x.IsEmpty
            with get () : bool = 
                x.NewEquiClasses.IsEmpty
                    && x.NewEquiCharacteristics.IsEmpty

        member x.RemoveDups(): EquiCreateClassifactions = 
            let equiClassDistinctKey (v: NewClassEqui) : string =
                v.EquipmentId + "!!" + v.Class

            /// Allowed different instances of same field (e.g. AIB_AI2_REFERENCE), but not same instances of same field
            let equiCharDistinctKey (v: NewValuaEqui) : string = 
                v.EquipmentId + "!!" + v.CharacteristicID + "!!" + v.Value.DescriptionValue

            { NewEquiClasses = x.NewEquiClasses |> List.distinctBy equiClassDistinctKey
              NewEquiCharacteristics = x.NewEquiCharacteristics |> List.distinctBy equiCharDistinctKey
            }

        static member Concat (source : EquiCreateClassifactions list) : EquiCreateClassifactions = 
            { NewEquiClasses            = source |> List.map (fun x -> x.NewEquiClasses) |> List.concat
              NewEquiCharacteristics    = source |> List.map (fun x -> x.NewEquiCharacteristics) |> List.concat
            }

    // ************************************************************************
    // Translation - equipment
    
    
    /// Count is deduced by caller
    let newValuaEqui (count: int) (source: S4EquiClassification) : NewValuaEqui = 
        { EquipmentId = source.EquiId
          ClassType = IntegerString.OfString "002"
          CharacteristicID = source.CharName
          ValueCount = count
          Value = source.CharValue
        }

    let newClassEqui (source : S4EquiClassification) : NewClassEqui = 
        { EquipmentId = source.EquiId
          Class = source.ClassName
          Status = 1
        }

