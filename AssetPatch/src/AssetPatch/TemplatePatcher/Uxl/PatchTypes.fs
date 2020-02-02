// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Uxl


module PatchTypes =
    
    open System

    open AssetPatch.Base
    open AssetPatch.Base.FuncLocPath


    // This represents one row
    type MmopChangeRequest = 
        { Description: string
          ChangeRequestType: string
          FunctionalLocation: FuncLocPath option
          EquipmentId: string option                  // Generally numeric
          ProcessRequestor: string
        }
        member x.ToAssocs() = 
            [ ("Description (Long)",            x.Description)  
            ; ("Type of Change Request",        x.ChangeRequestType)
            ; ("FL-Functional Location",        
                    Option.defaultValue "" <| Option.map (fun floc -> floc.ToString()) x.FunctionalLocation)
            ; ("EQ-Equipment",                  Option.defaultValue "" x.EquipmentId)
            ; ("Process Requestor",             x.ProcessRequestor)
            ] |> AssocList.ofList


    type MmopNewFuncLoc = 
        { FunctionalLocation: FuncLocPath
          Description: string 
          FunLocCategory: int
        }
        member x.ToAssocs() = 
            [ ("Functional Location",           x.FunctionalLocation.ToString())
            ; ("Masked Func Loc",               x.FunctionalLocation.ToString())
            ; ("Active Inactive Stat",          "")
            ; ("Description (medium)",          x.Description)    
            ] |> AssocList.ofList


    type MmopNewFlocMultilingualText = 
        { FunctionalLocation: FuncLocPath
          Description: string 
          LongText: string
        }
        member x.ToAssocs() = 
            [ ("Functional Location",           x.FunctionalLocation.ToString())
            ; ("Description",                   x.Description)    
            ; ("Long Text",                     x.LongText)
            ] |> AssocList.ofList


    type MMopFlocClassification = 
        { FunctionalLocation: FuncLocPath
          Class: string
          CharacteristicName: string
          CharacteristicValue: string
        }
        member x.ToAssocs() = 
            [ ("Functional Location",           x.FunctionalLocation.ToString())
            ; ("Class Type",                    "003")
            ; ("Class",                         x.Class)
            ; ("Characteristics",               x.CharacteristicName)
            ; ("Char Value",                    x.CharacteristicValue)
            ] |> AssocList.ofList


    type MmopNewEqui = 
        { EquiId: string
          EquiCategory: string  
          Description: string 
          FunctionalLocation: FuncLocPath
        }
        member x.ToAssocs() = 
            [ ("Equiment",                      x.EquiId)
            ; ("EquipCategory",                 x.EquiCategory)
            ; ("Description (medium)",          x.Description)
            ; ("Functional loc.",               x.FunctionalLocation.ToString())
            ] |> AssocList.ofList

    type MmopNewEquiMultilingualText = 
        { EquiId: string
          Description: string 
          LongText: string
        }
        member x.ToAssocs() = 
            [ ("Equipment",                     x.EquiId)
            ; ("Description (medium)",          x.Description)
            ; ("Long Text",                     x.LongText)
            ] |> AssocList.ofList

    type MMopEquiClassification = 
        { EquiId: string
          Class: string
          CharacteristicName: string
          CharacteristicValue: string
        }
        member x.ToAssocs() = 
            [ ("Equipment",                     x.EquiId)
            ; ("Class Type",                    "002")
            ; ("Class",                         x.Class)
            ; ("Characteristics",               x.CharacteristicName)
            ; ("Char Value",                    x.CharacteristicValue)  
            ] |> AssocList.ofList