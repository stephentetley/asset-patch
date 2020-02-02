// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Uxl


module PatchTypes =
    
    open System

    open AssetPatch.Base
    open AssetPatch.Base.FuncLocPath



    type MmopChangeRequest = 
        { Description: string
          ChangeRequestType: string
          FunctionalLocation: FuncLocPath
          EquipmentId: string           // Generally numeric
          ProcessRequestor: string
        }

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

    type MMopFlocClassification = 
        { FunctionalLocation: FuncLocPath
          Class: string
          CharacteristicName: string
          CharacteristicValue: string
        }

    type MmopNewEqui = 
        { EquiId: string
          EquiCategory: string  
          Description: string 
          FunctionalLocation: FuncLocPath
        }

    type MmopNewEquiMultilingualText = 
        { EquiId: string
          Description: string 
          LongText: string
        }

    type MMopEquiClassification = 
        { EquiId: string
          Class: string
          CharacteristicName: string
          CharacteristicValue: string
        }