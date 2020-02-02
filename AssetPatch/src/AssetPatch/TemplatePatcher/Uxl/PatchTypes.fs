// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Uxl


module PatchTypes =
    
    open System

    open AssetPatch.Base.FuncLocPath

    type MmopChangeRequest = 
        { Description: string
          ChangeRequestType: string
          FunctionalLocation: FuncLocPath
          EquipmentId: string           // Generally numeric
          ProcessRequestor: string
        }

    type MmopNewFloc = 
        { FunctionalLocation: FuncLocPath
          Description: string 
          FunLocCategory: int
        }

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