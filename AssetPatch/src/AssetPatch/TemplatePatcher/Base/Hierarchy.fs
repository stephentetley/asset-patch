// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Base



module Hierarchy =
    
    open System

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.Base.ValuaValue 

    type FuncLocProperties = 
        { StartupDate : DateTime 
          ObjectStatus : string         
          StructureIndicator : string
          MaintenancePlant : uint32
          ControllingArea : uint32
          CompanyCode : uint32
          Currency : string
        }

    type S4FlocClassification = 
        { FuncLoc: FuncLocPath
          ClassName: string
          CharName: string
          CharValue: ValuaValue
        }

    ///
    type S4EquiClassification = 
        { EquiId: string
          ClassName: string
          CharName: string
          CharValue: ValuaValue
        }


    type S4Equipment = 
        { EquiId: string
          SuperEquiId: string option
          FuncLoc : FuncLocPath
          FlocProperties : FuncLocProperties
          Description : string          
          MultilingualText : string
          Category : string
          ObjectType : string
          Class: string
          StartupDate: DateTime
          Manufacturer : string option
          Model : string option
          SerialNumber : string option
          ConstructionYear : uint16 option
          ConstructionMonth : uint8 option
          Classifications : S4EquiClassification list
          SuboridinateEquipment : S4Equipment list
          MemoLine : string
        }


    type S4FunctionalLocation = 
        { FuncLoc : FuncLocPath
          FlocProperties : FuncLocProperties
          Description : string
          ObjectType : string
          Classifications : S4FlocClassification list
          SubFlocs: S4FunctionalLocation list
          Equipment : S4Equipment list        
        }

    