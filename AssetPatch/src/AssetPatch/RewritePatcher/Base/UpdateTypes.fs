// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.RewritePatcher.Base

module UpdateTypes = 

    open System
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.Base.ValuaValue

    type EquiProperty =
        | EquipCategory
        | DescriptionMedium
        | ObjectType
        | StartupDate
        | Manufacturer
        | ModelNumber
        | ManufPartNum
        | ManufSerialNum
        | ConstructionYear 
        | ConstructionMonth
        | CompanyCode
        | FunctionalLocation
        | SuperordEquip
        | StatusOfAnObject
        | StatusWithoutStatusNum

    type FlocProperty = 
        | DescriptionMedium
        | FunLocCategory
        | StructureIndicator
        | ObjectType
        | StartupDate
        | ConstructionYear
        | ConstructionMonth
        | CompanyCode
        | SuperiorFuncLoc
        | EquipInstall
        | StatusOfAnObject
        | StatusWithoutStatusNum
        
    
    type EquiChange = 
        | DeleteClass of equiId: string * className: string
        | DeleteChar of equiId: string * className: string * charName: string
        | UpdateProperties of equiId: string * changes: (EquiProperty * ValuaValue) list        
        | UpdateChar of equiId: string * className: string * charName: string * value: ValuaValue
        | UpdateMultilingualText of equiId: string * text: string
         
    type FlocChange = 
        | DeleteClass of funcLoc: FuncLocPath * className: string
        | DeleteChar of funcLoc: FuncLocPath * className: string * charName: string
        | UpdateProperties of funcLoc: FuncLocPath * changes: (FlocProperty * ValuaValue) list
        | UpdateChar of funcLoc: FuncLocPath * className: string * charName: string * value: ValuaValue
        | UpdateMultilingualText of funcLoc: FuncLocPath * text: string


 