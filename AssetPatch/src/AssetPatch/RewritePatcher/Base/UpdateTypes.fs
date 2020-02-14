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
        | UpdateProperties of equiId: string * changes: (EquiProperty * ValuaValue) list        
        | UpdateMultilingualText of equiId: string * text: string
        | DeleteClass of equiId: string * className: string
        | DeleteChar of equiId: string * className: string * charName: string
        | UpdateChar of equiId: string * className: string * charName: string * value: ValuaValue
         
    type FlocChange = 
        | UpdateProperties of funcLoc: FuncLocPath * changes: (FlocProperty * ValuaValue) list
        | UpdateMultilingualText of funcLoc: FuncLocPath * text: string
        | DeleteClass of funcLoc: FuncLocPath * className: string
        | DeleteChar of funcLoc: FuncLocPath * className: string * charName: string
        | UpdateChar of funcLoc: FuncLocPath * className: string * charName: string * value: ValuaValue
       

    /// Sort all changes, coalesce UpdateProperties on same floc
    let groupFlocPropertyChanges (source: FlocChange list): FlocChange list = 
        let rec work x xs cont = 
            match xs with
            | [] -> cont [x]
            | c :: cs -> 
                match (x,c) with
                | FlocChange.UpdateProperties(f1, ps1), FlocChange.UpdateProperties(f2, ps2) when f1 = f2 -> 
                    let x1 = FlocChange.UpdateProperties(f1, ps1 @ ps2)
                    work x1 cs (fun vs -> cont vs)
                | _, _ -> work c cs (fun vs -> cont (x::vs))

        match List.sort source with
        | [] -> []
        | x :: xs -> work x xs (fun ac -> ac)

        
    /// Sort all changes, coalesce UpdateProperties on same floc
    let groupEquiPropertyChanges (source: EquiChange list): EquiChange list = 
        let rec work x xs cont = 
            match xs with
            | [] -> cont [x]
            | c :: cs -> 
                match (x,c) with
                | EquiChange.UpdateProperties(f1, ps1), EquiChange.UpdateProperties(f2, ps2) when f1 = f2 -> 
                    let x1 = EquiChange.UpdateProperties(f1, ps1 @ ps2)
                    work x1 cs (fun vs -> cont vs)
                | _, _ -> work c cs (fun vs -> cont (x::vs))

        match List.sort source with
        | [] -> []
        | x :: xs -> work x xs (fun ac -> ac)
