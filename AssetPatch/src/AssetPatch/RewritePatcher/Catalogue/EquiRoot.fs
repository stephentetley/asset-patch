// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.RewritePatcher.Catalogue



module EquiRoot =
    
    open System

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.Base.ValuaValue
    open AssetPatch.RewritePatcher.Base.UpdateTypes
    open AssetPatch.RewritePatcher.Base.Rewrite
    
    // ************************************************************************
    // Equipment 

    let deleteMultilingualText (): EquiRewrite<unit, #HasEquiId> = 
        primitiveEquiRewrite (fun equiId -> EquiChange.DeleteMultilingualText(equiId))

    let deleteClass (className: string): EquiRewrite<unit, #HasEquiId> = 
        primitiveEquiRewrite (fun equiId -> EquiChange.DeleteClass(equiId, className))

    let deleteCharacteristic (className: string) (charName: string) : EquiRewrite<unit, #HasEquiId> = 
        primitiveEquiRewrite (fun equiId -> EquiChange.DeleteChar(equiId, className, charName))

    let updateProperty (prop: EquiProperty) (value: ValuaValue): EquiRewrite<unit, #HasEquiId>  = 
        primitiveEquiRewrite (fun equiId -> EquiChange.UpdateProperties(equiId, [(prop, value)]))

    let updateMultilingualText (text: String): EquiRewrite<unit, #HasEquiId>  = 
        primitiveEquiRewrite (fun equiId -> EquiChange.UpdateMultilingualText(equiId, text))


    let updateChararacteristic  (className: string) 
                                (charName: string) 
                                (value: ValuaValue) : EquiRewrite<unit, #HasEquiId> = 
        primitiveEquiRewrite (fun equiId -> EquiChange.UpdateChar(equiId, className, charName, value))


  

    // ************************************************************************
    // Equipment attributes


    /// Usually "I" or "E"... 
    let equipmentCategory (value: string) : EquiRewrite<Unit, #HasEquiId> = 
        updateProperty EquiProperty.EquipCategory (TextValue value)


    let description (value: string) : EquiRewrite<Unit, #HasEquiId> = 
        updateProperty EquiProperty.DescriptionMedium (TextValue value)

    let objectType (value: string) : EquiRewrite<Unit, #HasEquiId> = 
        updateProperty EquiProperty.ObjectType (TextValue value)

    let startupDate (value: DateTime option) : EquiRewrite<Unit, #HasEquiId> = 
        match value with
        | None -> 
            updateProperty EquiProperty.StartupDate NullValue
        | Some dt -> 
            updateProperty EquiProperty.StartupDate (DateValue dt)

    let manufacturer (value: string) : EquiRewrite<Unit, #HasEquiId> = 
        updateProperty EquiProperty.Manufacturer (TextValue value)
        

    let modelNumber (value: string) : EquiRewrite<Unit, #HasEquiId> = 
        updateProperty EquiProperty.ModelNumber (TextValue value)
        
    let manufacturerPartNumber (value: string) : EquiRewrite<Unit, #HasEquiId> = 
        updateProperty EquiProperty.ManufPartNum (TextValue value)
    
    let manufacturerSerialNumber (value: string) : EquiRewrite<Unit, #HasEquiId> = 
        updateProperty EquiProperty.ManufSerialNum (TextValue value)
    
    let constructionYear (value: int option) : EquiRewrite<Unit, #HasEquiId> = 
        match value with
        | None -> 
            updateProperty EquiProperty.ConstructionYear NullValue
        | Some d -> 
            updateProperty EquiProperty.ConstructionYear (IntValue (bigint d))

    
    let constructionMonth (value: int option) : EquiRewrite<Unit, #HasEquiId> = 
        match value with
        | None -> 
            updateProperty EquiProperty.ConstructionMonth NullValue
        | Some d -> 
            updateProperty EquiProperty.ConstructionMonth (IntValue (bigint d))

    let companyCode (value: string) : EquiRewrite<Unit, #HasEquiId> =
        updateProperty EquiProperty.CompanyCode (TextValue value)

    let functionalLocation (value: FuncLocPath option) : EquiRewrite<Unit, #HasEquiId> =
        match value with
        | None -> 
            updateProperty EquiProperty.FunctionalLocation NullValue
        | Some floc -> 
            updateProperty EquiProperty.FunctionalLocation (TextValue <| floc.ToString())


    let superordinateEquipment (value: string) : EquiRewrite<Unit, #HasEquiId> =
        updateProperty EquiProperty.SuperordEquip (TextValue value)

    let statusOfAnObject (value: string) : EquiRewrite<Unit, #HasEquiId> =
        updateProperty EquiProperty.StatusOfAnObject (TextValue value)

    let statusWithoutStatusNum (value: string) : EquiRewrite<Unit, #HasEquiId> =
        updateProperty EquiProperty.StatusWithoutStatusNum (TextValue value)

