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

    let deleteMultilingualText (): EquiRewrite<#HasEquiId> = 
        primitiveEquiRewrite (fun equiId _ -> EquiChange.DeleteMultilingualText(equiId))

    let deleteClass (className: string): EquiRewrite<#HasEquiId> = 
        primitiveEquiRewrite (fun equiId _ -> EquiChange.DeleteClass(equiId, className))

    let deleteCharacteristic (className: string) (charName: string) : EquiRewrite<#HasEquiId> = 
        primitiveEquiRewrite (fun equiId _ -> EquiChange.DeleteChar(equiId, className, charName))

    let updateProperty (prop: EquiProperty) (value: ValuaValue): EquiRewrite<#HasEquiId>  = 
        primitiveEquiRewrite (fun equiId superId -> EquiChange.UpdateProperties(equiId, superId, [(prop, value)]))

    let updateMultilingualText (text: String): EquiRewrite<#HasEquiId>  = 
        primitiveEquiRewrite (fun equiId _ -> EquiChange.UpdateMultilingualText(equiId, text))


    let updateChararacteristic  (className: string) 
                                (charName: string) 
                                (value: ValuaValue) : EquiRewrite<#HasEquiId> = 
        primitiveEquiRewrite (fun equiId _ -> EquiChange.UpdateChar(equiId, className, charName, value))


  

    // ************************************************************************
    // Equipment attributes


    /// Usually "I" or "E"... 
    let equipmentCategory (value: string) : EquiRewrite<#HasEquiId> = 
        updateProperty EquiProperty.EquipCategory (TextValue value)


    let description (value: string) : EquiRewrite<#HasEquiId> = 
        updateProperty EquiProperty.DescriptionMedium (TextValue value)

    let objectType (value: string) : EquiRewrite<#HasEquiId> = 
        updateProperty EquiProperty.ObjectType (TextValue value)

    let startupDate (value: DateTime option) : EquiRewrite<#HasEquiId> = 
        match value with
        | None -> 
            updateProperty EquiProperty.StartupDate NullValue
        | Some dt -> 
            updateProperty EquiProperty.StartupDate (DateValue dt)

    let manufacturer (value: string) : EquiRewrite<#HasEquiId> = 
        updateProperty EquiProperty.Manufacturer (TextValue value)
        

    let modelNumber (value: string) : EquiRewrite<#HasEquiId> = 
        updateProperty EquiProperty.ModelNumber (TextValue value)
        
    let manufacturerPartNumber (value: string) : EquiRewrite<#HasEquiId> = 
        updateProperty EquiProperty.ManufPartNum (TextValue value)
    
    let manufacturerSerialNumber (value: string) : EquiRewrite<#HasEquiId> = 
        updateProperty EquiProperty.ManufSerialNum (TextValue value)
    
    let constructionYear (value: int option) : EquiRewrite<#HasEquiId> = 
        match value with
        | None -> 
            updateProperty EquiProperty.ConstructionYear NullValue
        | Some d -> 
            updateProperty EquiProperty.ConstructionYear (IntValue (bigint d))

    
    let constructionMonth (value: int option) : EquiRewrite<#HasEquiId> = 
        match value with
        | None -> 
            updateProperty EquiProperty.ConstructionMonth NullValue
        | Some d -> 
            updateProperty EquiProperty.ConstructionMonth (IntValue (bigint d))

    let companyCode (value: string) : EquiRewrite<#HasEquiId> =
        updateProperty EquiProperty.CompanyCode (TextValue value)

    let functionalLocation (value: FuncLocPath option) : EquiRewrite<#HasEquiId> =
        match value with
        | None -> 
            updateProperty EquiProperty.FunctionalLocation NullValue
        | Some floc -> 
            updateProperty EquiProperty.FunctionalLocation (TextValue <| floc.ToString())


    let superordinateEquipment (value: string) : EquiRewrite<#HasEquiId> =
        updateProperty EquiProperty.SuperordEquip (TextValue value)

    let statusOfAnObject (value: string) : EquiRewrite<#HasEquiId> =
        updateProperty EquiProperty.StatusOfAnObject (TextValue value)

    let statusWithoutStatusNum (value: string) : EquiRewrite<#HasEquiId> =
        updateProperty EquiProperty.StatusWithoutStatusNum (TextValue value)

