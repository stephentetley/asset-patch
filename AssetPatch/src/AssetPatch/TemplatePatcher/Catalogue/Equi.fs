// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Catalogue



module Equi =
    
    open AssetPatch.Base.ValuaValue
    open AssetPatch.TemplatePatcher.Base.Template
    open AssetPatch.Lib.OSGB36
// ************************************************************************
    // Equipment

    let _no_equipment_ : Equipment list = []
    let _no_subordinate_equipment_ : Equipment list = []





    // ************************************************************************
    // Equipment attributes

    let manufacturer (name : string) : EquipmentAttribute = 
        equipmentAttribute <| fun e1 ->  { e1 with Manufacturer = Some name }
        
    let model (name : string) : EquipmentAttribute =
        equipmentAttribute <| fun e1 -> { e1 with Model = Some name }
        
    let serial_number (productCode : string) : EquipmentAttribute =
        equipmentAttribute <| fun e1 -> { e1 with SerialNumber = Some productCode }

    let construction_year (year : int) : EquipmentAttribute = 
        equipmentAttribute <| fun e1 -> { e1 with ConstructionYear = Some (uint16 year) }
    
    let construction_month (month : int) : EquipmentAttribute = 
        equipmentAttribute <| fun e1 -> { e1 with ConstructionMonth = Some (uint8 month) }
    
    // ************************************************************************
    // Classes and characteritics

    /// *:UNICLASS_CODE
    /// This is currently blank
    let uniclass_code () : EquiCharacteristic = 
        _equiCharacteristic "UNICLASS_CODE" NullValue

    /// *:UNICLASS_DESC
    /// This is currently blank
    let uniclass_desc () : EquiCharacteristic = 
        _equiCharacteristic "UNICLASS_DESC" NullValue

    /// *:UNICLASS_DESC
    /// This is usually "SEE LONG TEXT"
    let memo_line (v: string) : EquiCharacteristic = 
        _equiCharacteristic "MEMO_LINE" (TextValue v)


    /// AIB_REFERENCE
    let aib_reference : EquiCharacteristic list -> EquiClass = 
        _equiClass "AIB_REFERENCE"

    /// AIB_REFERENCE:AI2_AIB_REFERENCE
    let ai2_aib_reference (v : string) : EquiCharacteristic = 
        _equiCharacteristic "AI2_AIB_REFERENCE" (TextValue v)

    /// AIB_REFERENCE:S4_AIB_REFERENCE
    /// This is always blank
    let s4_aib_reference () : EquiCharacteristic = 
        _equiCharacteristic "S4_AIB_REFERENCE" NullValue


    let aib_reference_common (saiNumber : string) 
                             (pliNumber: string) : EquiClass = 
        aib_reference 
            [ ai2_aib_reference saiNumber
              ai2_aib_reference pliNumber
            ]


    /// Class:EAST_NORTH
    let east_north : EquiCharacteristic list -> EquiClass = 
        _equiClass "EAST_NORTH" 

    /// EAST_NORTH:EASTING
    let easting (v : int) : EquiCharacteristic = 
        _equiCharacteristic "EASTING" (intValue v)

    /// EAST_NORTH:NORTHING
    let northing (v : int) : EquiCharacteristic = 
        _equiCharacteristic "NORTHING" (intValue v)

    
    let east_north_common (ngr : string) : EquiClass = 
        match NGR.Create ngr with
        | None -> equiClassError (sprintf "invalid NGR: %s" ngr)
        | Some s -> 
            let ea = ngrToEastingNorthing s
            east_north [ easting ea.Easting ;  northing ea.Northing ]

    