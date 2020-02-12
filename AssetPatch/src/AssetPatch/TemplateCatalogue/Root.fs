// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplateCatalogue


[<AutoOpen>]
module Root =
    
    open AssetPatch.Base.ValuaValue
    open AssetPatch.TemplatePatcher.Base.Template
    open AssetPatch.Lib.OSGB36

    // ************************************************************************
    // Function

    let _no_functions_ : Function list = []

    let control_and_automation : Classification list -> ProcessGroup list -> Function =
        _function "CAA" "Control and Automation" "CAA"

    let electrical_power_supply : Classification list -> ProcessGroup list -> Function =
        _function "E" "Electrical Power Supply" "EPS"

    let environmental_discharge : Classification list -> ProcessGroup list -> Function =
        _function "EDC" "Environmental Discharge" "EDC"

    let site_infrastructure : Classification list -> ProcessGroup list -> Function =
        _function "SIF" "Site Infrastructure" "SIF"


    // ************************************************************************
    // Process Group

    let _no_process_groups_ : ProcessGroup list = []

    let control : Classification list -> Process list -> ProcessGroup =
        _processGroup "CON" "Control" "CON"

    let combined_treatment : Classification list -> Process list -> ProcessGroup =
        _processGroup "COT" "Combined Treatment" "COT"

    let liquid_discharge : Classification list -> Process list -> ProcessGroup =
        _processGroup "LQD" "Liquid Discharge" "LQD"

    let networks : Classification list -> Process list -> ProcessGroup =
        _processGroup "NET" "Networks" "NET"

    let waste_water_transfer : Classification list -> Process list -> ProcessGroup =
        _processGroup "WTF" "Waste Water Transfer" "WTF"


    // ************************************************************************
    // Process

    let _no_processes_ : Process list = []

    let pumping : Classification list -> System list -> Process =
        _process "PMG" "Pumping" "PMG"

    let regulatory_monitoring : Classification list -> System list -> Process =
        _process "RGM" "Regulatory Monitoring" "RGM"

    let telemetry : Classification list -> System list -> Process =
        _process "TEL" "Telemetry" "TEL"

    // ************************************************************************
    // System

    let _no_systems_ : System list = []

    let montoring_system (shortCode : string) (description : string) 
                : Classification list -> Assembly list -> Equipment list -> System =        
        _system shortCode description "SMON"


    let telemetry_system (shortCode : string) (description : string)  
                : Classification list -> Assembly list -> Equipment list -> System =    
        _system shortCode description "CTOS"

    
    // ************************************************************************
    // Assembly

    let _no_assemblies_ : Assembly list = []
    
    // ************************************************************************
    // Item

    let _no_items_ : Item list = []

    // ************************************************************************
    // Component

    let _no_components_ : Component list = []

    
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
    let uniclass_code () : Characteristic = 
        _characteristic "UNICLASS_CODE" NullValue

    /// *:UNICLASS_DESC
    /// This is currently blank
    let uniclass_desc () : Characteristic = 
        _characteristic "UNICLASS_DESC" NullValue

    /// *:UNICLASS_DESC
    /// This is usually "SEE LONG TEXT"
    let memo_line (v: string) : Characteristic = 
        _characteristic "MEMO_LINE" (TextValue v)


    /// AIB_REFERENCE
    let aib_reference : Characteristic list -> Classification = 
        _classification "AIB_REFERENCE"

    /// AIB_REFERENCE:AI2_AIB_REFERENCE
    let ai2_aib_reference (v : string) : Characteristic = 
        _characteristic "AI2_AIB_REFERENCE" (TextValue v)

    /// AIB_REFERENCE:S4_AIB_REFERENCE
    /// This is always blank
    let s4_aib_reference () : Characteristic = 
        _characteristic "S4_AIB_REFERENCE" NullValue


    let aib_reference_floc_common (saiNumber : string) : Classification = 
        aib_reference [ ai2_aib_reference saiNumber; s4_aib_reference () ]

    let aib_reference_equipment_common (saiNumber : string) (pliNumber: string) : Classification = 
        aib_reference 
            [ ai2_aib_reference saiNumber
              ai2_aib_reference pliNumber
              s4_aib_reference () 
            ]


    /// Class:EAST_NORTH
    let east_north : Characteristic list -> Classification = 
        _classification "EAST_NORTH" 

    /// EAST_NORTH:EASTING
    let easting (v : int) : Characteristic = 
        _characteristic "EASTING" (intValue v)

    /// EAST_NORTH:NORTHING
    let northing (v : int) : Characteristic = 
        _characteristic "NORTHING" (intValue v)

    
    let east_north_common (ngr : string) : Classification = 
        match NGR.Create ngr with
        | None -> templateError (sprintf "invalid NGR: %s" ngr)
        | Some s -> 
            let ea = ngrToEastingNorthing s
            east_north [ easting ea.Easting ;  northing ea.Northing ]

    