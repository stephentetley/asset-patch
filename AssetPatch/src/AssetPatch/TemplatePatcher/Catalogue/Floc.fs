// Copyright (c) Stephen Tetley 202
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Catalogue


module Floc =
    
    open AssetPatch.Base.ValuaValue
    open AssetPatch.TemplatePatcher.Base.Template
    open AssetPatch.Lib.OSGB36

    // ************************************************************************
    // Function

    let _no_functions_ : Function list = []

    let control_and_automation : FlocClass list -> ProcessGroup list -> Function =
        _function "CAA" "Control and Automation" "CAA"

    let electrical_power_supply : FlocClass list -> ProcessGroup list -> Function =
        _function "E" "Electrical Power Supply" "EPS"

    let environmental_discharge : FlocClass list -> ProcessGroup list -> Function =
        _function "EDC" "Environmental Discharge" "EDC"

    let site_infrastructure : FlocClass list -> ProcessGroup list -> Function =
        _function "SIF" "Site Infrastructure" "SIF"


    // ************************************************************************
    // Process Group

    let _no_process_groups_ : ProcessGroup list = []

    let control : FlocClass list -> Process list -> ProcessGroup =
        _processGroup "CON" "Control" "CON"

    let combined_treatment : FlocClass list -> Process list -> ProcessGroup =
        _processGroup "COT" "Combined Treatment" "COT"

    let liquid_discharge : FlocClass list -> Process list -> ProcessGroup =
        _processGroup "LQD" "Liquid Discharge" "LQD"

    let networks : FlocClass list -> Process list -> ProcessGroup =
        _processGroup "NET" "Networks" "NET"

    let waste_water_transfer : FlocClass list -> Process list -> ProcessGroup =
        _processGroup "WTF" "Waste Water Transfer" "WTF"


    // ************************************************************************
    // Process

    let _no_processes_ : Process list = []

    let pumping : FlocClass list -> System list -> Process =
        _process "PMG" "Pumping" "PMG"

    let regulatory_monitoring : FlocClass list -> System list -> Process =
        _process "RGM" "Regulatory Monitoring" "RGM"

    let telemetry : FlocClass list -> System list -> Process =
        _process "TEL" "Telemetry" "TEL"

    // ************************************************************************
    // System

    let _no_systems_ : System list = []

    let montoring_system (shortCode : string) (description : string) 
                : FlocClass list -> Assembly list -> Equipment list -> System =        
        _system shortCode description "SMON"


    let telemetry_system (shortCode : string) (description : string)  
                : FlocClass list -> Assembly list -> Equipment list -> System =    
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
    // Classes and characteritics

    /// *:UNICLASS_CODE
    /// This is currently blank
    let uniclass_code () : FlocCharacteristic = 
        _flocCharacteristic "UNICLASS_CODE" NullValue

    /// *:UNICLASS_DESC
    /// This is currently blank
    let uniclass_desc () : FlocCharacteristic = 
        _flocCharacteristic "UNICLASS_DESC" NullValue

    /// *:UNICLASS_DESC
    /// This is usually "SEE LONG TEXT"
    let memo_line (v: string) : FlocCharacteristic = 
        _flocCharacteristic "MEMO_LINE" (TextValue v)


    /// AIB_REFERENCE
    let aib_reference : FlocCharacteristic list -> FlocClass = 
        _flocClass "AIB_REFERENCE"

    /// AIB_REFERENCE:AI2_AIB_REFERENCE
    let ai2_aib_reference (v : string) : FlocCharacteristic = 
        _flocCharacteristic "AI2_AIB_REFERENCE" (TextValue v)

    /// AIB_REFERENCE:S4_AIB_REFERENCE
    /// This is always blank
    let s4_aib_reference () : FlocCharacteristic = 
        _flocCharacteristic "S4_AIB_REFERENCE" NullValue


    let aib_reference_common (saiNumber : string) : FlocClass = 
        aib_reference [ ai2_aib_reference saiNumber; s4_aib_reference () ]



    /// Class:EAST_NORTH
    let east_north : FlocCharacteristic list -> FlocClass = 
        _flocClass "EAST_NORTH" 

    /// EAST_NORTH:EASTING
    let easting (v : int) : FlocCharacteristic = 
        _flocCharacteristic "EASTING" (intValue v)

    /// EAST_NORTH:NORTHING
    let northing (v : int) : FlocCharacteristic = 
        _flocCharacteristic "NORTHING" (intValue v)

    
    let east_north_common (ngr : string) : FlocClass = 
        match NGR.Create ngr with
        | None -> flocClassError (sprintf "invalid NGR: %s" ngr)
        | Some s -> 
            let ea = ngrToEastingNorthing s
            east_north [ easting ea.Easting ;  northing ea.Northing ]

    