// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Catalogue



module Netw =
    
    open System

    open AssetPatch.Base.ValuaValue
    open AssetPatch.TemplatePatcher.Base.Template


    let telemetry_outstation (name : string) (startupDate: DateTime) (memoLine : string)
                : EquipmentAttribute list -> EquiClass list -> Equipment list -> Equipment = 
        _equipment name "I" "NETW" "NETWTL" startupDate memoLine

    let modem (name : string) (startupDate: DateTime) (memoLine : string)
                : EquipmentAttribute list -> EquiClass list -> Equipment list -> Equipment = 
        _equipment name "I" "NETW" "NETWMO" startupDate memoLine


    /// Class:NETWTL
    let netwtl : EquiCharacteristic list -> EquiClass = 
        _equiClass "NETWTL" 

    /// Class:NETWMO
    let netwmo : EquiCharacteristic list -> EquiClass = 
        _equiClass "NETWMO" 
    
    /// NETW_SUPPLY_VOLTAGE
    let netw_supply_voltage (v : int) : EquiCharacteristic =         
        _equiCharacteristic "NETW_SUPPLY_VOLTAGE" (intValue v)

    /// NETW_SUPPLY_VOLTAGE_UNITS
    let netw_supply_voltage_units (v : string) : EquiCharacteristic =         
        _equiCharacteristic "NETW_SUPPLY_VOLTAGE_UNITS" (TextValue v)

    /// IP_RATING
    let ip_rating (v : string) : EquiCharacteristic =         
        _equiCharacteristic "IP_RATING" (TextValue v)

    /// NETW_SPECIFIC_ORDER_CODE
    let netw_specific_order_code (v : string) : EquiCharacteristic =         
        _equiCharacteristic "NETW_SPECIFIC_ORDER_CODE" (TextValue v)
    
    /// MEMO_LINE
    let memo_line (v : string) : EquiCharacteristic =         
        _equiCharacteristic "MEMO_LINE" (TextValue v)

    /// LOCATION_ON_SITE
    let location_on_site (v : string) : EquiCharacteristic =         
        _equiCharacteristic "LOCATION_ON_SITE" (TextValue v)

    /// SPECIFIC_MODEL
    let specific_model (v : string) : EquiCharacteristic =         
        _equiCharacteristic "SPECIFIC_MODEL" (TextValue v)

    /// MANUFACTURERS_ASSET_LIFE_YR
    let manufacturers_asset_life_yr (v : int) : EquiCharacteristic =         
        _equiCharacteristic "MANUFACTURERS_ASSET_LIFE_YR" (intValue v)
