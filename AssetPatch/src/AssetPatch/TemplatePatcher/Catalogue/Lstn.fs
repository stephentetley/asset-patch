﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Catalogue



module Lstn =
    
    open System

    open AssetPatch.Base.ValuaValue
    open AssetPatch.TemplatePatcher.Base.Template

    let lstn_level_transmitter (name : string) (startupDate: DateTime) (memoLine : string)
                : EquipmentAttribute list -> EquiClass list -> Equipment list -> Equipment =
        _equipment name "I" "LSTN" "LSTNUT" startupDate memoLine


    /// Class:LSTNUT
    let lstnut : EquiCharacteristic list -> EquiClass = 
        _equiClass "LSTNUT"     

    /// Too many cases to make an enum worthwhile
    type RelayFunction = string

    /// LSTN_RELAY_{ix}_FUNCTION
    let lstn_relay_function (ix : int) (v : RelayFunction) : EquiCharacteristic = 
        let name = sprintf "LSTN_RELAY_%i_FUNCTION" ix
        _equiCharacteristic name (textUpperCase v)


    /// LSTN_RELAY_{ix}_ON_LEVEL_M
    let lstn_relay_on_level (ix : int) (v : decimal) : EquiCharacteristic = 
        let name = sprintf "LSTN_RELAY_%i_ON_LEVEL_M" ix
        _equiCharacteristic name (DecimalValue v)

    /// LSTN_RELAY_{ix}_OFF_LEVEL_M
    let lstn_relay_off_level (ix : int) (v : decimal) : EquiCharacteristic = 
        let name = sprintf "LSTN_RELAY_%i_OFF_LEVEL_M" ix
        _equiCharacteristic name (DecimalValue v)

    /// LSTN_TRANSDUCER_MODEL
    let lstn_transducer_model (v : string) : EquiCharacteristic =         
        _equiCharacteristic "LSTN_TRANSDUCER_MODEL" (TextValue v)

    /// LSTN_TRANSDUCER_SERIAL_NO
    let lstn_transducer_serial_no (v : string) : EquiCharacteristic =         
        _equiCharacteristic "LSTN_TRANSDUCER_SERIAL_NO" (TextValue v)

    /// LSTN_RANGE_MIN
    let lstn_range_min (v : decimal) : EquiCharacteristic =         
        _equiCharacteristic "LSTN_RANGE_MIN" (DecimalValue v)

    /// LSTN_RANGE_MAX
    let lstn_range_max (v : decimal) : EquiCharacteristic =         
        _equiCharacteristic "LSTN_RANGE_MAX" (DecimalValue v)


    /// LSTN_RANGE_UNITS
    let lstn_range_units (v : string) : EquiCharacteristic =         
        _equiCharacteristic "LSTN_RANGE_UNITS" (TextValue v)

    /// LSTN_SIGNAL_TYPE
    let lstn_signal_type (v : string) : EquiCharacteristic =         
        _equiCharacteristic "LSTN_SIGNAL_TYPE" (TextValue v)

    /// LSTN_SUPPLY_VOLTAGE
    let lstn_supply_voltage (v : int) : EquiCharacteristic =         
        _equiCharacteristic "LSTN_SUPPLY_VOLTAGE" (intValue v)

    /// IP_RATING
    let ip_rating (v : int) : EquiCharacteristic =         
        _equiCharacteristic "IP_RATING" (intValue v)

    /// MEMO_LINE 
    /// Usually "SEE LONG TEXT"
    let memo_line(v : string) : EquiCharacteristic = 
        _equiCharacteristic "MEMO_LINE" (TextValue v)