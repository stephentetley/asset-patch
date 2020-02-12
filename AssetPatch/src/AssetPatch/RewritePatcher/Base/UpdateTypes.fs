// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.RewritePatcher.Base

module UpdateTypes = 

    open System

    type EqCharacteristicChange = 
        | CharValue of string
        | ChDeleteId of bool

    type EqClassChange = 
        | EqCharacteristicChange of charName: string * change: EqCharacteristicChange
        | DeleteInd of bool
        

    type EqMultilingualTextChange = 
        | LongText of string
        | DeleteIndicator of bool
        | Language of string

    let eqMultilingualTextChangesMap (source: EqMultilingualTextChange list): Map<string, 

    type EquipmentDataChange = 
        | EqClassChange of className: string * change: EqClassChange
        | EqMultiLingualTextChange of change: EqMultilingualTextChange
        | EquipmentCategory of string
        | Description of string
        | ObjectType of string
        | StartupDate of DateTime option
        | Manufacturer of string
        | ModelNumber of string
        | ManufPartNum of string


    type FlocCharacteristicChange = 
        | CharValue of string
        | ChDeleteId of bool

    type FlocClassChange = 
        | EqCharacteristicChange of charName: string * change: EqCharacteristicChange
        | DeleteInd of bool
        

    type FlocMultilingualTextChange = 
        | LongText of string
        | DeleteIndicator of bool
        | Language of string
       


    