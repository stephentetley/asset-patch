// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.RewritePatcher.Base

module Uxl = 

    open AssetPatch.RewritePatcher.Base.UpdateTypes

    let genEqMultilingualText (equipmentId: string)
                                (changes: EqMultilingualTextChange list) : EquiMultilingualText = 
        { EquipmentId = equipmentId
          DeleteIndicator: bool
        Language: string
        DescriptionMedium: string 
        LongText: string
        }