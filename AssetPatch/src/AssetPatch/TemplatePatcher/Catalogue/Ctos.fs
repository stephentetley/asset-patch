// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Catalogue



module Ctos =
    
    open AssetPatch.Base.ValuaValue
    open AssetPatch.TemplatePatcher.Base.Template


    /// Class:CTOSSY
    let ctossy : FlocCharacteristic list -> FlocClass = 
        _flocClass "CTOSSY" 
    

    /// SYSTEM_TYPE
    /// Often "REMOTE TELEMETRY OUTSTATION"...
    let system_type (v : string) : FlocCharacteristic = 
        _flocCharacteristic "SYSTEM_TYPE" (TextValue v)

        
