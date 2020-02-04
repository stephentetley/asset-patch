// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplateCatalogue



module Ctos =
    
    open AssetPatch.TemplatePatcher.Base.CommonTypes
    open AssetPatch.TemplatePatcher.Base.Template


    /// Class:CTOSSY
    let ctossy : Characteristic list -> Classification = 
        _classification "CTOSSY" 
    

    /// SYSTEM_TYPE
    /// Often "REMOTE TELEMETRY OUTSTATION"...
    let system_type (v : string) : Characteristic = 
        _characteristic "SYSTEM_TYPE" (TextValue v)

        
