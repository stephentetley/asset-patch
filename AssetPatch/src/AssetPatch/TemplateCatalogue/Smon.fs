// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplateCatalogue



module Smon =
    
    open AssetPatch.Base.ValuaValue
    open AssetPatch.TemplatePatcher.Base.Template


    /// Class:SMONSY
    let smonsy : Characteristic list -> Classification = 
        _classification "SMONSY" 
    

    /// SYSTEM_TYPE
    let system_type (v : string) : Characteristic = 
        _characteristic "SYSTEM_TYPE" (TextValue v)
