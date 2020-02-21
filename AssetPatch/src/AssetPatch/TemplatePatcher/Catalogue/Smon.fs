// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Catalogue



module Smon =
    
    open AssetPatch.Base.ValuaValue
    open AssetPatch.TemplatePatcher.Base.Template


    /// Class:SMONSY
    let smonsy : FlocCharacteristic list -> FlocClass = 
        _flocClass "SMONSY" 
    

    /// SYSTEM_TYPE
    let system_type (v : string) : FlocCharacteristic = 
        _flocCharacteristic "SYSTEM_TYPE" (TextValue v)
