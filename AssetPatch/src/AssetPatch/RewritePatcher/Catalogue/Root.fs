// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.RwritePatcher.Catalogue


[<AutoOpen>]
module Root =
    
    open AssetPatch.Base.ValuaValue
    open AssetPatch.RewritePatcher.Base.UpdateTypes
    open AssetPatch.RewritePatcher.Base.Rewrite
    

    // ************************************************************************
    // Equipment attributes



    let manufacturer (equiId: string) (name: string) : EquiRewrite<Unit> = 
        updateEquiProperty equiId EquiProperty.Manufacturer (TextValue name)
        

    let model (equiId: string) (name: string) : EquiRewrite<Unit> = 
        updateEquiProperty equiId EquiProperty.ModelNumber (TextValue name)
        
    