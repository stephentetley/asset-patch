// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.RewritePatcher.Catalogue


module FlocRoot =
    
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.Base.ValuaValue
    open AssetPatch.RewritePatcher.Base.UpdateTypes
    open AssetPatch.RewritePatcher.Base.Rewrite
    

    let deleteMultilingualText () : FlocRewrite<unit, #HasFuncLoc> =         
        primitiveFlocRewrite (fun floc -> FlocChange.DeleteMultilingualText(floc))

    let deleteClass (className: string): FlocRewrite<unit, #HasFuncLoc> = 
        primitiveFlocRewrite (fun floc -> FlocChange.DeleteClass(floc, className))

    let deleteCharacteristic (className: string) (charName: string): FlocRewrite<unit, #HasFuncLoc> = 
        primitiveFlocRewrite (fun floc -> FlocChange.DeleteChar(floc, className, charName))

    let updateProperty (prop: FlocProperty) (value: ValuaValue): FlocRewrite<unit, #HasFuncLoc>  = 
        primitiveFlocRewrite (fun floc -> FlocChange.UpdateProperties(floc, [(prop, value)]))

    let updateMultilingualText (text: string) : FlocRewrite<unit, #HasFuncLoc>  = 
        primitiveFlocRewrite (fun floc -> FlocChange.UpdateMultilingualText(floc, text))

    let updateCharacteristic (className: string) 
                            (charName: string) 
                            (value: ValuaValue) : FlocRewrite<unit, #HasFuncLoc> = 
        primitiveFlocRewrite (fun floc -> FlocChange.UpdateChar(floc, className, charName, value))



    // ************************************************************************
    // Functional Location attributes

    let description (value: string) : FlocRewrite<Unit, #HasFuncLoc> = 
        updateProperty FlocProperty.DescriptionMedium (TextValue value)
    

