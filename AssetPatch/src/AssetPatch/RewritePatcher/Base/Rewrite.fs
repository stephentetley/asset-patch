// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.RewritePatcher.Base

module Rewrite = 

    open System
    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.Base.ValuaValue
    open AssetPatch.RewritePatcher.Base.UpdateTypes


    type private JoinList<'a> = 
        | Empty
        | One of 'a
        | Join of left: JoinList<'a> * right: JoinList<'a>

    let private toList (source: JoinList<'a>): 'a list = 
        let rec work js ac cont = 
            match js with
            | Empty -> cont ac
            | One x -> cont (x :: ac)
            | Join(a,b) -> 
                work b ac (fun ac1 -> 
                work a ac1 cont)
        work source [] (fun x -> x)

    let private append (a: JoinList<'a>) (b: JoinList<'a>) : JoinList<'a> = Join(a,b)

    /// Writer + Error 
    type Rewrite<'a, 'change> = 
        private | Rewrite of (Result<'a * JoinList<'change>, ErrMsg>)

    let private getRewrite (src: Rewrite<'a, 'change>): Result<'a * JoinList<'change>, ErrMsg> = 
        let (Rewrite body) = src in body

    let runRewrite (rw: Rewrite<'a, 'change>): Result<'a * 'change list, ErrMsg> = 
        match getRewrite rw with
        | Ok(a, js) -> Ok(a, toList js)
        | Error msg -> Error msg

    let mreturn (x: 'a) : Rewrite<'a, 'change> = Rewrite(Ok(x, Empty))

    let inline private bindM (ma: Rewrite<'a, 'change>) 
                                (k: 'a -> Rewrite<'b, 'change>): Rewrite<'b, 'change> = 
        match getRewrite ma with
        | Error msg -> Rewrite(Error msg)
        | Ok(a, js1) -> 
            match getRewrite (k a) with
            | Error msg -> Rewrite(Error msg)
            | Ok(b, js2) -> Rewrite(Ok(b, append js1 js2))

    let inline private delayM (fn : unit -> Rewrite<'a, 'change>) : Rewrite<'a, 'change> = 
        bindM (mreturn ()) fn 

    type RewriteBuilder() = 
        member self.Return x        = mreturn x
        member self.Bind (p,f)      = bindM p f
        member self.Delay fn        = delayM fn
        member self.ReturnFrom(ma)  = ma


    let (rewrite : RewriteBuilder) = new RewriteBuilder()

    type FlocRewrite<'a> = Rewrite<'a, FlocChange>

    type EquiRewrite<'a> = Rewrite<'a, EquiChange>

    let primitiveRewrite (change: 'change): Rewrite<unit, 'change> = Rewrite(Ok((), One(change)))

    // ************************************************************************
    // Functional locations

    let deleteFlocMultilingualText (funcLoc : FuncLocPath) : FlocRewrite<unit> = 
        primitiveRewrite (FlocChange.DeleteMultilingualText(funcLoc))

    let deleteFlocClass (funcLoc : FuncLocPath) (className: string): FlocRewrite<unit> = 
        primitiveRewrite (FlocChange.DeleteClass(funcLoc, className))

    let deleteFlocChar (funcLoc : FuncLocPath) (className: string) (charName: string): FlocRewrite<unit> = 
        primitiveRewrite (FlocChange.DeleteChar(funcLoc, className, charName))

    let updateFlocProperty (funcLoc : FuncLocPath)  (prop: FlocProperty) (value: ValuaValue): FlocRewrite<unit>  = 
        primitiveRewrite (FlocChange.UpdateProperties(funcLoc, [(prop, value)]))

    let updateFlocChar (funcLoc : FuncLocPath)  
                        (className: string) 
                        (charName: string) (value: ValuaValue) : FlocRewrite<unit> = 
        primitiveRewrite (FlocChange.UpdateChar(funcLoc, className, charName, value))




    // ************************************************************************
    // Equipment 

    let deleteEquiMultilingualText (equiId : string): EquiRewrite<unit> = 
        primitiveRewrite (EquiChange.DeleteMultilingualText(equiId))

    let deleteEquiClass (equiId : string) (className: string): EquiRewrite<unit> = 
        primitiveRewrite (EquiChange.DeleteClass(equiId, className))

    let deleteEquiChar (equiId : string) (className: string) (charName: string) : EquiRewrite<unit> = 
        primitiveRewrite (EquiChange.DeleteChar(equiId, className, charName))

    let updateEquiProperty (equiId : string) (prop: EquiProperty) (value: ValuaValue): EquiRewrite<unit>  = 
        primitiveRewrite (EquiChange.UpdateProperties(equiId, [(prop, value)]))

    let updateEquiChar (equiId: string)  
                        (className: string) 
                        (charName: string) (value: ValuaValue) : EquiRewrite<unit> = 
        primitiveRewrite (EquiChange.UpdateChar(equiId, className, charName, value))
