// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.RewritePatcher.Base

module Rewrite = 

    open System
    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.Base.ValuaValue
    open AssetPatch.RewritePatcher.Base.UpdateTypes


    type internal JoinList<'a> = 
        | Empty
        | One of 'a
        | Join of left: JoinList<'a> * right: JoinList<'a>

    let internal joinToList (source: JoinList<'a>): 'a list = 
        let rec work js ac cont = 
            match js with
            | Empty -> cont ac
            | One x -> cont (x :: ac)
            | Join(a,b) -> 
                work b ac (fun ac1 -> 
                work a ac1 cont)
        work source [] (fun x -> x)

    let internal join (a: JoinList<'a>) (b: JoinList<'a>) : JoinList<'a> = Join(a,b)


        
    /// Reader + Writer + Error 
    ///
    /// The source should be able to supply EquiId of FuncLoc for the 
    /// relevant rewrites but it is expected to be a "larger" object 
    /// like a Csv row.
    /// 
    /// Target is one of FlocChange or EquiChange
    ///
    type Rewrite<'a, 'source, 'target> = 
        private | Rewrite of ('source -> Result<'a * JoinList<'target>, ErrMsg>)

    let private apply1 (rw: Rewrite<'a, 'source, 'target>) 
                        (source: 'source) : Result<'a * JoinList<'target>, ErrMsg> = 
        let (Rewrite f) = rw in f source

    let internal runRewrite (rw: Rewrite<'a, 'source, 'target>) (src: 'source) : Result<'a * JoinList<'target>, ErrMsg> = 
        apply1 rw src


        

    let mreturn (x: 'a) : Rewrite<'a, 'source, 'change> = 
        Rewrite <| fun src -> (Ok(x, Empty))

    let inline private bindM (ma: Rewrite<'a, 'source, 'change>) 
                                (k: 'a -> Rewrite<'b, 'source, 'change>): Rewrite<'b, 'source, 'change> = 
        Rewrite <| fun src -> 
            match apply1 ma src with
            | Error msg -> Error msg
            | Ok(a, js1) -> 
                match apply1 (k a) src with
                | Error msg -> Error msg
                | Ok(b, js2) -> Ok(b, join js1 js2)

    let inline private delayM (fn : unit -> Rewrite<'a, 'source, 'change>) : Rewrite<'a, 'source, 'change> = 
        bindM (mreturn ()) fn 

    type RewriteBuilder() = 
        member self.Return x        = mreturn x
        member self.Bind (p,f)      = bindM p f
        member self.Delay fn        = delayM fn
        member self.ReturnFrom(ma)  = ma


    let (rewrite : RewriteBuilder) = new RewriteBuilder()

    type FlocRewrite<'a, 'src> = Rewrite<'a, 'src, FlocChange>

    type HasFuncLoc = 
        abstract FuncLoc : FuncLocPath
   


    type EquiRewrite<'src> = Rewrite<unit, 'src, EquiChange>

    type HasEquiId = 
        abstract EquiId : string
        abstract SuperOrdinateId: string // may be blank


    let internal primitiveFlocRewrite (change: FuncLocPath -> 'change): Rewrite<unit, #HasFuncLoc, 'change> = 
        Rewrite <| fun src -> 
            let floc = src.FuncLoc
            Ok((), One(change floc))

    let internal primitiveEquiRewrite (change: string -> string -> 'change): Rewrite<unit, #HasEquiId, 'change> = 
        Rewrite <| fun src -> 
            let equiId = src.EquiId
            let superId = src.SuperOrdinateId
            Ok((), One(change equiId superId))


    // ************************************************************************
    // Reader - use different names to avoid clash with CompilerMonad

    let get () : Rewrite<'source, 'source, 'change> = 
        Rewrite <| fun src -> Ok(src, Empty)

    let gets (select: 'source -> 'a) : Rewrite<'a, 'source, 'change> = 
        Rewrite <| fun src -> Ok(select src, Empty)

    /// Note - don't provide `local`, we don't want clients changing the 
    /// source of a rewrite suring a rewrite.


    