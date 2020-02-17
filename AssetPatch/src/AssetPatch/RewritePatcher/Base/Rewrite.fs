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

    let private join (a: JoinList<'a>) (b: JoinList<'a>) : JoinList<'a> = Join(a,b)


        
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

    let runRewrite (rw: Rewrite<'a, 'source, 'target>) (src: 'source) : Result<'a * 'target list, ErrMsg> = 
        match apply1 rw src with
        | Error msg -> Error  msg
        | Ok(a, js) -> Ok (a, toList js)


        

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
   


    type EquiRewrite<'a, 'src> = Rewrite<'a, 'src, EquiChange>

    type HasEquiId = 
        abstract EquiId : string
    


    let internal primitiveFlocRewrite (change: FuncLocPath -> 'change): Rewrite<unit, #HasFuncLoc, 'change> = 
        Rewrite <| fun src -> 
            let floc = src.FuncLoc
            Ok((), One(change floc))

    let internal primitiveEquiRewrite (change: string -> 'change): Rewrite<unit, #HasEquiId, 'change> = 
        Rewrite <| fun src -> 
            let equiId = src.EquiId
            Ok((), One(change equiId))

    /// Apply the rewrite to all items in the source list. All must succeed.
    let rewriteAll (rw: Rewrite<'a, 'source, 'target>) 
                    (sources: 'source list) : Result<'a list * 'target list, ErrMsg> = 
        let rec work xs fk sk = 
            match xs with
            | [] -> sk [] Empty
            | x :: rs -> 
                match apply1 rw x with
                | Error msg -> fk msg
                | Ok(v1, js) -> work rs fk (fun vs js2 -> sk (v1 :: vs) (join js js2))
        work sources (fun msg -> Error msg) (fun xs jl -> Ok(xs, toList jl))



    // ************************************************************************
    // Reader

    let ask () : Rewrite<'source, 'source, 'change> = 
        Rewrite <| fun src -> Ok(src, Empty)

    let asks (select: 'source -> 'a) : Rewrite<'a, 'source, 'change> = 
        Rewrite <| fun src -> Ok(select src, Empty)

    /// Note - don't provide `local`, we don't want clients changing the 
    /// source of a rewrite suring a rewrite.


    // ************************************************************************
    // Monadic combinators

    let fmap (fn : 'a -> 'b) (ma : Rewrite<'a, 'src, 'change>) : Rewrite<'b, 'src, 'change> = 
        Rewrite <| fun src -> 
            match apply1 ma src with
            | Ok (a,w) -> Ok (fn a, w)
            | Error msg -> Error msg

    /// Operator for fmap.
    let ( |>> ) (action : Rewrite<'a, 'src, 'target>) 
                (update : 'a -> 'b) : Rewrite<'b, 'src, 'target> = 
        fmap update action

    /// Flipped fmap.
    let ( <<| ) (update : 'a -> 'b) 
                (action : Rewrite<'a, 'src, 'target>) : Rewrite<'b, 'src, 'target> = 
        fmap update action

    /// Haskell Applicative's (<*>)
    let apM (mf : Rewrite<'a -> 'b, 'src, 'target>) 
            (ma : Rewrite<'a, 'src, 'target>) : Rewrite<'b, 'src, 'target> = 
        rewrite { 
            let! fn = mf
            let! a = ma
            return (fn a) 
        }

    /// Operator for apM
    let ( <*> ) (ma : Rewrite<'a -> 'b, 'src, 'target>) 
                (mb : Rewrite<'a, 'src, 'target>) : Rewrite<'b, 'src, 'target> = 
        apM ma mb

    /// Bind operator
    let ( >>= ) (ma : Rewrite<'a, 'src, 'target>) 
                (fn : 'a -> Rewrite<'b, 'src, 'target>) : Rewrite<'b, 'src, 'target> = 
        bindM ma fn

    /// Flipped Bind operator
    let ( =<< ) (fn : 'a -> Rewrite<'b, 'src, 'target>) 
                (ma : Rewrite<'a, 'src, 'target>) : Rewrite<'b, 'src, 'target> = 
        bindM ma fn