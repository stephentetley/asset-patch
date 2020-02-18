// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.RewritePatcher.Base

module RewriteMonad = 


    open AssetPatch.Base.Common
    open AssetPatch.RewritePatcher.Base    
    open AssetPatch.RewritePatcher.Base.UpdateTypes
    
    type Env<'uenv> = 
        { ChangeRequestDescription: string
          TypeOfChangeRequest: string 
          ProcessRequester: string
          UserEnv: 'uenv
        }

    let defaultEnv (changeRequestDescription: string) (userEnv: 'uenv) : Env<'uenv> = 
        { ChangeRequestDescription= changeRequestDescription
          TypeOfChangeRequest = "AIWEAM0P"
          ProcessRequester = "ASSET DATA"
          UserEnv = userEnv
        }

    /// RewriteMonad is a Reader-Error monad.
    type RewriteMonad<'a, 'uenv> = 
        private | RewriteMonad of (Env<'uenv> -> Result<'a, ErrMsg>)

    let inline private apply1 (ma : RewriteMonad<'a, 'uenv>) 
                                (env : Env<'uenv>) : Result<'a, ErrMsg> = 
        let (RewriteMonad fn) = ma in fn env

    let mreturn (x:'a) : RewriteMonad<'a, 'uenv> = 
        RewriteMonad <| fun _ -> Ok x

    let inline private bindM (ma : RewriteMonad<'a, 'uenv>) 
                             (fn : 'a -> RewriteMonad<'b, 'uenv>) : RewriteMonad<'b, 'uenv> =
        RewriteMonad <| fun env -> 
            match apply1 ma env with
            | Ok a -> apply1 (fn a) env
            | Error msg -> Error msg

    let failM (msg:string) : RewriteMonad<'a, 'uenv> = 
        RewriteMonad <| fun _ -> Error msg
    
    let inline private altM  (ma : RewriteMonad<'a, 'uenv>) 
                             (mb : RewriteMonad<'a, 'uenv>) : RewriteMonad<'a, 'uenv> = 
        RewriteMonad <| fun env -> 
            match apply1 ma env with
            | Ok ans -> Ok ans
            | Error _ -> apply1 mb env

    let inline private delayM (fn : unit -> RewriteMonad<'a, 'uenv>) : RewriteMonad<'a, 'uenv> = 
        bindM (mreturn ()) fn 
    
    type RewriteMonadBuilder() = 
        member self.Return x        = mreturn x
        member self.Bind (p,f)      = bindM p f
        member self.Zero ()         = failM "Zero"
        member self.Combine (p,q)   = altM p q
        member self.Delay fn        = delayM fn
        member self.ReturnFrom(ma)  = ma


    let (rewriter : RewriteMonadBuilder) = new RewriteMonadBuilder()


    let runRewriter (env : Env<'uenv>)
                    (action : RewriteMonad<'a, 'uenv> ) : Result<'a, ErrMsg> =         
        apply1 action env

    // ************************************************************************
    // Reader

    let ask () : RewriteMonad<'uenv, 'uenv> = 
        RewriteMonad <| fun env -> Ok env.UserEnv

    let askChangeRequestDescription() : RewriteMonad<string, 'uenv> = 
        RewriteMonad <| fun env -> Ok env.ChangeRequestDescription

    let askProcessRequester() : RewriteMonad<string, 'uenv> = 
        RewriteMonad <| fun env -> Ok env.ProcessRequester

    let askTypeOfChangeRequest() : RewriteMonad<string, 'uenv> = 
        RewriteMonad <| fun env -> Ok env.TypeOfChangeRequest


    // Common operations
    let fmapM (update : 'a -> 'b) 
              (action : RewriteMonad<'a, 'uenv>) : RewriteMonad<'b, 'uenv> = 
        rewriter {
            let! a = action
            return (update a)
            }

    /// Operator for fmap.
    let ( |>> ) (action : RewriteMonad<'a, 'uenv>) 
                (update : 'a -> 'b) : RewriteMonad<'b, 'uenv> = 
        fmapM update action

    /// Flipped fmap.
    let ( <<| ) (update : 'a -> 'b) 
                (action : RewriteMonad<'a, 'uenv>) : RewriteMonad<'b, 'uenv> = 
        fmapM update action

    // ************************************************************************
    // Errors

    let throwError (msg : string) : RewriteMonad<'a, 'uenv> = 
        RewriteMonad <| fun _ -> Error msg

    // ************************************************************************
    // Strategies

    /// Apply the rewrite to all items in the source list. All must succeed.
    let rewriteAll (rw: Rewrite.Rewrite<'a, 'source, 'target>) 
                    (sources: 'source list) : RewriteMonad<'a list * 'target list, 'uenv> = 
        RewriteMonad <| fun _ -> 
            let rec work xs fk sk = 
                match xs with
                | [] -> sk [] Rewrite.Empty
                | x :: rs -> 
                    match Rewrite.runRewrite rw x with
                    | Error msg -> fk msg
                    | Ok(v1, js) -> work rs fk (fun vs js2 -> sk (v1 :: vs) (Rewrite.join js js2))
            work sources (fun msg -> Error msg) (fun xs jl -> Ok(xs, Rewrite.joinToList jl))


    // ************************************************************************
    // Lifters


    let liftResult (result : Result<'a, ErrMsg>) : RewriteMonad<'a, 'uenv> = 
        match result with
        | Ok ans -> mreturn ans
        | Error err -> throwError err

    // ************************************************************************
    // Traversals

    /// Implemented in CPS 
    let mapM (mf: 'a -> RewriteMonad<'b, 'uenv>) 
             (source : 'a list) : RewriteMonad<'b list, 'uenv> = 
        RewriteMonad <| fun env -> 
            let rec work (xs : 'a list)
                         (fk : ErrMsg -> Result<'b list, ErrMsg>) 
                         (sk : 'b list -> Result<'b list, ErrMsg>) = 
                match xs with
                | [] -> sk []
                | x :: rest -> 
                    match apply1 (mf x) env with
                    | Error msg -> fk msg
                    | Ok v1 -> 
                        work rest fk (fun vs ->
                        sk (v1::vs))
            work source (fun msg -> Error msg) (fun ans -> Ok ans)

