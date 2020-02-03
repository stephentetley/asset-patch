// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Base



module CompilerMonad =
    
    open FSharp.Core

    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath    
    open AssetPatch.TemplatePatcher.Base
    open AssetPatch.TemplatePatcher.Base.Template

 

    // Custom data goes in UserEnv
    type CompilerEnv<'uenv> = 
        { TemplateEnv : TemplateEnv
          FlocVariant : string option
          EquiVariant : string option
          UserEnv: 'uenv
         }

            
    let defaultCompilerEnv (templateEnv: TemplateEnv) (userEnv: 'uenv) : CompilerEnv<'uenv> = 
        { TemplateEnv = templateEnv
          FlocVariant = None
          EquiVariant = None
          UserEnv = userEnv }
    
    type State = int

    /// CompilerMonad is a Reader-Error monad.
    type CompilerMonad<'a, 'uenv> = 
        private | CompilerMonad of (CompilerEnv<'uenv> -> State -> Result<'a * State, ErrMsg>)

    let inline private apply1 (ma : CompilerMonad<'a, 'uenv>) 
                                (env : CompilerEnv<'uenv>) 
                                (st: State) : Result<'a * State, ErrMsg> = 
        let (CompilerMonad fn) = ma in fn env st

    let mreturn (x:'a) : CompilerMonad<'a, 'uenv> = 
        CompilerMonad <| fun _ st -> Ok (x, st)

    let inline private bindM (ma : CompilerMonad<'a, 'uenv>) 
                             (fn : 'a -> CompilerMonad<'b, 'uenv>) : CompilerMonad<'b, 'uenv> =
        CompilerMonad <| fun env st -> 
            match apply1 ma env st with
            | Ok (a, s1) -> apply1 (fn a) env s1
            | Error msg -> Error msg

    let failM (msg:string) : CompilerMonad<'a, 'uenv> = 
        CompilerMonad <| fun _ _ -> Error msg
    
    let inline private altM  (ma : CompilerMonad<'a, 'uenv>) 
                             (mb : CompilerMonad<'a, 'uenv>) : CompilerMonad<'a, 'uenv> = 
        CompilerMonad <| fun env st -> 
            match apply1 ma env st with
            | Ok ans -> Ok ans
            | Error _ -> apply1 mb env st
    
    
    let inline private delayM (fn : unit -> CompilerMonad<'a, 'uenv>) : CompilerMonad<'a, 'uenv> = 
        bindM (mreturn ()) fn 
    
    type CompilerMonadBuilder() = 
        member self.Return x        = mreturn x
        member self.Bind (p,f)      = bindM p f
        member self.Zero ()         = failM "Zero"
        member self.Combine (p,q)   = altM p q
        member self.Delay fn        = delayM fn
        member self.ReturnFrom(ma)  = ma


    let (compile : CompilerMonadBuilder) = new CompilerMonadBuilder()


    let runCompiler (userEnv : 'uenv)
                    (action : CompilerMonad<'a, 'uenv> ) : Result<'a, ErrMsg> =         
        let templateEnv  = { CurrentFloc = None
                             Properties = defaultEnvProperties () }
        let stateZero = 0
        apply1 action (defaultCompilerEnv templateEnv userEnv) stateZero
            |> Result.map fst


    


    // ************************************************************************
    // Usual monadic operations


    // Common operations
    let fmapM (update : 'a -> 'b) 
              (action : CompilerMonad<'a, 'uenv>) : CompilerMonad<'b, 'uenv> = 
        compile {
            let! a = action
            return (update a)
            }
            
       
    /// Operator for fmap.
    let ( |>> ) (action : CompilerMonad<'a, 'uenv>) 
                (update : 'a -> 'b) : CompilerMonad<'b, 'uenv> = 
        fmapM update action

    /// Flipped fmap.
    let ( <<| ) (update : 'a -> 'b) 
                (action : CompilerMonad<'a, 'uenv>) : CompilerMonad<'b, 'uenv> = 
        fmapM update action

    /// Haskell Applicative's (<*>)
    let apM (mf : CompilerMonad<'a -> 'b, 'uenv>) 
            (ma : CompilerMonad<'a, 'uenv>) : CompilerMonad<'b, 'uenv> = 
        compile { 
            let! fn = mf
            let! a = ma
            return (fn a) 
        }

    /// Operator for apM
    let ( <*> ) (ma : CompilerMonad<'a -> 'b, 'uenv>) 
                (mb : CompilerMonad<'a, 'uenv>) : CompilerMonad<'b, 'uenv> = 
        apM ma mb

    /// Bind operator
    let ( >>= ) (ma : CompilerMonad<'a, 'uenv>) 
                (fn : 'a -> CompilerMonad<'b, 'uenv>) : CompilerMonad<'b, 'uenv> = 
        bindM ma fn

    /// Flipped Bind operator
    let ( =<< ) (fn : 'a -> CompilerMonad<'b, 'uenv>) 
                (ma : CompilerMonad<'a, 'uenv>) : CompilerMonad<'b, 'uenv> = 
        bindM ma fn


    let kleisliL (mf : 'a -> CompilerMonad<'b, 'uenv>)
                 (mg : 'b -> CompilerMonad<'c, 'uenv>)
                 (source:'a) : CompilerMonad<'c, 'uenv> = 
        compile { 
            let! b = mf source
            let! c = mg b
            return c
        }

    /// Flipped kleisliL
    let kleisliR (mf : 'b -> CompilerMonad<'c, 'uenv>)
                 (mg : 'a -> CompilerMonad<'b, 'uenv>)
                 (source:'a) : CompilerMonad<'c, 'uenv> = 
        compile { 
            let! b = mg source
            let! c = mf b
            return c
        }


    /// Operator for kleisliL
    let (>=>) (mf : 'a -> CompilerMonad<'b, 'uenv>)
              (mg : 'b -> CompilerMonad<'c, 'uenv>)
              (source:'a) : CompilerMonad<'c, 'uenv> = 
        kleisliL mf mg source


    /// Operator for kleisliR
    let (<=<) (mf : 'b -> CompilerMonad<'c, 'uenv>)
              (mg : 'a -> CompilerMonad<'b, 'uenv>)
              (source:'a) : CompilerMonad<'c, 'uenv> = 
        kleisliR mf mg source

    /// Perform two actions in sequence. 
    /// Ignore the results of the second action if both succeed.
    let seqL (action1 : CompilerMonad<'a, 'uenv>) 
             (action2 : CompilerMonad<'b, 'uenv>) : CompilerMonad<'a, 'uenv> = 
        compile { 
            let! a = action1
            let! _ = action2
            return a
        }

    /// Operator for seqL
    let (.>>) (action1 : CompilerMonad<'a, 'uenv>) 
                (action2 : CompilerMonad<'b, 'uenv>) : CompilerMonad<'a, 'uenv> = 
        seqL action1 action2

    /// Perform two actions in sequence. 
    /// Ignore the results of the first action if both succeed.
    let seqR (action1 : CompilerMonad<'a, 'uenv>) 
             (action2 : CompilerMonad<'b, 'uenv>) : CompilerMonad<'b, 'uenv> = 
        compile { 
            let! _ = action1
            let! b = action2
            return b
        }

    /// Operator for seqR
    let (>>.) (action1 : CompilerMonad<'a, 'uenv>) 
              (action2 : CompilerMonad<'b, 'uenv>) : CompilerMonad<'b, 'uenv> = 
        seqR action1 action2


    // ************************************************************************
    // Errors

    let throwError (msg : string) : CompilerMonad<'a, 'uenv> = 
        CompilerMonad <| fun _ _ -> Error msg


    let swapError (newMessage : string) 
                  (ma : CompilerMonad<'a, 'uenv>) : CompilerMonad<'a, 'uenv> = 
        CompilerMonad <| fun env st -> 
            apply1 ma env st |> Result.mapError (fun _ -> newMessage)
            

    /// Operator for flip swapError
    let ( <?> ) (action : CompilerMonad<'a, 'uenv>) (msg : string) : CompilerMonad<'a, 'uenv> = 
        swapError msg action
    
    let augmentError (update : string -> string) 
                     (action : CompilerMonad<'a, 'uenv>) : CompilerMonad<'a, 'uenv> = 
        CompilerMonad <| fun env st ->
            apply1 action env st |> Result.mapError update



    // ************************************************************************
    // Reader operations

    let ask () : CompilerMonad<CompilerEnv<'uenv>, 'uenv> = 
        CompilerMonad <| fun env st -> Ok (env, st)

    let asks (projection : CompilerEnv<'uenv> -> 'a) : CompilerMonad<'a, 'uenv> = 
        CompilerMonad <| fun env st -> Ok (projection env, st)

    let local (modify : CompilerEnv<'uenv> -> CompilerEnv<'uenv>) (ma : CompilerMonad<'a, 'uenv>) : CompilerMonad<'a, 'uenv> = 
        CompilerMonad <| fun env st -> apply1 ma (modify env) st

    let askUserEnv () : CompilerMonad<'uenv, 'uenv> = 
        asks (fun env -> env.UserEnv)

    let asksUserEnv (projection : 'uenv -> 'a) : CompilerMonad<'a, 'uenv> = 
        asks (fun env -> projection env.UserEnv)

    let localUserEnv (modify : 'uenv -> 'uenv) (ma : CompilerMonad<'a, 'uenv>) : CompilerMonad<'a, 'uenv> = 
        local (fun env -> { env with UserEnv = modify env.UserEnv}) ma


    // ************************************************************************
    // Name supply

    let freshName (namer: int -> string): CompilerMonad<string, 'uenv>= 
        CompilerMonad <| fun _ st -> Ok (namer st, st+1)
            


    // ************************************************************************
    // Alternatives and Optionals...

    let ( <|> ) (action1 : CompilerMonad<'a, 'uenv>)  
                (action2 : CompilerMonad<'a, 'uenv>) : CompilerMonad<'a, 'uenv> = 
        altM action1 action2

    let liftOption (opt : Option<'a>) : CompilerMonad<'a, 'uenv> = 
        match opt with
        | Some ans -> mreturn ans
        | None -> throwError "liftOption None"

    let liftResult (result : Result<'a, ErrMsg>) : CompilerMonad<'a, 'uenv> = 
        match result with
        | Ok ans -> mreturn ans
        | Error err -> throwError err

    let liftResultBy (errProjection : 'Err -> ErrMsg) 
                     (result : Result<'a, 'Err>) : CompilerMonad<'a, 'uenv> = 
        match result with
        | Ok ans -> mreturn ans
        | Error err -> throwError (errProjection err)

    /// Evaluate a thunk that may trow an exception (cf Haskell IO)
    let liftAction (action : unit -> 'a) : CompilerMonad<'a, 'uenv> = 
        try 
            action () |> mreturn
        with
        | ex -> throwError ex.Message

    /// Try to run a computation.
    /// On failure, recover or throw again with the handler.
    let attempt (action : CompilerMonad<'a, 'uenv>) 
                (handler : ErrMsg -> CompilerMonad<'a, 'uenv>) : CompilerMonad<'a, 'uenv> = 
        CompilerMonad <| fun env st ->
            match apply1 action env st with
            | Ok ans -> Ok ans
            | Error msg -> apply1 (handler msg) env st


    let assertM (cond : CompilerMonad<bool, 'uenv>) 
                (errMsg : string) : CompilerMonad<unit, 'uenv> = 
        compile {
            match! cond with
            | true -> return ()
            | false -> return! throwError errMsg
        }

    let assertBool (cond: bool) (errMsg: string) : CompilerMonad<unit, 'uenv> = 
        compile {
            match cond with
            | true -> return ()
            | false -> return! throwError errMsg
        }

    /// Run a potentially failing action. If it succeeds the answer
    /// is wrapped in ``Some``. 
    /// If it fails trap the error and return ``None``.
    let optional (action : CompilerMonad<'a, 'uenv>) : CompilerMonad<'a option, 'uenv> = 
        attempt (action |>> Some) (fun _ -> mreturn None)

    /// Run an optional action - if it returns ``Some a`` return the 
    /// answer. If it returns ``None`` the fail.
    let getOptional (action : CompilerMonad<'a option, 'uenv>) : CompilerMonad<'a, 'uenv> = 
        compile { 
            match! action with
            | Some a -> return a
            | None -> return! throwError "getOptional - None" 
        }




    let choice (actions : CompilerMonad<'a, 'uenv> list) : CompilerMonad<'a, 'uenv> = 
        CompilerMonad <| fun env st -> 
            /// State doesn't need to be in the worker
            let rec work acts cont = 
                match acts with 
                | [] -> Error "choice"
                | action1 :: rest ->
                    match apply1 action1 env st with
                    | Ok ans -> cont (Ok ans)
                    | Error _ -> work rest cont
            work actions (fun x -> x)

    // ************************************************************************
    // liftM2 etc

    // liftM (which is fmap)
    let liftM (fn : 'a -> 'ans) 
                (action : CompilerMonad<'a, 'uenv>) : CompilerMonad<'ans, 'uenv> = 
        fmapM fn action

    let liftM2 (combine : 'a -> 'b -> 'ans) 
                (action1 : CompilerMonad<'a, 'uenv>) 
                (action2 : CompilerMonad<'b, 'uenv>) : CompilerMonad<'ans, 'uenv> = 
        compile { 
            let! a = action1
            let! b = action2
            return (combine a b)
        }

    let liftM3 (combine : 'a -> 'b -> 'c -> 'ans) 
                (action1 : CompilerMonad<'a, 'uenv>) 
                (action2 : CompilerMonad<'b, 'uenv>) 
                (action3 : CompilerMonad<'c, 'uenv>) : CompilerMonad<'ans, 'uenv> = 
        compile { 
            let! a = action1
            let! b = action2
            let! c = action3
            return (combine a b c)
        }

    let liftM4 (combine : 'a -> 'b -> 'c -> 'd -> 'ans) 
                (action1 : CompilerMonad<'a, 'uenv>) 
                (action2 : CompilerMonad<'b, 'uenv>) 
                (action3 : CompilerMonad<'c, 'uenv>) 
                (action4 : CompilerMonad<'d, 'uenv>) : CompilerMonad<'ans, 'uenv> = 
        compile { 
            let! a = action1
            let! b = action2
            let! c = action3
            let! d = action4
            return (combine a b c d)
        }


    let liftM5 (combine : 'a -> 'b -> 'c -> 'd -> 'e -> 'ans) 
                (action1 : CompilerMonad<'a, 'uenv>) 
                (action2 : CompilerMonad<'b, 'uenv>) 
                (action3 : CompilerMonad<'c, 'uenv>) 
                (action4 : CompilerMonad<'d, 'uenv>) 
                (action5 : CompilerMonad<'e, 'uenv>) : CompilerMonad<'ans, 'uenv> = 
        compile { 
            let! a = action1
            let! b = action2
            let! c = action3
            let! d = action4
            let! e = action5
            return (combine a b c d e)
        }

    let liftM6 (combine : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'ans) 
                (action1 : CompilerMonad<'a, 'uenv>) 
                (action2 : CompilerMonad<'b, 'uenv>) 
                (action3 : CompilerMonad<'c, 'uenv>) 
                (action4 : CompilerMonad<'d, 'uenv>) 
                (action5 : CompilerMonad<'e, 'uenv>) 
                (action6 : CompilerMonad<'f, 'uenv>) : CompilerMonad<'ans, 'uenv> = 
        compile { 
            let! a = action1
            let! b = action2
            let! c = action3
            let! d = action4
            let! e = action5
            let! f = action6
            return (combine a b c d e f)
        }

    let tupleM2 (action1 : CompilerMonad<'a, 'uenv>) 
                (action2 : CompilerMonad<'b, 'uenv>) : CompilerMonad<'a * 'b, 'uenv> = 
        liftM2 (fun a b -> (a,b)) action1 action2

    let tupleM3 (action1 : CompilerMonad<'a, 'uenv>) 
                (action2 : CompilerMonad<'b, 'uenv>) 
                (action3 : CompilerMonad<'c, 'uenv>) : CompilerMonad<'a * 'b * 'c, 'uenv> = 
        liftM3 (fun a b c -> (a,b,c)) action1 action2 action3

    let tupleM4 (action1 : CompilerMonad<'a, 'uenv>) 
                (action2 : CompilerMonad<'b, 'uenv>) 
                (action3 : CompilerMonad<'c, 'uenv>) 
                (action4 : CompilerMonad<'d, 'uenv>) : CompilerMonad<'a * 'b * 'c * 'd, 'uenv> = 
        liftM4 (fun a b c d -> (a,b,c,d)) action1 action2 action3 action4

    let tupleM5 (action1 : CompilerMonad<'a, 'uenv>) 
                (action2 : CompilerMonad<'b, 'uenv>) 
                (action3 : CompilerMonad<'c, 'uenv>) 
                (action4 : CompilerMonad<'d, 'uenv>) 
                (action5 : CompilerMonad<'e, 'uenv>) : CompilerMonad<'a * 'b * 'c * 'd * 'e, 'uenv> = 
        liftM5 (fun a b c d e -> (a,b,c,d,e)) action1 action2 action3 action4 action5

    let tupleM6 (action1 : CompilerMonad<'a, 'uenv>) 
                (action2 : CompilerMonad<'b, 'uenv>) 
                (action3 : CompilerMonad<'c, 'uenv>) 
                (action4 : CompilerMonad<'d, 'uenv>) 
                (action5 : CompilerMonad<'e, 'uenv>) 
                (action6 : CompilerMonad<'f, 'uenv>) : CompilerMonad<'a * 'b * 'c * 'd * 'e * 'f, 'uenv> = 
        liftM6 (fun a b c d e f -> (a,b,c,d,e,f)) 
                action1 action2 action3 action4 action5 action6



    let pipeM2 (action1 : CompilerMonad<'a, 'uenv>) 
               (action2 : CompilerMonad<'b, 'uenv>) 
               (combine:'a -> 'b -> 'ans) : CompilerMonad<'ans, 'uenv> = 
        liftM2 combine action1 action2

    let pipeM3 (action1 : CompilerMonad<'a, 'uenv>) 
               (action2 : CompilerMonad<'b, 'uenv>) 
               (action3 : CompilerMonad<'c, 'uenv>) 
               (combine : 'a -> 'b -> 'c -> 'ans) : CompilerMonad<'ans, 'uenv> = 
        liftM3 combine action1 action2 action3

    let pipeM4 (action1 : CompilerMonad<'a, 'uenv>) 
               (action2 : CompilerMonad<'b, 'uenv>) 
               (action3 : CompilerMonad<'c, 'uenv>) 
               (action4 : CompilerMonad<'d, 'uenv>) 
               (combine : 'a -> 'b -> 'c -> 'd -> 'ans) : CompilerMonad<'ans, 'uenv> = 
        liftM4 combine action1 action2 action3 action4

    let pipeM5 (action1 : CompilerMonad<'a, 'uenv>) 
               (action2 : CompilerMonad<'b, 'uenv>) 
               (action3 : CompilerMonad<'c, 'uenv>) 
               (action4 : CompilerMonad<'d, 'uenv>) 
               (action5 : CompilerMonad<'e, 'uenv>) 
               (combine : 'a -> 'b -> 'c -> 'd -> 'e -> 'ans) : CompilerMonad<'ans, 'uenv> = 
        liftM5 combine action1 action2 action3 action4 action5

    let pipeM6 (action1 : CompilerMonad<'a, 'uenv>) 
               (action2 : CompilerMonad<'b, 'uenv>) 
               (action3 : CompilerMonad<'c, 'uenv>) 
               (action4 : CompilerMonad<'d, 'uenv>) 
               (action5 : CompilerMonad<'e, 'uenv>) 
               (action6 : CompilerMonad<'f, 'uenv>) 
               (combine : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'ans) : CompilerMonad<'ans, 'uenv> = 
        liftM6 combine action1 action2 action3 action4 action5 action6

    // ************************************************************************
    // Templates

    let evalTemplate (rootFloc : FuncLocPath) (code : Template<'a>) : CompilerMonad<'a, 'uenv> = 
        CompilerMonad <| fun env st ->
            let env1 = { env.TemplateEnv with CurrentFloc = Some rootFloc }
            match runTemplate env1 code with
            | Ok ans -> Ok (ans, st)
            | Error msg -> Error msg

    let evalTemplateNoFloc (code : Template<'a>) : CompilerMonad<'a, 'uenv> = 
        CompilerMonad <| fun env st ->
            let env1 = { env.TemplateEnv with CurrentFloc = None }
            match runTemplate env1 code with
            | Ok a -> Ok (a, st)
            | Error msg -> Error msg




    // ************************************************************************
    // Traversals

    /// Implemented in CPS 
    let mapM (mf: 'a -> CompilerMonad<'b, 'uenv>) 
             (source : 'a list) : CompilerMonad<'b list, 'uenv> = 
        CompilerMonad <| fun env state -> 
            let rec work (xs : 'a list)
                         (st: State)
                         (fk : ErrMsg -> Result<'b list * State, ErrMsg>) 
                         (sk : State -> 'b list -> Result<'b list * State, ErrMsg>) = 
                match xs with
                | [] -> sk st []
                | x :: rest -> 
                    match apply1 (mf x) env st with
                    | Error msg -> fk msg
                    | Ok (v1,s1) -> 
                        work rest s1 fk (fun s2 vs ->
                        sk s2 (v1::vs))
            work source state (fun msg -> Error msg) (fun st ans -> Ok (ans, st))

    let forM (xs : 'a list) 
             (fn : 'a -> CompilerMonad<'b, 'uenv>) : CompilerMonad<'b list, 'uenv> = 
        mapM fn xs


    /// Implemented in CPS 
    let mapMz (mf: 'a -> CompilerMonad<'b, 'uenv>) 
              (source : 'a list) : CompilerMonad<unit, 'uenv> = 
        CompilerMonad <| fun env state -> 
            let rec work (xs : 'a list)
                         (st: State)
                         (fk : ErrMsg -> Result<unit * State, ErrMsg>) 
                         (sk : State -> Result<unit * State, ErrMsg>) = 
                match xs with
                | [] -> sk st
                | x :: rest ->
                    match apply1 (mf x) env st with
                    | Error msg -> fk msg
                    | Ok (_, s1) -> work rest s1 fk sk
            work source state (fun msg -> Error msg) (fun st -> Ok ((), st))


    let forMz (xs : 'a list) 
              (fn : 'a -> CompilerMonad<'b, 'uenv>) : CompilerMonad<unit, 'uenv> = 
        mapMz fn xs

    let foldM (action : 'acc -> 'a -> CompilerMonad<'acc, 'uenv>) 
                (initial : 'acc)
                (source : 'a list) : CompilerMonad<'acc, 'uenv> = 
        CompilerMonad <| fun env state -> 
            let rec work (acc : 'acc) 
                            (xs : 'a list) 
                            (st: State)
                            (fk : ErrMsg -> Result<'acc * State, ErrMsg>) 
                            (sk : State -> 'acc -> Result<'acc * State, ErrMsg>) = 
                match xs with
                | [] -> sk st acc
                | x1 :: rest -> 
                    match apply1 (action acc x1) env st with
                    | Error msg -> fk msg
                    | Ok (acc1, s1) -> 
                        work acc1 rest s1 fk sk
            work initial source state (fun msg -> Error msg) (fun st a -> Ok (a, st))



    let smapM (action : 'a -> CompilerMonad<'b, 'uenv>) 
                (source : seq<'a>) : CompilerMonad<seq<'b>, 'uenv> = 
        CompilerMonad <| fun env state ->
            let sourceEnumerator = source.GetEnumerator()
            let rec work (st: State) 
                            (fk : ErrMsg -> Result<seq<'b> * State, ErrMsg>) 
                            (sk : State -> seq<'b> -> Result<seq<'b> * State, ErrMsg>) = 
                if not (sourceEnumerator.MoveNext()) then 
                    sk st Seq.empty 
                else
                    let a1 = sourceEnumerator.Current
                    match apply1 (action a1) env st with
                    | Error msg -> fk msg
                    | Ok (b1, s1) -> 
                        work s1 fk (fun s2 sx -> 
                        sk s2 (seq { yield b1; yield! sx }))
            work state (fun msg -> Error msg) (fun st a -> Ok (a, st))

    let sforM (sx : seq<'a>) 
              (fn : 'a -> CompilerMonad<'b, 'uenv>) : CompilerMonad<seq<'b>, 'uenv> = 
        smapM fn sx
    
    let smapMz (action : 'a -> CompilerMonad<'b, 'uenv>) 
                (source : seq<'a>) : CompilerMonad<unit, 'uenv> = 
        CompilerMonad <| fun env state ->
            let sourceEnumerator = source.GetEnumerator()
            let rec work (st: State) 
                            (fk : ErrMsg -> Result<unit * State, ErrMsg>) 
                            (sk : State -> Result<unit * State, ErrMsg>) = 
                if not (sourceEnumerator.MoveNext()) then 
                    sk st
                else
                    let a1 = sourceEnumerator.Current
                    match apply1 (action a1) env st with
                    | Error msg -> fk msg
                    | Ok (_, s1) -> 
                        work s1 fk sk
            work state (fun msg -> Error msg) (fun st -> Ok ((), st))

    
    let sforMz (source : seq<'a>) 
                (action : 'a -> CompilerMonad<'b, 'uenv>) : CompilerMonad<unit, 'uenv> = 
        smapMz action source

        
    let sfoldM (action : 'acc -> 'a -> CompilerMonad<'acc, 'uenv>) 
                (initial : 'acc)
                (source : seq<'a>) : CompilerMonad<'acc, 'uenv> = 
        CompilerMonad <| fun env state ->
            let sourceEnumerator = source.GetEnumerator()
            let rec work (acc : 'acc) 
                            (st: State)
                            (fk : ErrMsg -> Result<'acc * State, ErrMsg>) 
                            (sk : State -> 'acc -> Result<'acc * State, ErrMsg>) = 
                if not (sourceEnumerator.MoveNext()) then 
                    sk st acc
                else
                    let x = sourceEnumerator.Current
                    match apply1 (action acc x) env st with
                    | Error msg -> fk msg
                    | Ok (acc1, s1) -> work acc1 s1 fk sk
            work initial state (fun msg -> Error msg) (fun st a -> Ok (a, st))


    /// Implemented in CPS 
    let mapiM (mf : int -> 'a -> CompilerMonad<'b, 'uenv>) 
                (source : 'a list) : CompilerMonad<'b list, 'uenv> = 
        CompilerMonad <| fun env state -> 
            let rec work (xs : 'a list)
                         (count : int)
                         (st: State)
                         (fk : ErrMsg -> Result<'b list * State, ErrMsg>) 
                         (sk : State -> 'b list -> Result<'b list * State, ErrMsg>) = 
                match xs with
                | [] -> sk st []
                | y :: ys -> 
                    match apply1 (mf count y) env st with
                    | Error msg -> fk msg
                    | Ok (v1, s1) -> 
                        work ys (count+1) s1 fk (fun s2 vs ->
                        sk s2 (v1::vs))
            work source 0 state (fun msg -> Error msg) (fun st a -> Ok (a, st))


    /// Implemented in CPS 
    let mapiMz (mf : int -> 'a -> CompilerMonad<'b, 'uenv>) 
                (source : 'a list) : CompilerMonad<unit, 'uenv> = 
        CompilerMonad <| fun env state -> 
            let rec work (xs : 'a list) 
                         (count : int)
                         (st: State)
                         (fk : ErrMsg -> Result<unit * State, ErrMsg>) 
                         (sk : State -> Result<unit * State, ErrMsg>) = 
                match xs with
                | [] -> sk st
                | y :: ys -> 
                    match apply1 (mf count y) env st with
                    | Error msg -> fk msg
                    | Ok (_, s1) -> work ys (count+1) s1 fk sk
            work source 0 state (fun msg -> Error msg) (fun st -> Ok ((), st))

    

    let foriM (xs : 'a list) 
              (fn : int -> 'a -> CompilerMonad<'b, 'uenv>) : CompilerMonad<'b list, 'uenv> = 
        mapiM fn xs

    let foriMz (xs : 'a list) 
               (fn : int -> 'a -> CompilerMonad<'b, 'uenv>) : CompilerMonad<unit, 'uenv> = 
        mapiMz fn xs


    /// Implemented in CPS 
    let filterM (mf: 'a -> CompilerMonad<bool, 'uenv>) 
                (source : 'a list) : CompilerMonad<'a list, 'uenv> = 
        CompilerMonad <| fun env state -> 
            let rec work (xs: 'a list)
                         (st: State)
                         (fk: ErrMsg -> Result<'a list * State, ErrMsg>) 
                         (sk: State -> 'a list -> Result<'a list * State, ErrMsg>) = 
                match xs with
                | [] -> sk st []
                | y :: ys -> 
                    match apply1 (mf y) env st with
                    | Error msg -> fk msg
                    | Ok (test, s1) -> 
                        work ys s1 fk (fun s2 vs ->
                        let vs1 = if test then (y::vs) else vs
                        sk s2 vs1)
            work source state (fun msg -> Error msg) (fun st a -> Ok (a, st))

    let unzipMapM (mf: 'a -> CompilerMonad<'b * 'c, 'uenv>) (source: 'a list): CompilerMonad<'b list * 'c list, 'uenv> = 
        compile {
            let! xs = mapM mf source
            return (List.map fst xs, List.map snd xs)
        }


    // ************************************************************************
    // Compile Class * Valua patches to update exting Flocs and Equipment...


    let applyTemplate (path : FuncLocPath) 
                                (xs: ('name * 'b) list ) 
                                (template: 'b -> Template.Template<'c>) : CompilerMonad<('name * 'c) list, 'uenv> = 
        forM xs (fun (name, x) -> evalTemplate path (template x) >>=  fun a -> mreturn (name, a))

    let applyFlocTemplate1 (path : FuncLocPath, body : 'a) 
                            (template: 'a -> Template.Template<'b>) : CompilerMonad<'b, 'uenv> = 
        evalTemplate path (template body)

    let applyFlocTemplate (xs: (FuncLocPath * 'a) list ) 
                        (template: 'a -> Template.Template<'b>) : CompilerMonad<'b list, 'uenv> = 
        forM xs (fun (path, x) -> evalTemplate path (template x))
