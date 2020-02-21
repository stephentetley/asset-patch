// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Base



module GenerateMonad =
    
    open FSharp.Core

    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath    
    open AssetPatch.TemplatePatcher.Base.Hierarchy
    open AssetPatch.TemplatePatcher.Base.Template
 

    // Custom data goes in UserEnv
    type GenerateEnv<'uenv> = 
        { TemplateEnv : TemplateEnv
          UserEnv: 'uenv
        }

            
    let defaultGenerateEnv (templateEnv: TemplateEnv) (userEnv: 'uenv) : GenerateEnv<'uenv> = 
        { TemplateEnv = templateEnv
          UserEnv = userEnv }
    
    type GenerateState = 
        { NameIndex: int }

    /// GenerateMonad is a Reader-GenerateState-Error monad.
    type GenerateMonad<'a, 'uenv> = 
        private | GenerateMonad of (GenerateEnv<'uenv> -> GenerateState -> Result<'a * GenerateState, ErrMsg>)

    let inline private apply1 (ma : GenerateMonad<'a, 'uenv>) 
                                (env : GenerateEnv<'uenv>)
                                (st :  GenerateState) : Result<'a * GenerateState, ErrMsg> = 
        let (GenerateMonad fn) = ma in fn env st

    let mreturn (x:'a) : GenerateMonad<'a, 'uenv> = 
        GenerateMonad <| fun _ st -> Ok (x, st)

    let inline private bindM (ma : GenerateMonad<'a, 'uenv>) 
                             (fn : 'a -> GenerateMonad<'b, 'uenv>) : GenerateMonad<'b, 'uenv> =
        GenerateMonad <| fun env st -> 
            match apply1 ma env st with
            | Ok (a, s1) -> apply1 (fn a) env s1
            | Error msg -> Error msg

    let failM (msg:string) : GenerateMonad<'a, 'uenv> = 
        GenerateMonad <| fun _ _ -> Error msg
    
    let inline private altM  (ma : GenerateMonad<'a, 'uenv>) 
                             (mb : GenerateMonad<'a, 'uenv>) : GenerateMonad<'a, 'uenv> = 
        GenerateMonad <| fun env st -> 
            match apply1 ma env st with
            | Ok ans -> Ok ans
            | Error _ -> apply1 mb env st
    
    
    let inline private delayM (fn : unit -> GenerateMonad<'a, 'uenv>) : GenerateMonad<'a, 'uenv> = 
        bindM (mreturn ()) fn 
    
    type GenerateMonadBuilder() = 
        member self.Return x        = mreturn x
        member self.Bind (p,f)      = bindM p f
        member self.Zero ()         = failM "Zero"
        member self.Combine (p,q)   = altM p q
        member self.Delay fn        = delayM fn
        member self.ReturnFrom(ma)  = ma


    let (generate : GenerateMonadBuilder) = new GenerateMonadBuilder()


    let runGenerate (userEnv : 'uenv)
                    (action : GenerateMonad<'a, 'uenv> ) : Result<'a, ErrMsg> =         
        let templateEnv  = defaultEnv ()
        let stateZero = { NameIndex = 2000 }
        apply1 action (defaultGenerateEnv templateEnv userEnv) stateZero 
            |> Result.map fst



    


    // ************************************************************************
    // Usual monadic operations


    // Common operations
    let fmapM (update : 'a -> 'b) 
              (action : GenerateMonad<'a, 'uenv>) : GenerateMonad<'b, 'uenv> = 
        generate {
            let! a = action
            return (update a)
            }
            
       
    /// Operator for fmap.
    let ( |>> ) (action : GenerateMonad<'a, 'uenv>) 
                (update : 'a -> 'b) : GenerateMonad<'b, 'uenv> = 
        fmapM update action

    /// Flipped fmap.
    let ( <<| ) (update : 'a -> 'b) 
                (action : GenerateMonad<'a, 'uenv>) : GenerateMonad<'b, 'uenv> = 
        fmapM update action

    /// Haskell Applicative's (<*>)
    let apM (mf : GenerateMonad<'a -> 'b, 'uenv>) 
            (ma : GenerateMonad<'a, 'uenv>) : GenerateMonad<'b, 'uenv> = 
        generate { 
            let! fn = mf
            let! a = ma
            return (fn a) 
        }

    /// Operator for apM
    let ( <*> ) (ma : GenerateMonad<'a -> 'b, 'uenv>) 
                (mb : GenerateMonad<'a, 'uenv>) : GenerateMonad<'b, 'uenv> = 
        apM ma mb

    /// Bind operator
    let ( >>= ) (ma : GenerateMonad<'a, 'uenv>) 
                (fn : 'a -> GenerateMonad<'b, 'uenv>) : GenerateMonad<'b, 'uenv> = 
        bindM ma fn

    /// Flipped Bind operator
    let ( =<< ) (fn : 'a -> GenerateMonad<'b, 'uenv>) 
                (ma : GenerateMonad<'a, 'uenv>) : GenerateMonad<'b, 'uenv> = 
        bindM ma fn


    let kleisliL (mf : 'a -> GenerateMonad<'b, 'uenv>)
                 (mg : 'b -> GenerateMonad<'c, 'uenv>)
                 (source:'a) : GenerateMonad<'c, 'uenv> = 
        generate { 
            let! b = mf source
            let! c = mg b
            return c
        }

    /// Flipped kleisliL
    let kleisliR (mf : 'b -> GenerateMonad<'c, 'uenv>)
                 (mg : 'a -> GenerateMonad<'b, 'uenv>)
                 (source:'a) : GenerateMonad<'c, 'uenv> = 
        generate { 
            let! b = mg source
            let! c = mf b
            return c
        }


    /// Operator for kleisliL
    let (>=>) (mf : 'a -> GenerateMonad<'b, 'uenv>)
              (mg : 'b -> GenerateMonad<'c, 'uenv>)
              (source:'a) : GenerateMonad<'c, 'uenv> = 
        kleisliL mf mg source


    /// Operator for kleisliR
    let (<=<) (mf : 'b -> GenerateMonad<'c, 'uenv>)
              (mg : 'a -> GenerateMonad<'b, 'uenv>)
              (source:'a) : GenerateMonad<'c, 'uenv> = 
        kleisliR mf mg source

    /// Perform two actions in sequence. 
    /// Ignore the results of the second action if both succeed.
    let seqL (action1 : GenerateMonad<'a, 'uenv>) 
             (action2 : GenerateMonad<'b, 'uenv>) : GenerateMonad<'a, 'uenv> = 
        generate { 
            let! a = action1
            let! _ = action2
            return a
        }

    /// Operator for seqL
    let (.>>) (action1 : GenerateMonad<'a, 'uenv>) 
                (action2 : GenerateMonad<'b, 'uenv>) : GenerateMonad<'a, 'uenv> = 
        seqL action1 action2

    /// Perform two actions in sequence. 
    /// Ignore the results of the first action if both succeed.
    let seqR (action1 : GenerateMonad<'a, 'uenv>) 
             (action2 : GenerateMonad<'b, 'uenv>) : GenerateMonad<'b, 'uenv> = 
        generate { 
            let! _ = action1
            let! b = action2
            return b
        }

    /// Operator for seqR
    let (>>.) (action1 : GenerateMonad<'a, 'uenv>) 
              (action2 : GenerateMonad<'b, 'uenv>) : GenerateMonad<'b, 'uenv> = 
        seqR action1 action2


    // ************************************************************************
    // Errors

    let throwError (msg : string) : GenerateMonad<'a, 'uenv> = 
        GenerateMonad <| fun _ _ -> Error msg


    let swapError (newMessage : string) 
                  (ma : GenerateMonad<'a, 'uenv>) : GenerateMonad<'a, 'uenv> = 
        GenerateMonad <| fun env st -> 
            apply1 ma env st |> Result.mapError (fun _ -> newMessage)
            

    /// Operator for flip swapError
    let ( <?> ) (action : GenerateMonad<'a, 'uenv>) (msg : string) : GenerateMonad<'a, 'uenv> = 
        swapError msg action
    
    let augmentError (update : string -> string) 
                     (action : GenerateMonad<'a, 'uenv>) : GenerateMonad<'a, 'uenv> = 
        GenerateMonad <| fun env st ->
            apply1 action env st |> Result.mapError update



    // ************************************************************************
    // Reader operations

    let ask () : GenerateMonad<GenerateEnv<'uenv>, 'uenv> = 
        GenerateMonad <| fun env st -> Ok (env, st)

    let asks (projection : GenerateEnv<'uenv> -> 'a) : GenerateMonad<'a, 'uenv> = 
        GenerateMonad <| fun env st -> Ok (projection env, st)

    let local (modify : GenerateEnv<'uenv> -> GenerateEnv<'uenv>) (ma : GenerateMonad<'a, 'uenv>) : GenerateMonad<'a, 'uenv> = 
        GenerateMonad <| fun env -> apply1 ma (modify env)

    let askUserEnv () : GenerateMonad<'uenv, 'uenv> = 
        asks (fun env -> env.UserEnv)

    let asksUserEnv (projection : 'uenv -> 'a) : GenerateMonad<'a, 'uenv> = 
        asks (fun env -> projection env.UserEnv)

    let localUserEnv (modify : 'uenv -> 'uenv) (ma : GenerateMonad<'a, 'uenv>) : GenerateMonad<'a, 'uenv> = 
        local (fun env -> { env with UserEnv = modify env.UserEnv}) ma

    // ************************************************************************
    // Alternatives and Optionals...

    let ( <|> ) (action1 : GenerateMonad<'a, 'uenv>)  
                (action2 : GenerateMonad<'a, 'uenv>) : GenerateMonad<'a, 'uenv> = 
        altM action1 action2

    let liftOption (opt : Option<'a>) : GenerateMonad<'a, 'uenv> = 
        match opt with
        | Some ans -> mreturn ans
        | None -> throwError "liftOption None"

    let liftResult (result : Result<'a, ErrMsg>) : GenerateMonad<'a, 'uenv> = 
        match result with
        | Ok ans -> mreturn ans
        | Error err -> throwError err

    let liftResultBy (errProjection : 'Err -> ErrMsg) 
                     (result : Result<'a, 'Err>) : GenerateMonad<'a, 'uenv> = 
        match result with
        | Ok ans -> mreturn ans
        | Error err -> throwError (errProjection err)

    /// Evaluate a thunk that may trow an exception (cf Haskell IO)
    let liftAction (action : unit -> 'a) : GenerateMonad<'a, 'uenv> = 
        try 
            action () |> mreturn
        with
        | ex -> throwError ex.Message

    /// Try to run a computation.
    /// On failure, recover or throw again with the handler.
    let attempt (action : GenerateMonad<'a, 'uenv>) 
                (handler : ErrMsg -> GenerateMonad<'a, 'uenv>) : GenerateMonad<'a, 'uenv> = 
        GenerateMonad <| fun env st ->
            match apply1 action env st with
            | Ok ans -> Ok ans
            | Error msg -> apply1 (handler msg) env st


    let assertM (cond : GenerateMonad<bool, 'uenv>) 
                (errMsg : string) : GenerateMonad<unit, 'uenv> = 
        generate {
            match! cond with
            | true -> return ()
            | false -> return! throwError errMsg
        }

    let assertBool (cond: bool) (errMsg: string) : GenerateMonad<unit, 'uenv> = 
        generate {
            match cond with
            | true -> return ()
            | false -> return! throwError errMsg
        }

    /// Run a potentially failing action. If it succeeds the answer
    /// is wrapped in ``Some``. 
    /// If it fails trap the error and return ``None``.
    let optional (action : GenerateMonad<'a, 'uenv>) : GenerateMonad<'a option, 'uenv> = 
        attempt (action |>> Some) (fun _ -> mreturn None)

    /// Run an optional action - if it returns ``Some a`` return the 
    /// answer. If it returns ``None`` the fail.
    let getOptional (action : GenerateMonad<'a option, 'uenv>) : GenerateMonad<'a, 'uenv> = 
        generate { 
            match! action with
            | Some a -> return a
            | None -> return! throwError "getOptional - None" 
        }




    let choice (actions : GenerateMonad<'a, 'uenv> list) : GenerateMonad<'a, 'uenv> = 
        GenerateMonad <| fun env state -> 
            /// State is outside worker as each application sees initial state
            let rec work acts cont = 
                match acts with 
                | [] -> Error "choice"
                | action1 :: rest ->
                    match apply1 action1 env state with
                    | Ok ans -> cont (Ok ans)
                    | Error _ -> work rest cont
            work actions (fun x -> x)

    // ************************************************************************
    // liftM2 etc

    // liftM (which is fmap)
    let liftM (fn : 'a -> 'ans) 
                (action : GenerateMonad<'a, 'uenv>) : GenerateMonad<'ans, 'uenv> = 
        fmapM fn action

    let liftM2 (combine : 'a -> 'b -> 'ans) 
                (action1 : GenerateMonad<'a, 'uenv>) 
                (action2 : GenerateMonad<'b, 'uenv>) : GenerateMonad<'ans, 'uenv> = 
        generate { 
            let! a = action1
            let! b = action2
            return (combine a b)
        }

    let liftM3 (combine : 'a -> 'b -> 'c -> 'ans) 
                (action1 : GenerateMonad<'a, 'uenv>) 
                (action2 : GenerateMonad<'b, 'uenv>) 
                (action3 : GenerateMonad<'c, 'uenv>) : GenerateMonad<'ans, 'uenv> = 
        generate { 
            let! a = action1
            let! b = action2
            let! c = action3
            return (combine a b c)
        }

    let liftM4 (combine : 'a -> 'b -> 'c -> 'd -> 'ans) 
                (action1 : GenerateMonad<'a, 'uenv>) 
                (action2 : GenerateMonad<'b, 'uenv>) 
                (action3 : GenerateMonad<'c, 'uenv>) 
                (action4 : GenerateMonad<'d, 'uenv>) : GenerateMonad<'ans, 'uenv> = 
        generate { 
            let! a = action1
            let! b = action2
            let! c = action3
            let! d = action4
            return (combine a b c d)
        }


    let liftM5 (combine : 'a -> 'b -> 'c -> 'd -> 'e -> 'ans) 
                (action1 : GenerateMonad<'a, 'uenv>) 
                (action2 : GenerateMonad<'b, 'uenv>) 
                (action3 : GenerateMonad<'c, 'uenv>) 
                (action4 : GenerateMonad<'d, 'uenv>) 
                (action5 : GenerateMonad<'e, 'uenv>) : GenerateMonad<'ans, 'uenv> = 
        generate { 
            let! a = action1
            let! b = action2
            let! c = action3
            let! d = action4
            let! e = action5
            return (combine a b c d e)
        }

    let liftM6 (combine : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'ans) 
                (action1 : GenerateMonad<'a, 'uenv>) 
                (action2 : GenerateMonad<'b, 'uenv>) 
                (action3 : GenerateMonad<'c, 'uenv>) 
                (action4 : GenerateMonad<'d, 'uenv>) 
                (action5 : GenerateMonad<'e, 'uenv>) 
                (action6 : GenerateMonad<'f, 'uenv>) : GenerateMonad<'ans, 'uenv> = 
        generate { 
            let! a = action1
            let! b = action2
            let! c = action3
            let! d = action4
            let! e = action5
            let! f = action6
            return (combine a b c d e f)
        }

    let tupleM2 (action1 : GenerateMonad<'a, 'uenv>) 
                (action2 : GenerateMonad<'b, 'uenv>) : GenerateMonad<'a * 'b, 'uenv> = 
        liftM2 (fun a b -> (a,b)) action1 action2

    let tupleM3 (action1 : GenerateMonad<'a, 'uenv>) 
                (action2 : GenerateMonad<'b, 'uenv>) 
                (action3 : GenerateMonad<'c, 'uenv>) : GenerateMonad<'a * 'b * 'c, 'uenv> = 
        liftM3 (fun a b c -> (a,b,c)) action1 action2 action3

    let tupleM4 (action1 : GenerateMonad<'a, 'uenv>) 
                (action2 : GenerateMonad<'b, 'uenv>) 
                (action3 : GenerateMonad<'c, 'uenv>) 
                (action4 : GenerateMonad<'d, 'uenv>) : GenerateMonad<'a * 'b * 'c * 'd, 'uenv> = 
        liftM4 (fun a b c d -> (a,b,c,d)) action1 action2 action3 action4

    let tupleM5 (action1 : GenerateMonad<'a, 'uenv>) 
                (action2 : GenerateMonad<'b, 'uenv>) 
                (action3 : GenerateMonad<'c, 'uenv>) 
                (action4 : GenerateMonad<'d, 'uenv>) 
                (action5 : GenerateMonad<'e, 'uenv>) : GenerateMonad<'a * 'b * 'c * 'd * 'e, 'uenv> = 
        liftM5 (fun a b c d e -> (a,b,c,d,e)) action1 action2 action3 action4 action5

    let tupleM6 (action1 : GenerateMonad<'a, 'uenv>) 
                (action2 : GenerateMonad<'b, 'uenv>) 
                (action3 : GenerateMonad<'c, 'uenv>) 
                (action4 : GenerateMonad<'d, 'uenv>) 
                (action5 : GenerateMonad<'e, 'uenv>) 
                (action6 : GenerateMonad<'f, 'uenv>) : GenerateMonad<'a * 'b * 'c * 'd * 'e * 'f, 'uenv> = 
        liftM6 (fun a b c d e f -> (a,b,c,d,e,f)) 
                action1 action2 action3 action4 action5 action6



    let pipeM2 (action1 : GenerateMonad<'a, 'uenv>) 
               (action2 : GenerateMonad<'b, 'uenv>) 
               (combine:'a -> 'b -> 'ans) : GenerateMonad<'ans, 'uenv> = 
        liftM2 combine action1 action2

    let pipeM3 (action1 : GenerateMonad<'a, 'uenv>) 
               (action2 : GenerateMonad<'b, 'uenv>) 
               (action3 : GenerateMonad<'c, 'uenv>) 
               (combine : 'a -> 'b -> 'c -> 'ans) : GenerateMonad<'ans, 'uenv> = 
        liftM3 combine action1 action2 action3

    let pipeM4 (action1 : GenerateMonad<'a, 'uenv>) 
               (action2 : GenerateMonad<'b, 'uenv>) 
               (action3 : GenerateMonad<'c, 'uenv>) 
               (action4 : GenerateMonad<'d, 'uenv>) 
               (combine : 'a -> 'b -> 'c -> 'd -> 'ans) : GenerateMonad<'ans, 'uenv> = 
        liftM4 combine action1 action2 action3 action4

    let pipeM5 (action1 : GenerateMonad<'a, 'uenv>) 
               (action2 : GenerateMonad<'b, 'uenv>) 
               (action3 : GenerateMonad<'c, 'uenv>) 
               (action4 : GenerateMonad<'d, 'uenv>) 
               (action5 : GenerateMonad<'e, 'uenv>) 
               (combine : 'a -> 'b -> 'c -> 'd -> 'e -> 'ans) : GenerateMonad<'ans, 'uenv> = 
        liftM5 combine action1 action2 action3 action4 action5

    let pipeM6 (action1 : GenerateMonad<'a, 'uenv>) 
               (action2 : GenerateMonad<'b, 'uenv>) 
               (action3 : GenerateMonad<'c, 'uenv>) 
               (action4 : GenerateMonad<'d, 'uenv>) 
               (action5 : GenerateMonad<'e, 'uenv>) 
               (action6 : GenerateMonad<'f, 'uenv>) 
               (combine : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'ans) : GenerateMonad<'ans, 'uenv> = 
        liftM6 combine action1 action2 action3 action4 action5 action6

    // ************************************************************************
    // Templates

    let evalTemplate (code : Template<'a>) : GenerateMonad<'a, 'uenv> = 
        GenerateMonad <| fun env st ->
            match runTemplate code env.TemplateEnv st.NameIndex with
            | Error msg -> Error msg
            | Ok (a, ix1) -> Ok(a, { st with NameIndex = ix1 })


    // ************************************************************************
    // Traversals

    /// Implemented in CPS 
    let mapM (mf: 'a -> GenerateMonad<'b, 'uenv>) 
             (source : 'a list) : GenerateMonad<'b list, 'uenv> = 
        GenerateMonad <| fun env state -> 
            let rec work (xs : 'a list)
                         (st: GenerateState)
                         (fk : ErrMsg -> Result<'b list * GenerateState, ErrMsg>) 
                         (sk : GenerateState -> 'b list -> Result<'b list * GenerateState, ErrMsg>) = 
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
             (fn : 'a -> GenerateMonad<'b, 'uenv>) : GenerateMonad<'b list, 'uenv> = 
        mapM fn xs


    /// Implemented in CPS 
    let mapMz (mf: 'a -> GenerateMonad<'b, 'uenv>) 
              (source : 'a list) : GenerateMonad<unit, 'uenv> = 
        GenerateMonad <| fun env state -> 
            let rec work (xs : 'a list)
                         (st : GenerateState)
                         (fk : ErrMsg -> Result<unit * GenerateState, ErrMsg>) 
                         (sk : GenerateState -> Result<unit * GenerateState, ErrMsg>) = 
                match xs with
                | [] -> sk st
                | x :: rest ->
                    match apply1 (mf x) env st with
                    | Error msg -> fk msg
                    | Ok (_, s1) -> work rest s1 fk sk
            work source state (fun msg -> Error msg) (fun st -> Ok ((), st))


    let forMz (xs : 'a list) 
              (fn : 'a -> GenerateMonad<'b, 'uenv>) : GenerateMonad<unit, 'uenv> = 
        mapMz fn xs

    let foldM (action : 'acc -> 'a -> GenerateMonad<'acc, 'uenv>) 
                (initial : 'acc)
                (source : 'a list) : GenerateMonad<'acc, 'uenv> = 
        GenerateMonad <| fun env state -> 
            let rec work (acc : 'acc) 
                            (xs : 'a list) 
                            (st : GenerateState)
                            (fk : ErrMsg -> Result<'acc * GenerateState, ErrMsg>) 
                            (sk : GenerateState -> 'acc -> Result<'acc * GenerateState, ErrMsg>) = 
                match xs with
                | [] -> sk st acc
                | x1 :: rest -> 
                    match apply1 (action acc x1) env st with
                    | Error msg -> fk msg
                    | Ok (acc1, s1) -> 
                        work acc1 rest s1 fk sk
            work initial source state (fun msg -> Error msg) (fun st a -> Ok (a, st))



    let smapM (action : 'a -> GenerateMonad<'b, 'uenv>) 
                (source : seq<'a>) : GenerateMonad<seq<'b>, 'uenv> = 
        GenerateMonad <| fun env state ->
            let sourceEnumerator = source.GetEnumerator()
            let rec work (st : GenerateState) 
                            (fk : ErrMsg -> Result<seq<'b> * GenerateState, ErrMsg>) 
                            (sk : GenerateState -> seq<'b> -> Result<seq<'b> * GenerateState, ErrMsg>) = 
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
              (fn : 'a -> GenerateMonad<'b, 'uenv>) : GenerateMonad<seq<'b>, 'uenv> = 
        smapM fn sx
    
    let smapMz (action : 'a -> GenerateMonad<'b, 'uenv>) 
                (source : seq<'a>) : GenerateMonad<unit, 'uenv> = 
        GenerateMonad <| fun env state ->
            let sourceEnumerator = source.GetEnumerator()
            let rec work (st : GenerateState) 
                            (fk : ErrMsg -> Result<unit * GenerateState, ErrMsg>) 
                            (sk : GenerateState -> Result<unit * GenerateState, ErrMsg>) = 
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
                (action : 'a -> GenerateMonad<'b, 'uenv>) : GenerateMonad<unit, 'uenv> = 
        smapMz action source

        
    let sfoldM (action : 'acc -> 'a -> GenerateMonad<'acc, 'uenv>) 
                (initial : 'acc)
                (source : seq<'a>) : GenerateMonad<'acc, 'uenv> = 
        GenerateMonad <| fun env state ->
            let sourceEnumerator = source.GetEnumerator()
            let rec work (acc : 'acc) 
                            (st : GenerateState)
                            (fk : ErrMsg -> Result<'acc * GenerateState, ErrMsg>) 
                            (sk : GenerateState -> 'acc -> Result<'acc * GenerateState, ErrMsg>) = 
                if not (sourceEnumerator.MoveNext()) then 
                    sk st acc
                else
                    let x = sourceEnumerator.Current
                    match apply1 (action acc x) env st with
                    | Error msg -> fk msg
                    | Ok (acc1, s1) -> work acc1 s1 fk sk
            work initial state (fun msg -> Error msg) (fun st a -> Ok (a, st))


    /// Implemented in CPS 
    let mapiM (mf : int -> 'a -> GenerateMonad<'b, 'uenv>) 
                (source : 'a list) : GenerateMonad<'b list, 'uenv> = 
        GenerateMonad <| fun env state -> 
            let rec work (xs : 'a list)
                         (count : int)
                         (st : GenerateState)
                         (fk : ErrMsg -> Result<'b list * GenerateState, ErrMsg>) 
                         (sk : GenerateState -> 'b list -> Result<'b list * GenerateState, ErrMsg>) = 
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
    let mapiMz (mf : int -> 'a -> GenerateMonad<'b, 'uenv>) 
                (source : 'a list) : GenerateMonad<unit, 'uenv> = 
        GenerateMonad <| fun env state -> 
            let rec work (xs : 'a list) 
                         (count : int)
                         (st : GenerateState)
                         (fk : ErrMsg -> Result<unit * GenerateState, ErrMsg>) 
                         (sk : GenerateState -> Result<unit * GenerateState, ErrMsg>) = 
                match xs with
                | [] -> sk st
                | y :: ys -> 
                    match apply1 (mf count y) env st with
                    | Error msg -> fk msg
                    | Ok (_, s1) -> work ys (count+1) s1 fk sk
            work source 0 state (fun msg -> Error msg) (fun st -> Ok ((), st))

    

    let foriM (xs : 'a list) 
              (fn : int -> 'a -> GenerateMonad<'b, 'uenv>) : GenerateMonad<'b list, 'uenv> = 
        mapiM fn xs

    let foriMz (xs : 'a list) 
               (fn : int -> 'a -> GenerateMonad<'b, 'uenv>) : GenerateMonad<unit, 'uenv> = 
        mapiMz fn xs


    /// Implemented in CPS 
    let filterM (mf: 'a -> GenerateMonad<bool, 'uenv>) 
                (source : 'a list) : GenerateMonad<'a list, 'uenv> = 
        GenerateMonad <| fun env state -> 
            let rec work (xs: 'a list)
                         (st: GenerateState)
                         (fk: ErrMsg -> Result<'a list * GenerateState, ErrMsg>) 
                         (sk: GenerateState -> 'a list -> Result<'a list * GenerateState, ErrMsg>) = 
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

    let unzipMapM (mf: 'a -> GenerateMonad<'b * 'c, 'uenv>) (source: 'a list): GenerateMonad<'b list * 'c list, 'uenv> = 
        generate {
            let! xs = mapM mf source
            return (List.map fst xs, List.map snd xs)
        }


    // ************************************************************************
    // Eval templates



    let applySite (template: Site) (siteCode: SiteCode): GenerateMonad<S4FunctionalLocation, 'uenv> = 
        evalTemplate (getSite template siteCode)
        
    let applyFunction (template: Function) (funcLoc: FuncLocPath): GenerateMonad<S4FunctionalLocation, 'uenv> = 
        evalTemplate (getFunction template funcLoc)

    let applyProcessGroup (template: ProcessGroup) (funcLoc: FuncLocPath): GenerateMonad<S4FunctionalLocation, 'uenv> = 
        evalTemplate (getProcessGroup template funcLoc)
        
    let applyProcess (template: Process) (funcLoc: FuncLocPath): GenerateMonad<S4FunctionalLocation, 'uenv> = 
        evalTemplate (getProcess template funcLoc)


    let applySystem (template: System) (funcLoc: FuncLocPath): GenerateMonad<S4FunctionalLocation, 'uenv> = 
        evalTemplate (getSystem template funcLoc)

    let applyAssembly (template: Assembly) (funcLoc: FuncLocPath): GenerateMonad<S4FunctionalLocation, 'uenv> = 
        evalTemplate (getAssembly template funcLoc)

    let applyItem (template: Item) (funcLoc: FuncLocPath): GenerateMonad<S4FunctionalLocation, 'uenv> = 
        evalTemplate (getItem template funcLoc)

    let applyComponent (template: Component) (funcLoc: FuncLocPath): GenerateMonad<S4FunctionalLocation, 'uenv> = 
        evalTemplate (getComponent template funcLoc)
        

    let applyEquipment (template: Equipment) 
                        (parent: EquipmentId option)  
                        (funcLoc: FuncLocPath): GenerateMonad<S4Equipment, 'uenv> = 
        evalTemplate (getEquipment template parent funcLoc)

    let applyFlocClass (template: FlocClass) 
                        (funcLoc: FuncLocPath): GenerateMonad<S4FlocClassification list , 'uenv> = 
            evalTemplate (getFlocClass template funcLoc)

    let applyEquiClass (template: EquiClass) 
                        (equiId: EquipmentId): GenerateMonad<S4EquiClassification list , 'uenv> = 
            evalTemplate (getEquiClass template equiId)


    let applyFlocCharacteristic (template: FlocCharacteristic)
                                (funcLoc: FuncLocPath)
                                (className: ClassName) : GenerateMonad<S4FlocClassification, 'uenv> = 
        evalTemplate (getFlocCharacteristic template funcLoc className)


    let applyEquiCharacteristic (template: EquiCharacteristic)
                                (equiId: EquipmentId)
                                (className: ClassName) : GenerateMonad<S4EquiClassification, 'uenv> = 
        evalTemplate (getEquiCharacteristic template equiId className)

