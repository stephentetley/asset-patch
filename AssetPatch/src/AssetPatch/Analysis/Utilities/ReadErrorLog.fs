// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.Anaylsis.Utilities


module ReadErrorLog = 
    
    open System.Text.RegularExpressions

    type Validation<'t> = 
        private | Okay of 't
                | Errs of string list



    let validate (va: Validation<'t>): Result<'t, string> = 
        match va with
        | Okay(a) -> Ok a
        | Errs(msgs) -> String.concat System.Environment.NewLine msgs |> Error


    type Matcher<'t> = 
        | Matcher of (string -> Validation<'t>)

    let private apply1 (ma: Matcher<'t>) (input: string): Validation<'t> = 
        let (Matcher f) = ma in f input

    let mreturn(x: 't): Matcher<'t> = 
        Matcher <| fun _ -> Okay x

    let merror(msg: string): Matcher<'t> = 
        Matcher <| fun _ -> Errs [msg]

    let fmap (f: 't -> 't1) (va: Matcher<'t>): Matcher<'t1> = 
        Matcher <| fun input -> 
            match apply1 va input with
            | Okay a  -> Okay (f a)
            | Errs xs -> Errs xs

    let ap (mf: Matcher<'t -> 't1>) (ma: Matcher<'t>): Matcher<'t1> =
        Matcher <| fun input -> 
            match apply1 mf input with
            | Okay f  -> apply1 (fmap f ma) input
            | Errs xs -> 
                match apply1 ma input with
                | Okay _  -> Errs xs
                | Errs ys -> Errs (xs @ ys)

    let alt (ma: Matcher<'t>) (mb: Matcher<'t>): Matcher<'t> = 
        Matcher <| fun input ->
            match apply1 ma input with
            | Okay a -> Okay a
            | Errs xs -> 
                match apply1 mb input with
                | Okay b  -> Okay b 
                | Errs ys -> Errs (xs @ ys)
 

    let withDefault (x: 't) (ma: Matcher<'t>): Matcher<'t> = 
        Matcher <| fun input ->
            match apply1 ma input with
            | Okay a -> Okay a
            | Errs _ -> Okay x

    let optional (ma: Matcher<'t>): Matcher<'t option> = 
        Matcher <| fun input ->
            match apply1 ma input with
            | Okay a -> Okay (Some a)
            | Errs _ -> Okay None

    let liftM2 (f: 't1 -> 't2 -> 't3) (m1: Matcher<'t1>) (m2: Matcher<'t2>): Matcher<'t3> = 
        ap (fmap f m1) m2

    let liftM3 (f: 't1 -> 't2 -> 't3 -> 't4) (m1: Matcher<'t1>) (m2: Matcher<'t2>) (m3: Matcher<'t3>): Matcher<'t4> = 
        ap (liftM2 f m1 m2) m3

    let liftM4 (f: 't1 -> 't2 -> 't3 -> 't4 -> 't5) (m1: Matcher<'t1>) (m2: Matcher<'t2>) (m3: Matcher<'t3>) (m4: Matcher<'t4>): Matcher<'t5> = 
        ap (liftM3 f m1 m2 m3) m4
    
    let liftM5 (f: 't1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6) (m1: Matcher<'t1>) (m2: Matcher<'t2>) (m3: Matcher<'t3>) (m4: Matcher<'t4>) (m5: Matcher<'t5>): Matcher<'t6> = 
        ap (liftM4 f m1 m2 m3 m4) m5

    let liftM6 (f: 't1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7) (m1: Matcher<'t1>) (m2: Matcher<'t2>) (m3: Matcher<'t3>) (m4: Matcher<'t4>) (m5: Matcher<'t5>) (m6: Matcher<'t6>): Matcher<'t7> = 
        ap (liftM5 f m1 m2 m3 m4 m5) m6
    
    let liftM7 (f: 't1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8) (m1: Matcher<'t1>) (m2: Matcher<'t2>) (m3: Matcher<'t3>) (m4: Matcher<'t4>) (m5: Matcher<'t5>) (m6: Matcher<'t6>) (m7: Matcher<'t7>): Matcher<'t8> = 
        ap (liftM6 f m1 m2 m3 m4 m5 m6) m7
    
    let liftM8 (f: 't1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9) (m1: Matcher<'t1>) (m2: Matcher<'t2>) (m3: Matcher<'t3>) (m4: Matcher<'t4>) (m5: Matcher<'t5>) (m6: Matcher<'t6>) (m7: Matcher<'t7>) (m8: Matcher<'t8>): Matcher<'t9> = 
        ap (liftM7 f m1 m2 m3 m4 m5 m6 m7) m8

    let liftM9(f: 't1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10) (m1: Matcher<'t1>) (m2: Matcher<'t2>) (m3: Matcher<'t3>) (m4: Matcher<'t4>) (m5: Matcher<'t5>) (m6: Matcher<'t6>) (m7: Matcher<'t7>) (m8: Matcher<'t8>) (m9: Matcher<'t9>): Matcher<'t10> = 
        ap (liftM8 f m1 m2 m3 m4 m5 m6 m7 m8) m9
    
    let liftM10(f: 't1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 't11) (m1: Matcher<'t1>) (m2: Matcher<'t2>) (m3: Matcher<'t3>) (m4: Matcher<'t4>) (m5: Matcher<'t5>) (m6: Matcher<'t6>) (m7: Matcher<'t7>) (m8: Matcher<'t8>) (m9: Matcher<'t9>) (m10: Matcher<'t10>): Matcher<'t11> = 
        ap (liftM9 f m1 m2 m3 m4 m5 m6 m7 m8 m9) m10

    let pipeV2 (m1: Matcher<'t1>) (m2: Matcher<'t2>) (f: 't1 -> 't2 -> 't3): Matcher<'t3> =
        liftM2 f m1 m2

    let pipeV3 (m1: Matcher<'t1>) (m2: Matcher<'t2>) (m3: Matcher<'t3>) (f: 't1 -> 't2 -> 't3 -> 't4): Matcher<'t4> =
        liftM3 f m1 m2 m3
    
    let pipeV4 (m1: Matcher<'t1>) (m2: Matcher<'t2>) (m3: Matcher<'t3>) (m4: Matcher<'t4>) (f: 't1 -> 't2 -> 't3 -> 't4 -> 't5): Matcher<'t5> = 
        liftM4 f m1 m2 m3 m4

    let pipeV5 (m1: Matcher<'t1>) (m2: Matcher<'t2>) (m3: Matcher<'t3>) (m4: Matcher<'t4>) (m5: Matcher<'t5>) (f: 't1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6): Matcher<'t6> = 
        liftM5 f m1 m2 m3 m4 m5

    let pipeV6 (m1: Matcher<'t1>) (m2: Matcher<'t2>) (m3: Matcher<'t3>) (m4: Matcher<'t4>) (m5: Matcher<'t5>) (m6: Matcher<'t6>) (f: 't1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7): Matcher<'t7> = 
        liftM6 f m1 m2 m3 m4 m5 m6

    let pipeV7 (m1: Matcher<'t1>) (m2: Matcher<'t2>) (m3: Matcher<'t3>) (m4: Matcher<'t4>) (m5: Matcher<'t5>) (m6: Matcher<'t6>) (m7: Matcher<'t7>) (f: 't1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8): Matcher<'t8> = 
        liftM7 f m1 m2 m3 m4 m5 m6 m7

    let pipeV8 (m1: Matcher<'t1>) (m2: Matcher<'t2>) (m3: Matcher<'t3>) (m4: Matcher<'t4>) (m5: Matcher<'t5>) (m6: Matcher<'t6>) (m7: Matcher<'t7>) (m8: Matcher<'t8>) (f: 't1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9): Matcher<'t9> = 
        liftM8 f m1 m2 m3 m4 m5 m6 m7 m8

    let pipeV9 (m1: Matcher<'t1>) (m2: Matcher<'t2>) (m3: Matcher<'t3>) (m4: Matcher<'t4>) (m5: Matcher<'t5>) (m6: Matcher<'t6>) (m7: Matcher<'t7>) (m8: Matcher<'t8>) (m9: Matcher<'t9>) (f: 't1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10): Matcher<'t10> = 
        liftM9 f m1 m2 m3 m4 m5 m6 m7 m8 m9

    let pipeV10 (m1: Matcher<'t1>) (m2: Matcher<'t2>) (m3: Matcher<'t3>) (m4: Matcher<'t4>) (m5: Matcher<'t5>) (m6: Matcher<'t6>) (m7: Matcher<'t7>) (m8: Matcher<'t8>) (m9: Matcher<'t9>) (m10: Matcher<'t10>) (f: 't1 -> 't2 -> 't3 -> 't4 -> 't5 -> 't6 -> 't7 -> 't8 -> 't9 -> 't10 -> 't11): Matcher<'t11> = 
        liftM10 f m1 m2 m3 m4 m5 m6 m7 m8 m9 m10



    let matchValue(pattern: string): Matcher<string> = 
        Matcher <| fun input -> 
            let answer = Regex.Match(input= input, pattern = pattern)
            if answer.Success then
                Okay answer.Value
            else
                Errs ["no match"]

    let runMatcher (ma: Matcher<'t>) (input: string): Result<'t, string> = 
        let (Matcher f) = ma 
        f input |> validate

    let choose (ma: Matcher<'t option>) (input: string []) : Result<'t list, string> = 
        let failk = fun n body -> Error (sprintf "Failure at line %i: \n%s" n body)
        let rec work (i: int) fk sk = 
            if i >= input.Length then
                sk []
            else 
                let row = input.[i]
                match validate(apply1 ma row) with
                | Error body -> fk (i+1) body
                | Ok None -> work (i+1) fk (fun vs -> sk vs)
                | Ok (Some v1) -> work (i+1) fk (fun vs -> sk (v1 :: vs))
        work 0 failk (fun xs -> Ok xs)

