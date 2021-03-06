﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base


module Common = 
    
    open System
    open System.IO
    open FSharp.Core



    type ErrMsg = string    

    let safeName (input:string) : string =
        let replace (problems : char list) (subst : string) (s : string) : string = 
            List.fold (fun (s:string) (c:char) -> s.Replace(c.ToString(), subst)) s problems
        let parens = ['('; ')'; '['; ']'; '{'; '}']
        let bads = ['\\'; '/'; ':'; '?'; '*'] 
        let white = ['\n'; '\t']
        input 
            |> replace parens ""
            |> replace bads "_"
            |> replace white "_"
            |> fun x -> x.Trim() 

    let makeOutputDirectory (dirName : string) : unit = 
        if not <| Directory.Exists(dirName) then
            Directory.CreateDirectory(dirName) |> ignore
        else ()

    let showS4Bool (source : bool) : string = 
        if source then "X" else ""

    let showS4Date (date : DateTime) : string = 
        date.ToString(format = "dd.MM.yyyy")

    let optionalS4Date (source : DateTime option) : string = 
        Option.defaultValue "" <| Option.map showS4Date source

    let readS4Date (source : string) : DateTime option = 
        try 
            DateTime.ParseExact(s = source, format = "dd.MM.yyyy", provider = Globalization.CultureInfo.InvariantCulture) 
                |> Some
        with
        |_ -> None

    let unknownIfBlank (source: string): string = 
        if String.IsNullOrWhiteSpace source then "UNKNOWN" else source
        
           

        
