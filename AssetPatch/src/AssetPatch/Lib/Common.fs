// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Lib


module Common = 

    open System
    open System.Text.RegularExpressions

    // ************************************************************************
    // Read cell functions

    let notBlank (s : string) : bool = 
        match s with
        | null | "" -> false
        | _ -> true


    let tryGetInt (source : string) : int option = 
        try
            int source |> Some
        with
        | _ -> None

    let tryGetDecimal (source : string) : decimal option = 
        try
            decimal source |> Some
        with
        | _ -> None

    let tryGetString (source : string) : string option = 
        match source with
        | null -> None
        | _ -> Some source

    let tryGetNonBlank (source : string) : string option = 
        if String.IsNullOrWhiteSpace source then None else Some source


    /// Note input string might have hh:mm:ss suffix. 
    /// So take first 10 characters.
    let tryGetUSDate (source : string) : DateTime option =
        try
            match DateTime.TryParseExact( s = source.Substring(startIndex=0, length=10)
                                        , format = "MM/dd/yyyy"
                                        , provider = Globalization.CultureInfo.InvariantCulture
                                        , style = Globalization.DateTimeStyles.None) with
            | true, date -> Some date
            | false, _ -> None
        with
        | _ -> None


    /// Note input string might have hh:mm:ss suffix. 
    /// So take first 10 characters.
    let getUSDate (source : string) : DateTime  =
        match DateTime.TryParseExact( s = source.Substring(startIndex=0, length=10)
                                    , format = "MM/dd/yyyy"
                                    , provider = Globalization.CultureInfo.InvariantCulture
                                    , style = Globalization.DateTimeStyles.None) with
        | true, date -> date
        | false, _ -> failwith "getUSDate"

    let getAIMonth(src: string): int = 
        match src.Trim().ToUpper() with
        | "JAN" -> 1
        | "FEB" -> 2
        | "MAR" -> 3
        | "APR" -> 4
        | "MAY" -> 5
        | "JUN" -> 6
        | "JUL" -> 7
        | "AUG" -> 8
        | "SEP" -> 9
        | "OCT" -> 10
        | "NOV" -> 11
        | "DEC" -> 12
        | _ -> 0


    /// Of form "Oct 02 2019" i.e Three letter month, two digit day, four digit year  
    let getAiDate(s: string): DateTime option = 
        let patt = "^(?<mon>[A-Z][a-z]{2}) (?<day>[0-9]{2}) (?<year>[0-9]{4})"
        let m = Regex.Match(input = s, pattern = patt)
        if m.Success then
            let day = m.Groups.["day"].Value |> int
            let mon = m.Groups.["mon"].Value
            let year = m.Groups.["year"].Value |> int
            DateTime(year = year, month = 1, day = day) |> Some
        else 
            None
        