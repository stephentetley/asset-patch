// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base



module ValuaValue =
    
    open System
   
    type ValuaValue = 
        | NullValue
        | IntValue of bigint
        | DecimalValue of decimal
        | TextValue of string
        | DateValue of DateTime
        
        /// This prints a value for all cases
        member x.DescriptionValue 
            with get(): string = 
                match x with
                | NullValue -> ""
                | IntValue i -> i.ToString()
                | DecimalValue d -> d.ToString()
                | TextValue s -> s
                | DateValue dt -> dt.ToString(format="dd.MM.yyyy")


        member x.CharacteristicValue 
            with get(): string option = 
                match x with
                | NullValue -> None
                | IntValue _ -> None
                | DecimalValue _ -> None
                | TextValue s -> Some s
                | DateValue _ -> None


        member x.NumericValueFrom
            with get(): string option = 
                match x with
                | NullValue -> None
                | IntValue i -> i.ToString() |> Some
                | DecimalValue d -> d.ToString() |> Some
                | TextValue _ -> None
                | DateValue dt -> dt.ToString(format = "yyyyMMdd") |> Some

        member x.NumericValueTo
            with get(): string option = 
                match x with
                | NullValue -> None
                | IntValue _ -> Some "0"
                | DecimalValue _ -> Some "0"
                | TextValue _ -> None
                | DateValue _ -> Some "0"

        member x.UxlValue
            with get(): string = 
                match x with
                | NullValue -> ""
                | IntValue i -> i.ToString()
                | DecimalValue d -> d.ToString()
                | TextValue s -> s
                | DateValue dt -> dt.ToString(format="dd.MM.yyyy")

    let intValue (i : int) : ValuaValue = 
        IntValue (bigint i)

    let int32Value (i : int32) : ValuaValue = 
        IntValue (bigint i)

    let uint32Value (i : uint32) : ValuaValue = 
        IntValue (bigint i)

    let textUpperCase (s : string) : ValuaValue = 
        TextValue <| s.ToUpper().Trim()

    // Should either see TextValue or Null value.
    let uxlStringValue(v: ValuaValue): string = 
        match v with
        | NullValue -> ""
        | TextValue s -> s
        | _ -> ""

    // Should either see DateValue or Null value.
    let uxlDataTimeOptionValue(v: ValuaValue): DateTime option = 
        match v with
        | NullValue -> None
        | DateValue dt -> Some dt
        | _ -> None
    
    // Should either see IntValue or Null value.
    let uxlIntOptionValue(v: ValuaValue): int option = 
        match v with
        | NullValue -> None
        | IntValue i -> Some (int i)
        | _ -> None



    // ************************************************************************
    // Other helpers

    let dateDefault : DateTime = 
        new DateTime(year = 1970, month = 1, day = 1)
       
