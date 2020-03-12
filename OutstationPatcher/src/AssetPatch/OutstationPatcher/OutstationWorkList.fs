// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace AssetPatch.OutstationPatcher

module OutstationWorkList =

    open FSharp.Data

    open AssetPatch.Lib.Common


    [<Literal>]
    let WorkListSchema = 
        "AI Site Name (string)," +
        "S4 Root Funcloc (string)," +
        "NGR (string)," +
        "CAA SAI Reference [Control Services] (string)," +
        "NET AIB Reference [usually Control Services] (string)," +
        "TEL AIB Reference [RTS Monitoring] (string)," +
        "SYS AIB Reference [usually RTS Monitoring] (string)," +
        "Equi SAI Number (string)," +
        "Equi PLI Number (string)," +
        "System Code (string)," +
        "System Name(string)," +
        "Description [S4 display name] (string)," +
        "Manufacturer (string)," +
        "Model (string)," +
        "Serial Number (string)," +
        "Installed From Date (string)," +
        "Memo Line (string)," +
        "Modem Name (string)," +
        "Modem Manufacturer (string)," +
        "Modem Model (string)," +
        "Modem Serial Number (string)," +
        "Modem Install From Date (string)"

    [<Literal>]
    let WorkListSample = 
        "some name," +
        "CODE01," +
        "TA1000520005," +
        "CODE10," +
        "CODE10," +
        "CODE11," +
        "CODE11," +
        "CODE12," +
        "PLI12," +
        "SYS01," +
        "X System," +
        "X Object," +
        "make," +
        "model," +
        "XYZ/001," +
        "Feb 02 2020," +
        "comment," +
        "modem," +
        "modem make," +
        "modem model," +
        "MOD/123," +
        "Feb 02 2020"

    type WorkListTable = 
         CsvProvider< Sample = WorkListSample,
                      Schema = WorkListSchema,
                      HasHeaders = true >

     type WorkListRow = WorkListTable.Row


    let readWorkList (csvpath : string) : WorkListRow list =
        WorkListTable.Load(uri = csvpath).Rows
            |> Seq.toList


