// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause


namespace DisposeEquiPatcher

module InputData =
    
    open FSharp.Interop.Excel

    open AssetPatch.Lib.Common


    [<Literal>]
    let PROVIDERSOURCE = __SOURCE_DIRECTORY__ + @"\..\excel-sample\Dispose_Equi_Worklist_Sample.xlsx"

    type WorkListTable = 
        ExcelFile< FileName = PROVIDERSOURCE,
                       SheetName = "Work_List",
                       ForceString = true >

    type WorkListRow = WorkListTable.Row


    let readWorkList (xlsxPath : string) : WorkListRow list =
        let source = (new WorkListTable(filename = xlsxPath)).Data
        source
            |> Seq.choose (fun (row : WorkListRow) -> 
                            if notBlank row.``S4 Equipment Id`` then Some row else None)
            |> Seq.toList


    