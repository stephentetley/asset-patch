﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace OutstationPatcher

module InputData =

    open FSharp.Interop.Excel
    
    open AssetPatch.TemplatePatcher.CompilerMonad
    open AssetPatch.Lib.Common

    [<Literal>]
    let PROVIDERSOURCE = __SOURCE_DIRECTORY__ + @"\..\excel-sample\OS_Worklist_Sample.xlsx"
    
    type WorkListTable = 
        ExcelFile< FileName = PROVIDERSOURCE,
                    SheetName = "Work_List",
                    ForceString = true >

    type WorkListRow = WorkListTable.Row

    


    let readWorkList (xlsxPath : string) : CompilerMonad<WorkListRow list> =
        let action () = 
            let source = (new WorkListTable(filename = xlsxPath)).Data
            source
                |> Seq.choose (fun (row : WorkListRow) -> 
                                if notBlank row.``S4 Root FuncLoc`` then Some row else None)
                |> Seq.toList
        liftAction action