// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Anaylsis.Utilities


module TidyChangeFile = 
    
    open AssetPatch.Base.Common
    open AssetPatch.Base.Aiw.ChangeFileParser
    open AssetPatch.Base.Aiw.ChangeFilePrinter
    open AssetPatch.Analysis

    let tidyChangeFile (priorities : string list) 
                        (removes : string list)
                        (sourceFile: string)
                        (destFile : string) : Result<unit, ErrMsg> =
        let transform = 
            AbsChangeFile.ofChangeFile 
                >> AbsChangeFile.prioritize priorities 
                >> AbsChangeFile.restrict removes
                >> AbsChangeFile.toChangeFile
        try 
            readAiwChangeFile sourceFile
                |> Result.bind transform
                |> Result.map (fun changes -> writeAiwChangeFile changes destFile)
        with
        | ex -> Error (ex.Message)
    