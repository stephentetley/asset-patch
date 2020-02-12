// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.Analysis.Utilities

module CsvExport = 


    open AssetPatch.Base
    open AssetPatch.Base.CsvFile
    open AssetPatch.Base.Aiw.ChangeFile
    open AssetPatch.Base.Aiw.Acronyms
    

  
    let private translateHeaders (entityType: EntityType) (codes: string []): string [] = 
        let decode1 (code: string) = 
            match decodeAcronym entityType code with
            | None -> code
            | Some desc -> sprintf "%s (%s)" desc code
        Array.map decode1 codes

    let exportCsv (changeFile: AiwChangeFile) (outputPath: string) : Unit = 
        let assocs = changeFile.RowAssocs ()
        match List.tryHead assocs with
        | None -> ()
        | Some a ->             
            let headers = AssocList.keys a |> translateHeaders changeFile.Header.EntityType
            let rows = List.map AssocList.values assocs
            writeCsvFile csvDefaults { Headers = headers; Rows = rows } outputPath
