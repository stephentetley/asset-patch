// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Uxl


module PatchCompiler =
    
    open System.IO

    open AssetPatch.Base.Common
    open AssetPatch.Base.Uxl.FileTypes
    open AssetPatch.TemplatePatcher.Base.CompilerMonad
    open AssetPatch.TemplatePatcher.Uxl.Base
    open AssetPatch.TemplatePatcher.Uxl.Emitter

    // ************************************************************************/
    // Gen file name

    let private genFileName (directory : string)
                            (filePrefix : string) 
                            (namePart : string): UxlCompilerMonad<string> = 
        compile {
            let name1 = sprintf "%s_%s.csv" (safeName filePrefix) (safeName namePart)
            return Path.Combine(directory, name1)
        }    

    // ************************************************************************
    // Write output


    let writeMmopCreateData (directory : string) 
                        (filePrefix : string) 
                        (source : MmopCreateData) : UxlCompilerMonad<unit> =         
        if source.IsEmpty then
            mreturn ()
        else            
            let mmopData = source.RemoveDups()
            compile {
                let! outPath01 = genFileName directory filePrefix "01_change_request_details_tab"
                do! liftResult <| writeChangeRequestDetails mmopData.ChangeRequests outPath01
                
                // FuncLoccs
                let! outPath02 = genFileName directory filePrefix "02_function_location_data_tab"
                do! liftResult <| writeFunctionalLocationData mmopData.NewFuncLocs outPath02
                let! outPath03 = genFileName directory filePrefix "03_fl_mulitlingual_text_tab"
                do!liftResult <|  writeFlocMultilingualText mmopData.NewFlocMultilingualTexts outPath03
                let! outPath04 = genFileName directory filePrefix "04_fl_classification_tab"
                do! liftResult <| writeFlocClassification mmopData.NewFlocClassifications outPath04

                // Equipment
                let! outPath05 = genFileName directory filePrefix "05_equipment_data_tab"
                do! liftResult <| writeEquipmentData mmopData.NewEquipments outPath05
                let! outPath06 = genFileName directory filePrefix "06_eq_mulitlingual_text_tab"
                do! liftResult <| writeEquiMultilingualText mmopData.NewEquiMultilingualTexts outPath06
                let! outPath07 = genFileName directory filePrefix "07_eq_classification_tab"
                do! liftResult <| writeEquiClassification mmopData.NewEquiClassifications outPath07
                return ()
            }
