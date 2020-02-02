// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Uxl


module PatchCompiler =
    
    open System.IO

    open AssetPatch.Base.Common
    open AssetPatch.TemplatePatcher.Base.CompilerMonad
    open AssetPatch.TemplatePatcher.Uxl.Base
    open AssetPatch.TemplatePatcher.Uxl.PatchWriter
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
                        (mmopData : MmopCreateData) : UxlCompilerMonad<unit> = 
        
        if mmopData.IsEmpty then
            mreturn ()
        else            
            compile {
                let! outPath01 = genFileName directory filePrefix "01_change_request_details_tab"
                do! writeNewChangeRequestsFile mmopData.ChangeRequests outPath01
                let! outPath02 = genFileName directory filePrefix "02_function_location_data_tab"
                do! writeNewFuncLocsFile mmopData.NewFuncLocs outPath02
 
                return ()
            }
