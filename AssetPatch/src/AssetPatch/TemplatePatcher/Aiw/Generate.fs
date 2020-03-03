// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Aiw


module Generate =
    
    open System.IO

    open AssetPatch.Base.Common
    open AssetPatch.TemplatePatcher.Base.GenerateMonad
    open AssetPatch.TemplatePatcher.Aiw.Base
    open AssetPatch.TemplatePatcher.Aiw.FileTypes
    open AssetPatch.TemplatePatcher.Aiw.Emitter


    // ************************************************************************/
    // Gen file name

    let private genFileName (directory : string)
                            (filePrefix : string) 
                            (namePart : string): AiwGenerate<string> = 
        generate {
            let name1 = sprintf "%s_%s.txt" (safeName filePrefix) (safeName namePart)
            return Path.Combine(directory, name1)
        }  


    let private createSubfolder (parentDirectory : string)
                                (subfolder : string) : AiwGenerate<string> = 
        let fullpath = Path.Combine(parentDirectory, subfolder)
        let createFolder _ = 
            Directory.CreateDirectory(fullpath) |> ignore
            fullpath
            
        if Directory.Exists(fullpath) then
            mreturn fullpath
        else
            liftAction <| createFolder
        
        
    // ************************************************************************
    // Write output

    let private writeFlocCreateData1 (directory : string) 
                                    (filePrefix : string) 
                                    (level: int) 
                                    (source : FlocCreateData) : AiwGenerate<unit> =  
        generate {
            let src = source.GetLevel level

            // FuncLocs
            let! outpath01 = genFileName directory filePrefix (sprintf "level_%i_create_flocs" level)
            do! writeNewFuncLocs src.NewFuncLocs outpath01

            // Multilingual text
            //let! outpath02 = genFileName directory filePrefix (sprintf "level_%i_multilingual_text" level)
            //do!liftResult <|  writeFlocMultilingualText mmopData.NewFlocMultilingualTexts outPath03

            // Classes
            //let! outpath03 = genFileName directory filePrefix (sprintf "level_%i_classes" level)
            //do! liftResult <| writeFlocClassification mmopData.NewFlocClassifications outPath04
        
            // Characteristics
            //let! outpath04 = genFileName directory filePrefix (sprintf "level_%i_characteristics" level)

            return ()
        }


    let writeFlocCreateData (directory : string) 
                            (filePrefix : string) 
                            (source : FlocCreateData) : AiwGenerate<unit> =         
        if source.IsEmpty then
            mreturn ()
        else            
            let mmopData = source.RemoveDups()
            generate {
                let! directory1 = createSubfolder directory "01_create_flocs"
                do! forMz [1..8] (fun i -> writeFlocCreateData1 directory1 filePrefix i source) 
                return ()
            }