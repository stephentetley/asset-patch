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


    // FlocCreateData

    let private writeFlocCreateData1 (directory : string) 
                                    (filePrefix : string) 
                                    (level: int) 
                                    (source : FlocCreateData) : AiwGenerate<unit> =  
        generate {
            let src = source.GetLevel level

            // FuncLocs
            let! outpath01 = genFileName directory filePrefix (sprintf "level%i_step1_create_flocs" level)
            do! writeNewFuncLocs src.NewFuncLocs outpath01
           
            // Classes
            let! outpath02 = genFileName directory filePrefix (sprintf "level%i_step2_classes" level)
            do! writeNewClassFlocs src.NewFlocClasses outpath02
        
            // Characteristics
            let! outpath03 = genFileName directory filePrefix (sprintf "level%i_step3_characteristics" level)
            do! writeNewValuaFlocs src.NewFlocCharacteristics outpath03
            return ()
        }


    let writeFlocCreateData (directory : string) 
                            (filePrefix : string) 
                            (source : FlocCreateData) : AiwGenerate<unit> =         
        if source.IsEmpty then
            mreturn ()
        else            
            generate {
                let source1 = source.RemoveDups()
                let! directory1 = createSubfolder directory "01_create_flocs"
                do! forMz [1..8] (fun i -> writeFlocCreateData1 directory1 filePrefix i source1) 
                return ()
            }

    // EquiCreateData

    let private writeEquiCreateData1 (directory : string) 
                                        (filePrefix : string) 
                                        (source : EquiCreateData) : AiwGenerate<unit> =  
        generate {
                
        
            // Equi
            let! outpath01 = genFileName directory filePrefix "create_equi"
            do! writeNewEquis source.NewEquipment outpath01

            return()
        }

    let writeEquiCreateData (directory : string) 
                            (filePrefix : string) 
                            (source : EquiCreateData) : AiwGenerate<unit> =         
        if source.IsEmpty then
            mreturn ()
        else            
            let source1 = source.RemoveDups()
            generate {
                let! directory1 = createSubfolder directory "02_create_equi"
                do! writeEquiCreateData1 directory1 filePrefix source1
                return ()
            }

    // EquiCreateClassifactions

    let private writeEquiCreateClassifactions1 (directory : string) 
                                                (filePrefix : string) 
                                                (source : EquiCreateClassifications) : AiwGenerate<unit> =  
        generate {
                
        
            // Classes
            let! outpath01 = genFileName directory filePrefix "step1_classes"
            do! writeNewClassEquis source.NewEquiClasses outpath01

            // characteristics
            let! outpath02 = genFileName directory filePrefix "step2_characteristics"
            do! writeNewValuaEquis source.NewEquiCharacteristics outpath02

            // Multilingual texts
            let! outpath03 = genFileName directory filePrefix "step3_multilingual_texts"
            do! writeNewEqmltxts source.NewEquiMultilingualTexts outpath03

            return()
        }

    let writeEquiCreateClassifactions (directory : string) 
                                        (filePrefix : string) 
                                        (source : EquiCreateClassifications) : AiwGenerate<unit> =         
        if source.IsEmpty then
            mreturn ()
        else            
            let source1 = source.RemoveDups()
            generate {
                let! directory1 = createSubfolder directory "03_equi_classifications"
                do! writeEquiCreateClassifactions1 directory1 filePrefix source1
                return ()
            }