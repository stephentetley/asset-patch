// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Aiw

module PatchCompiler =
    
    open System.IO

    open AssetPatch.Base.Common
    open AssetPatch.TemplatePatcher.Base.CompilerMonad
    open AssetPatch.TemplatePatcher.Aiw.Base
    open AssetPatch.TemplatePatcher.Aiw.PatchTypes
    open AssetPatch.TemplatePatcher.Aiw.PatchWriter
    open AssetPatch.TemplatePatcher.Aiw.EmitPhase1
    open AssetPatch.TemplatePatcher.Aiw.EmitPhase2
    open AssetPatch.TemplatePatcher.Aiw.EmitNewAttributes




    // ************************************************************************/
    // Gen file name

    let private genFileName (directory : string) 
                            (filePrefix : string) 
                            (namePart : string) : AiwCompilerMonad<string> = 
        compile {
            let name1 = sprintf "%s_%s.txt" (safeName filePrefix) (safeName namePart)
            return Path.Combine(directory, name1)
        }    


    


    // ************************************************************************
    // Write output


    let writePhase1FlocData (directory : string) 
                            (filePrefix : string) 
                            (funcLocResults : Phase1FlocData) : AiwCompilerMonad<unit> = 
        
        if funcLocResults.IsEmpty then
            mreturn ()
        else            
            compile {
                let! outPath01 = genFileName directory filePrefix "01_create_flocs"
                do! writeNewFuncLocsFile funcLocResults.FuncLocs outPath01
                let! outPath02 = genFileName directory filePrefix "02_link_flocs"
                do! writeLinkFuncLocsFile funcLocResults.FuncLocLinks outPath02
                let! outPath03 = genFileName directory filePrefix "03_create_classflocs"
                do! writeNewClassFlocsFile funcLocResults.ClassFlocs outPath03
                let! outPath04 = genFileName directory filePrefix "04_create_valuaflocs"
                do! writeNewValuaFlocsFile funcLocResults.ValuaFlocs outPath04
                return ()
            }

   
    
    // Write an Equi patch file
    // ___05_create_equipment
    let writePhase1EquiData (directory : string) 
                        (filePrefix : string) 
                        (equiData : NewEqui list) : AiwCompilerMonad<unit> = 
        if List.isEmpty equiData then
            mreturn ()
        else
            compile { 
                let! fileName = genFileName directory filePrefix "05_create_equipment"
                do! writeNewEquisFile equiData fileName
                return()
            }


    /// This just writes a List of equipment, not a patch
    let writePhase1EquipmentList (directory: string) 
                                (filePrefix: string) 
                                (equiData: NewEqui list) : AiwCompilerMonad<unit> =
        let makeRow (equi: NewEqui): string = 
            sprintf "%s\t%s" equi.Description (equi.FuncLoc.ToString())

        compile { 
            let name1 = sprintf "%s_equipment_list.txt" (safeName filePrefix)
            let path = Path.Combine(directory, name1)
            let sw = new StreamWriter(path = path)
            do sw.WriteLine "TXTMI\tTPLN_EILO"
            do List.iter (fun (row: NewEqui) -> sw.WriteLine (makeRow row)) equiData
            do sw.Close()
            return ()
        }

    /// Write Phase1 Floc Data patches and NewEqui patches
    let writePhase1All (directory : string) 
                        (filePrefix : string) 
                        (phase1FlocData : Phase1FlocData) 
                        (phase1Equipment: NewEqui list) : AiwCompilerMonad<unit> = 
        
        compile {
            let source = phase1FlocData.RemoveDups ()
            do! writePhase1FlocData directory filePrefix source
            do! writePhase1EquiData directory filePrefix phase1Equipment
            do! writePhase1EquipmentList directory filePrefix phase1Equipment
            return ()
        }
            
         


    // Write ClassEqui and ValuaEqui and Eqmltext patch files
    let writePhase2Data (directory : string) 
                        (filePrefix : string) 
                        (equiData : Phase2Data) : AiwCompilerMonad<unit> = 
        if equiData.IsEmpty then
            mreturn ()
        else
            compile {
                let! outPath06 = genFileName directory filePrefix "06_create_classequis"
                do! writeNewClassEquisFile equiData.ClassEquis outPath06
                let! outPath07 = genFileName directory filePrefix "07_create_valuaequis"
                do! writeNewValuaEquisFile equiData.ValuaEquis outPath07
                let! outPath08 = genFileName directory filePrefix "08_create_eqmltxts"
                do! writeNewEqmltxtsFile equiData.Eqmltxts outPath08
                return ()
            }

    // ************************************************************************
    // New Floc Attributes (add to existing floc)



    /// Write new ClassFloc and ValuaFloc patches
    let writeFlocAttributes (directory : string) 
                        (filePrefix : string) 
                        (flocAttrs : FlocAttributes) : AiwCompilerMonad<unit> =         
        compile {
            let! outPath01 = genFileName directory filePrefix "01_add_classflocs"
            do! writeNewClassFlocsFile flocAttrs.ClassFlocs outPath01
            let! outPath02 = genFileName directory filePrefix "02_add_valuaflocs"
            do! writeNewValuaFlocsFile flocAttrs.ValuaFlocs outPath02
            return ()
        }

    // ************************************************************************
    // New Equi Attributes (add to existing equipment)



    /// Write new ClassEqui and ValuaEqui patches
    let writeEquiAttributes (directory : string) 
                        (filePrefix : string) 
                        (equiAttrs : EquiAttributes) : AiwCompilerMonad<unit> =         
        compile {
            let! outPath01 = genFileName directory filePrefix "01_add_classequis"
            do! writeNewClassEquisFile equiAttrs.ClassEquis outPath01
            let! outPath02 = genFileName directory filePrefix "02_add_valuaequis"
            do! writeNewValuaEquisFile equiAttrs.ValuaEquis outPath02
            return ()
        }
