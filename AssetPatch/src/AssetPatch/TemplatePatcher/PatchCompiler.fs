// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher


module PatchCompiler =
    
    open System.IO


    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath 
    open AssetPatch.TemplatePatcher.PatchTypes
    open AssetPatch.TemplatePatcher.CompilerMonad
    open AssetPatch.TemplatePatcher.PatchWriter
    open AssetPatch.TemplatePatcher.EmitPhase1
    open AssetPatch.TemplatePatcher.EmitPhase2


    // ************************************************************************
    // Compile Class * Valua patches to update exting Flocs and Equipment...


    let applyTemplate (path : FuncLocPath) 
                                (xs: ('name * 'b) list ) 
                                (template: 'b -> Template.Template<'c>) : CompilerMonad<('name * 'c) list> = 
        forM xs (fun (name, x) -> evalTemplate path (template x) >>=  fun a -> mreturn (name, a))

    let applyFlocTemplate1 (path : FuncLocPath, body : 'a) 
                            (template: 'a -> Template.Template<'b>) : CompilerMonad<'b> = 
        evalTemplate path (template body)

    let applyFlocTemplate (xs: (FuncLocPath * 'a) list ) 
                        (template: 'a -> Template.Template<'b>) : CompilerMonad<'b list> = 
        forM xs (fun (path, x) -> evalTemplate path (template x))


    // ************************************************************************
    // Write output


    let writePhase1FlocData (directory : string) 
                            (filePrefix : string) 
                            (funcLocResults : Phase1FlocData) : CompilerMonad<unit> = 
        
        if funcLocResults.IsEmpty then
            mreturn ()
        else
            compile {
                do! writeNewFuncLocsFile directory filePrefix funcLocResults.FuncLocs
                do! writeLinkFuncLocsFile directory filePrefix funcLocResults.FuncLocLinks
                do! writeNewClassFlocsFile directory filePrefix funcLocResults.ClassFlocs
                do! writeNewValuaFlocsFile directory filePrefix funcLocResults.ValuaFlocs
                return ()
            }

   
    
    // Write an Equi patch file
    let writePhase1EquiData (directory : string) 
                        (filePrefix : string) 
                        (equiData : NewEqui list) : CompilerMonad<unit> = 
        if List.isEmpty equiData then
            mreturn ()
        else
            writeNewEquisFile directory filePrefix equiData


    /// This just writes a List of equipment, not a patch
    let writePhase1EquipmentList (directory: string) 
                                (filePrefix: string) 
                                (equiData: NewEqui list) : CompilerMonad<Unit> =
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
                        (phase1Equipment: NewEqui list) : CompilerMonad<unit> = 
        
        compile {
            let source = phase1FlocData.RemoveDups ()
            do! writePhase1FlocData directory filePrefix source
            do! writePhase1EquiData directory filePrefix phase1Equipment
            do! writePhase1EquipmentList directory filePrefix phase1Equipment
            return ()
        }
            
            


    // Write ClassEqui and ValuaEqui patch files
    let writePhase2EquiData (directory : string) 
                                (filePrefix : string) 
                                (equiData : Phase2Data) : CompilerMonad<unit> = 
        if equiData.IsEmpty then
            mreturn ()
        else
            compile {
                do! writeNewClassEquisFile directory filePrefix equiData.ClassEquis
                do! writeNewValuaEquisFile directory filePrefix equiData.ValuaEquis
                do! writeNewEqmltxtsFile directory filePrefix equiData.Eqmltxts
                return ()
            }

    // Write ClassEqui and ValuaEqui patch files
    let writePhase2Data (directory : string) 
                                (filePrefix : string) 
                                (phase2Data : Phase2Data) : CompilerMonad<unit> = 
        writePhase2EquiData directory filePrefix phase2Data


    