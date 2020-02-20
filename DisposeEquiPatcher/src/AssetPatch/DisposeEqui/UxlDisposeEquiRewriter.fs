// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.DisposeEqui

module UxlDisposeEquiRewriter =
    
    open System

    open AssetPatch.RewritePatcher.Base.Rewrite
    open AssetPatch.RewritePatcher.Base.RewriteMonad
    open AssetPatch.RewritePatcher.Base.Uxl
    open AssetPatch.RewritePatcher.Catalogue.EquiRoot
    open AssetPatch.DisposeEqui.InputData

    
    type UxlOptions = 
        { ProcessRequester : string 
          ChangeRequestDescription: string
          FileNamePrefix: string
          WorkListPath : string
          OutputDirectory : string
        }

    type internal DisposeEqui = 
        { ProcessRequester: string
          ChangeRequestDescription: string
          EquipmentId: string   
          DescriptionMediumText: string
          Status: string
        }


    /// To get an type that we are in control of (i.e. we can implement 
    /// the HasEquiId interface) we have to wrap the Row type provided by
    /// ExcelProvider
    
    [< Struct>]
    type WorkRow = 
        | WorkRow of WorkListRow
    
        member x.Row 
            with get () : WorkListRow = 
                match x with | WorkRow x1 -> x1
        
        interface HasEquiId with
            member x.EquiId = 
                match x with 
                | WorkRow x1 -> x1.``S4 Equipment Id`` 
    
            member x.SuperOrdinateId = 
                match x with 
                | WorkRow x1 -> 
                    let s1 = x1.``Superior Equipment``
                    if String.IsNullOrWhiteSpace s1 then "" else s1
    
    let equiDispose () : EquiRewrite<WorkRow> = 
        rewrite { 
            let! s1 = gets <| fun (x:WorkRow) -> x.Row.``Name ``
            do! description (s1 + " (Del)")
            do! statusOfAnObject "DISP"
            return ()
        }


    let runUxlDisposeEquiPatcher (opts: UxlOptions) = 
        let worklist = 
            readWorkList opts.WorkListPath
                |> List.map WorkRow

        runRewriter (defaultEnv opts.ChangeRequestDescription ())
            <| rewriter { 
                    let! changes = rewriteEquiAll (equiDispose ()) worklist
                    do! writeChangeRequestAndEquipmentPatches changes opts.OutputDirectory opts.FileNamePrefix
                    return ()
                }