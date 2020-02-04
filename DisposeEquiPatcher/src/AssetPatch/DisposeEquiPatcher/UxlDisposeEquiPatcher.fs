// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.DisposeEquiPatcher

module UxlDisposeEquiPatcher =
    
    open System.IO

    open AssetPatch.DisposeEquiPatcher.InputData

    type UxlOptions = 
        { ProcessRequester : string 
          ChangeRequestDescription: string
          WorkListPath : string
          OutputDirectory : string
          }

    type DisposeEqui = 
        { ProcessRequester: string
          ChangeRequestDescription: string
          EquipmentId: string   
          DescriptionMediumText: string
          Status: string
        }


    let private inputRowToDisposeEqui (opts: UxlOptions) (row: WorkListRow) : DisposeEqui = 
        { ProcessRequester = opts.ProcessRequester
          ChangeRequestDescription = opts.ChangeRequestDescription
          EquipmentId = row.``S4 Equipment Id``         
          DescriptionMediumText = row.``Name ``
          Status = "DISP"
        }

    


    let runUxlDisposeEquiPatcher (opts : UxlOptions) : Result<unit, string> = 
        let worklist = readWorkList opts.WorkListPath |> List.map (inputRowToDisposeEqui opts)
        let name1 = Path.GetFileNameWithoutExtension(opts.WorkListPath) + "_retire_upload.txt"
        let outputPath = Path.Combine(opts.OutputDirectory, name1)
        //let changeFile = makeChangeFile opts.UserName worklist
        //writeChangeFile changeFile outputPath |> Ok
        Error "TODO"