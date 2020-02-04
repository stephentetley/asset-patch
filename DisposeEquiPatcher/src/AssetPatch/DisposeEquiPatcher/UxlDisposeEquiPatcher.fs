// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.DisposeEquiPatcher

module UxlDisposeEquiPatcher =
    
    open System.IO

    open AssetPatch.TemplatePatcher.Base.CompilerMonad
    open AssetPatch.TemplatePatcher.Uxl.Base
    open AssetPatch.TemplatePatcher.Uxl.PatchTypes
    open AssetPatch.TemplatePatcher.Uxl.PatchWriter
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

    let private emitChangeRequestDetails (source: DisposeEqui): ChangeRequestDetails = 
        { DescriptionLong = source.ChangeRequestDescription
          Priority = ""
          DueDate = None
          Reason = ""
          ChangeRequestType = "AIWEAM0P"
          ChangeRequestGroup = ""
          FuncLocOrEquipment = Choice2Of2 source.EquipmentId
          ProcessRequester = source.ProcessRequester
        }

    // This is a modification so only pertinent fields need data
    let private emitEquipmentData (source: DisposeEqui): EquimentData = 
        { EquipmentId = source.EquipmentId
          EquipCategory = ""
          DescriptionMedium = source.DescriptionMediumText + " (Del)"
          ObjectType = ""
          StartupDate = None
          Manufacturer = ""
          ModelNumber = ""
          ManufPartNumber = ""
          ManufSerialNumber = ""
          ConstructionYear = None
          ConstructionMonth = None
          CompanyCode = ""
          FunctionalLocation = None
          SuperordEquip = ""
          StatusOfAnObject = "DISP"
          StatusWithoutStatusNum = ""
        }

    let runUxlDisposeEquiPatcher (opts : UxlOptions) : Result<unit, string> = 
        let userEnv = 
            { defaultUxlEnv opts.ChangeRequestDescription with ProcessRequester = opts.ProcessRequester }
        runCompiler userEnv
            <| compile { 
                let worklist = readWorkList opts.WorkListPath |> List.map (inputRowToDisposeEqui opts)
                
                /// Change Request
                let name1 = "01_dispose_equi_change_request_details.csv"
                let output1 = Path.Combine(opts.OutputDirectory, name1)
                do! writeChangeRequestDetails (List.map emitChangeRequestDetails worklist) output1
                
                /// Equipment Data
                let name2 = "01_dispose_equipment_data.csv"
                let output2 = Path.Combine(opts.OutputDirectory, name2)
                do! writeEquipmentData (List.map emitEquipmentData worklist) output2
                return ()
            }