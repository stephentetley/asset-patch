// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace AssetPatch.OutstationPatcher

module OutstationTemplate =
    
    open System

    open AssetPatch.TemplatePatcher.Base.Template
    open AssetPatch.TemplatePatcher.Catalogue
    open AssetPatch.TemplatePatcher.Catalogue.Equi
    open AssetPatch.TemplatePatcher.Catalogue.AssetCondition
    open AssetPatch.TemplatePatcher.Catalogue.Ctos
    open AssetPatch.TemplatePatcher.Catalogue.Netw
    open AssetPatch.Lib.Common

    open AssetPatch.OutstationPatcher.OutstationWorkList


    let private startupDateTrafo (parameters: {| InstallDate: string |}) : EnvTransformer = 
        match tryGetUSDate parameters.InstallDate with
        | None -> id
        | Some date -> startupDate date

    let private getL2Sainum (parameters: WorkListRow): string = 
        parameters.``CAA SAI Reference [Control Services]``

    let private getL3Sainum (parameters: WorkListRow): string = 
        parameters.``NET AIB Reference [usually Control Services]``
        
    let private getL4Sainum (parameters: WorkListRow): string = 
        parameters.``TEL AIB Reference [RTS Monitoring]``
        
    let private getL5Sainum (parameters: WorkListRow): string = 
        parameters.``SYS AIB Reference [usually RTS Monitoring]``
        

    // ************************************************************************
    // Hierarchy templates

    let makeModem (parameters : WorkListRow) : Equipment = 
        let installDate = getAiDate parameters.``Modem Install From Date`` |> Option.defaultValue DateTime.Now
        modem parameters.``Modem Name`` 
              installDate
              parameters.``Memo Line``
            [ manufacturer parameters.``Modem Manufacturer``
              model parameters.``Modem Model``
              serial_number parameters.``Modem Serial Number``
              construction_year installDate.Year
              construction_month installDate.Month
            ]
            [ Equi.east_north_common parameters.NGR
              Equi.aib_reference_common parameters.``Equi SAI Number`` parameters.``Equi PLI Number``
              netwmo [
                Equi.uniclass_code ()
                Equi.uniclass_desc ()
                Equi.memo_line "SEE LONG TEXT"
                ]
              asset_condition_common installDate.Year
            ]
            _no_subordinate_equipment_ 
            

    let makeTelemetryOustation (parameters : WorkListRow) : Equipment = 
        let installDate = getAiDate parameters.``Installed From Date`` |> Option.defaultValue DateTime.Now
        telemetry_outstation 
            parameters.``Description [S4 display name]``
            installDate
            parameters.``Memo Line``
            
            [ manufacturer parameters.Manufacturer
              model parameters.Model
              serial_number parameters.``Serial Number``
              construction_year installDate.Year
              construction_month installDate.Month
            ]
            [ east_north_common parameters.NGR
              aib_reference_common parameters.``Equi SAI Number`` parameters.``Equi PLI Number``
              netwtl [
                    uniclass_code ()
                    uniclass_desc ()
                    memo_line "SEE LONG TEXT"
                    ] 
              asset_condition_common installDate.Year
            ]
            _no_subordinate_equipment_

    /// Level 5
    let makeSYS (parameters : WorkListRow) : System = 
        Floc.telemetry_system 
                parameters.``System Code``
                parameters.``System Name``
                [ Floc.east_north_common parameters.NGR
                  Floc.aib_reference_common (getL5Sainum parameters)
                  ctossy 
                    [ system_type "REMOTE TELEMETRY SYSTEM"
                    ]
                ]
                Floc._no_assemblies_
                [ makeTelemetryOustation parameters
                  makeModem parameters
                ]
    
    /// Level 4
    let makeTEL (parameters : WorkListRow) : Process = 
        Floc.telemetry
            [ Floc.east_north_common parameters.NGR
              Floc.aib_reference_common (getL4Sainum parameters)
            ]
            [   
                makeSYS parameters
            ]

    /// Level 3
    let makeNET (parameters : WorkListRow) : ProcessGroup = 
        Floc.networks
            [ Floc.east_north_common parameters.NGR
              Floc.aib_reference_common (getL3Sainum parameters)
            ]
            [ 
                makeTEL parameters
            ]

    /// Level 2
    let makeCAA (parameters : WorkListRow) : Function = 
        Floc.control_and_automation 
            [ Floc.east_north_common parameters.NGR
              Floc.aib_reference_common (getL2Sainum parameters)
            ]
            [ 
                makeNET parameters
            ]
    

