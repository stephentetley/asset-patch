// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace OutstationPatcher

module OutstationTemplate =
    
    open System

    open AssetPatch.TemplatePatcher.Template
    open AssetPatch.TemplateCatalogue
    open AssetPatch.TemplateCatalogue.Ctos
    open AssetPatch.TemplateCatalogue.Netw
    open AssetPatch.Lib.Common

    open OutstationPatcher.InputData


    /// Note - this is very flaky as ExcelProvider seems to have difficulty 
    /// with Excel's type casting.
    let getInstallDate (source : string) : DateTime = 
        match tryGetUSDate source with
        | Some date -> date
        | None -> new DateTime(year=1970, month=1, day=1)


    let private startupDateTrafo (parameters: {| InstallDate: string |}) : EnvTransformer = 
        match tryGetUSDate parameters.InstallDate with
        | None -> id
        | Some date -> startupDate date

    let private getL2Sainum (parameters: WorkListRow): string = 
        parameters.``AI2 CAA Sainum``

    let private getL3Sainum (parameters: WorkListRow): string = 
        let level3 = parameters.``AI2 NET Sainum``
        match level3.ToUpper() with
        | "DITTO" -> getL2Sainum parameters
        | _ -> level3

    let private getL4Sainum (parameters: WorkListRow): string = 
        let level4 = parameters.``AI2 TEL Sainum``
        match level4.ToUpper() with
        | "DITTO" -> getL3Sainum parameters
        | _ -> level4

    let private getL5Sainum (parameters: WorkListRow): string = 
        let level5 = parameters.``AI2 SYS Sainum``
        match level5.ToUpper() with
        | "DITTO" -> getL4Sainum parameters
        | _ -> level5


    // ************************************************************************
    // Hierarchy templates

    let makeModem (parameters : WorkListRow) : Equipment = 
        let installDate = getInstallDate parameters.``Outstation Install Date``
        modem parameters.``Modem Name`` 
            [ east_north_common parameters.NGR
              aib_reference_equipment_common parameters.``AI2 Equipment SAI Number`` parameters.``AI2 Equipment PLI Code``
              netwtl []    
            ]
            _no_subordinate_equipment_ 
            [ manufacturer parameters.``Modem Manufacturer``
              model parameters.``Modem Model``
              serial_number parameters.``Modem Serial Number``
              construction_year installDate.Year
              construction_month installDate.Month
            ]

    let makeTelemetryOustation (parameters : WorkListRow) : Equipment = 
        let installDate = getInstallDate parameters.``Outstation Install Date``

        telemetry_outstation parameters.``Telemetry Outstation Name``
            [ east_north_common parameters.NGR
              aib_reference_equipment_common parameters.``AI2 Equipment SAI Number`` parameters.``AI2 Equipment PLI Code``
              netwtl []    
            ]
            _no_subordinate_equipment_ 
            [ manufacturer parameters.``Outstation Manufacturer``
              model parameters.``Outstation Model``
              serial_number parameters.``Oustation Serial Number``
              construction_year installDate.Year
              construction_month installDate.Month
            ]

    /// Level 5
    let makeSYS (parameters : WorkListRow) : System = 
        locals [startupDateTrafo {| InstallDate = parameters.``Outstation Install Date`` |}]
            <| telemetry_system parameters.``S4 System Code`` parameters.``S4 System Name``
                [ east_north_common parameters.NGR
                  aib_reference_floc_common (getL5Sainum parameters)
                  ctossy 
                    [ system_type "REMOTE TELEMETRY SYSTEM"
                    ]
                ]
                _no_assemblies_
                [ makeTelemetryOustation parameters
                  makeModem parameters
                ]
    
    /// Level 4
    let makeTEL (parameters : WorkListRow) : Process = 
        telemetry
            [ east_north_common parameters.NGR
              aib_reference_floc_common (getL4Sainum parameters)
            ]
            [   
                makeSYS parameters
            ]

    /// Level 3
    let makeNET (parameters : WorkListRow) : ProcessGroup = 
        networks
            [ east_north_common parameters.NGR
              aib_reference_floc_common (getL3Sainum parameters)
            ]
            [ 
                makeTEL parameters
            ]

    /// Level 2
    let makeCAA (parameters : WorkListRow) : Function = 
        control_and_automation 
            [ east_north_common parameters.NGR
              aib_reference_floc_common (getL2Sainum parameters)
            ]
            [ 
                makeNET parameters
            ]
    

