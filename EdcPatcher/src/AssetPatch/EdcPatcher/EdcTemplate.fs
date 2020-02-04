// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace AssetPatch.EdcPatcher

module EdcTemplate =
    
    open System

    open AssetPatch.TemplatePatcher.Base.Template
    open AssetPatch.Lib.Common
    open AssetPatch.TemplateCatalogue
    open AssetPatch.TemplateCatalogue.AssetCondition
    open AssetPatch.TemplateCatalogue.Lstn
    open AssetPatch.TemplateCatalogue.Smon


    open AssetPatch.EdcPatcher.InputData

    let private optString (source : string) : string option = 
        match source with
        | null | "" -> None
        |_ -> Some source

    /// Note - this is very flaky as ExcelProvider seems to have difficulty 
    /// with Excel's type casting.
    let private getInstallDate (source : string) : DateTime = 
        match source with
        | null | "" -> new DateTime(year=1970, month=1, day=1)
        | _ -> 
            match tryGetUSDate source with
            | Some date -> date
            | None -> new DateTime(year=1970, month=1, day=1)


    let private whenNotBlank (fn : string -> Characteristic) (str: string) : Characteristic option = 
        if String.IsNullOrWhiteSpace str then None else Some (str.Trim() |> fn)
        
    let private whenDecimal (fn : decimal -> Characteristic) (str: string) : Characteristic option = 
        try 
            if String.IsNullOrWhiteSpace str then None else Some (str |> decimal |> fn)
        with
        | _ -> None

    let private lstnut_leaf_instance (parameters : WorkListRow) : Classification = 
        lstnut
            [ uniclass_code ()
              uniclass_desc ()
              yield!
                (List.choose id
                    [ whenNotBlank lstn_transducer_model        parameters.``Transducer Model``
                      whenNotBlank lstn_transducer_serial_no    parameters.``Transducer Serial Number``
              
                      whenNotBlank (lstn_relay_function 1)      parameters.``Relay 1 Function``
                      whenDecimal (lstn_relay_on_level 1)       parameters.``Relay 1 On``
                      whenDecimal (lstn_relay_off_level 1)      parameters.``Relay 1 Off``
              
                      whenNotBlank (lstn_relay_function 2)      parameters.``Relay 2 Function``
                      whenDecimal (lstn_relay_on_level 2)       parameters.``Relay 2 On``
                      whenDecimal (lstn_relay_off_level 2)      parameters.``Relay 2 Off``
              
                      whenNotBlank (lstn_relay_function 3)      parameters.``Relay 3 Function``
                      whenDecimal (lstn_relay_on_level 3)       parameters.``Relay 3 On``
                      whenDecimal (lstn_relay_off_level 3)      parameters.``Relay 3 Off``
              
                      whenNotBlank (lstn_relay_function 4)      parameters.``Relay 4 Function``
                      whenDecimal (lstn_relay_on_level 4)       parameters.``Relay 4 On``
                      whenDecimal (lstn_relay_off_level 4)      parameters.``Relay 4 Off``
              
                      whenNotBlank (lstn_relay_function 5)      parameters.``Relay 5 Function``
                      whenDecimal (lstn_relay_on_level 5)       parameters.``Relay 5 On``
                      whenDecimal (lstn_relay_off_level 5)      parameters.``Relay 5 Off``

                      whenNotBlank (lstn_relay_function 6)      parameters.``Relay 6 Function``
                      whenDecimal (lstn_relay_on_level 6)       parameters.``Relay 6 On``
                      whenDecimal (lstn_relay_off_level 6)      parameters.``Relay 6 Off``
              
                    ] )

              ]

   
    let private startupDateTrafo (parameters: {| InstallDate: string |}) : EnvTransformer = 
        match tryGetUSDate parameters.InstallDate with
        | None -> id
        | Some date -> startupDate date


    let private getL2Sainum (parameters: WorkListRow): string = 
        parameters.``AI2 EDC Sainum``

    let private getL3Sainum (parameters: WorkListRow): string = 
        let level3 = parameters.``AI2 LQD Sainum``
        match level3.ToUpper() with
        | "DITTO" -> getL2Sainum parameters
        | _ -> level3

    let private getL4Sainum (parameters: WorkListRow): string = 
        let level4 = parameters.``AI2 RGM Sainum``
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

    let makeLevelTransmitter (parameters : WorkListRow) : Equipment = 
        let year : int = (getInstallDate parameters.``Install Date``).Year
        let month : int = (getInstallDate parameters.``Install Date``).Month
        locals [startupDateTrafo {| InstallDate = parameters.``Install Date`` |}]
            <| lstn_level_transmitter parameters.``Level Controller Name``
                    parameters.``Memo Line``
                [ east_north_common parameters.NGR
                  aib_reference_equipment_common parameters.``AI2 Equipment Sainum`` parameters.``AI2 Equipment PLI Code``
                  lstnut_leaf_instance parameters
                  asset_condition_common year
                ]
                _no_subordinate_equipment_
                [ manufacturer parameters.``Controller Manufacturer``
                  model parameters.``Controller Model``
                  serial_number parameters.``Controller Serial Number``
                  construction_year year

                  construction_month month
                ]


    let makeSYS (parameters : WorkListRow) : System =  
        let sysCode = parameters.``S4 System Code``
        locals [startupDateTrafo {| InstallDate = parameters.``Install Date`` |}]
            <| montoring_system sysCode "EA Event Duration Monitoring"
                    [ east_north_common parameters.NGR
                      aib_reference_floc_common (getL5Sainum parameters)
                      smonsy 
                          [ system_type "EA Overflow Monitoring" 
                          ]
                    ]
                    _no_assemblies_
                    [ 
                        makeLevelTransmitter parameters
                    ]
                
    /// Level 4
    let makeRGM (parameters : WorkListRow) : Process = 
        locals [startupDateTrafo {| InstallDate = parameters.``Install Date`` |}]
            <| regulatory_monitoring
                [ east_north_common parameters.NGR
                  aib_reference_floc_common (getL4Sainum parameters)
                ]
                [   
                  makeSYS parameters
                ]

    /// Level 3
    let makeLQD (parameters : WorkListRow) : ProcessGroup = 
        locals [startupDateTrafo {| InstallDate = parameters.``Install Date`` |}]
            <| liquid_discharge
                [ east_north_common parameters.NGR
                  aib_reference_floc_common (getL3Sainum parameters)
                ]
                [   
                  makeRGM parameters
                ]

    /// Level 2
    let makeEDC (parameters : WorkListRow) : Function =        
        locals [startupDateTrafo {| InstallDate = parameters.``Install Date`` |}]
            <| environmental_discharge 
                [ east_north_common parameters.NGR
                  aib_reference_floc_common (getL2Sainum parameters)
                ]
                [ 
                    makeLQD parameters
                ]
    

