// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace EdcPatcher

module EdcTemplate =
    
    open System

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.Template
    open AssetPatch.TemplateCatalogue
    open AssetPatch.TemplateCatalogue.Smonsy
    open AssetPatch.Lib.Common
    open AssetPatch.Lib.OSGB36

    open EdcPatcher.InputData

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


    let private aib_reference_leaf_instance (parameters : WorkListRow) : Class = 
        aib_reference 
            [ s4_aib_reference () 
              ai2_aib_reference parameters.``AI2 Equipment SAI Number``
              ai2_aib_reference parameters.``AI2 Equipment PLI Code``
            ]

    let private lstnut_leaf_instance (parameters : WorkListRow) : Class = 
        lstnut
            [ uniclass_code ()
              uniclass_desc ()
              optional <| lstn_transducer_model         parameters.``Transducer Model``
              optional <| lstn_transducer_serial_no     parameters.``Transducer Serial Number``
              applyOptional (lstn_relay_function 1) (tryGetNonBlank parameters.``Relay 1 Function``)
              applyOptional (lstn_relay_on_level 1) (tryGetDecimal parameters.``Relay 1 On``)
              applyOptional (lstn_relay_off_level 1) (tryGetDecimal parameters.``Relay 1 Off``)
              
              applyOptional (lstn_relay_function 2) (tryGetNonBlank parameters.``Relay 2 Function``)
              applyOptional (lstn_relay_on_level 2) (tryGetDecimal parameters.``Relay 2 On``)
              applyOptional (lstn_relay_off_level 2) (tryGetDecimal parameters.``Relay 2 Off``)
              
              applyOptional (lstn_relay_function 3) (tryGetNonBlank parameters.``Relay 3 Function``)
              applyOptional (lstn_relay_on_level 3) (tryGetDecimal parameters.``Relay 3 On``)
              applyOptional (lstn_relay_off_level 3) (tryGetDecimal parameters.``Relay 3 Off``)
              
              applyOptional (lstn_relay_function 4) (tryGetNonBlank parameters.``Relay 4 Function``)
              applyOptional (lstn_relay_on_level 4) (tryGetDecimal parameters.``Relay 4 On``)
              applyOptional (lstn_relay_off_level 4) (tryGetDecimal parameters.``Relay 4 Off``)
              
              applyOptional (lstn_relay_function 5) (tryGetNonBlank parameters.``Relay 5 Function``)
              applyOptional (lstn_relay_on_level 5) (tryGetDecimal parameters.``Relay 5 On``)
              applyOptional (lstn_relay_off_level 5) (tryGetDecimal parameters.``Relay 5 Off``)

              applyOptional (lstn_relay_function 6) (tryGetNonBlank parameters.``Relay 6 Function``)
              applyOptional (lstn_relay_on_level 6) (tryGetDecimal parameters.``Relay 6 On``)
              applyOptional (lstn_relay_off_level 6) (tryGetDecimal parameters.``Relay 6 Off``)
              
            ]

    
    let private east_north_common (parameters: {| NGR: string |}) = 
        match NGR.Create parameters.NGR with
        | Some eastNorth -> east_north_ngr eastNorth
        | None ->  east_north [ easting 0; northing 0 ]

    let private aib_reference_common (parameters: {| SAI: string |}) =  
        aib_reference 
            [   s4_aib_reference ()
                ai2_aib_reference parameters.SAI
                
            ]

    let private startupDateTrafo (parameters: {| InstallDate: string |}) : EnvTransformer = 
        match tryGetUSDate parameters.InstallDate with
        | None -> id
        | Some date -> startupDate date

    // ************************************************************************
    // Hierarchy templates

    let edcLevelTransmitter (parameters : WorkListRow) : Equipment = 
        let year : int = (getInstallDate parameters.``Install Date``).Year
        let month : int = (getInstallDate parameters.``Install Date``).Month
        locals [startupDateTrafo {| InstallDate = parameters.``Install Date`` |}]
            <| lstn_level_transmitter parameters.``Level Controller Name``
                [ east_north_common {| NGR = parameters.NGR |}
                  aib_reference_leaf_instance parameters
                  lstnut_leaf_instance parameters
                  asset_condition_new_item year
                ]
                _no_subordinate_equipment_
                [ manufacturer parameters.``Controller Manufacturer``
                  model parameters.``Controller Model``
                  serial_number parameters.``Controller Serial Number``
                  construction_year year
                  construction_month month
                ]


    let edcSystem (parameters : WorkListRow) : System =  
        let sysFloc = FuncLocPath.Create parameters.``S4 Equipment FuncLoc``
        match sysFloc.LevelCode 5 with
        | None -> templateError (sprintf "failed to get system for %s" (sysFloc.ToString()) )
        | Some sysCode -> 
            locals [startupDateTrafo {| InstallDate = parameters.``Install Date`` |}]
                <| montoring_system sysCode "EA Event Duration Monitoring"
                    [ east_north_common {| NGR = parameters.NGR |}
                      aib_reference_common {| SAI = parameters.``AI2 Root Reference`` |}
                      smonsy 
                        [ system_type "EA Overflow Monitoring" 
                        ]
                    ]
                    _no_assemblies_
                    [ 
                      edcLevelTransmitter parameters
                    ]
                
    let edcRegulatoryMonitoring (parameters : WorkListRow) : Process = 
        locals [startupDateTrafo {| InstallDate = parameters.``Install Date`` |}]
            <| regulatory_monitoring
                [ east_north_common {| NGR = parameters.NGR |}
                  aib_reference_common {| SAI = parameters.``AI2 Root Reference`` |}
                ]
                [   
                  edcSystem parameters
                ]

    let edcLiquidDischarge (parameters : WorkListRow) : ProcessGroup = 
        locals [startupDateTrafo {| InstallDate = parameters.``Install Date`` |}]
            <| liquid_discharge
                [ east_north_common {| NGR = parameters.NGR |}
                  aib_reference_common {| SAI = parameters.``AI2 Root Reference`` |}
                ]
                [   
                  edcRegulatoryMonitoring parameters
                ]


    let edcEnvironmentalDischarge (parameters : WorkListRow) : Function =        
        locals [startupDateTrafo {| InstallDate = parameters.``Install Date`` |}]
            <| environmental_discharge 
                [ east_north_common {| NGR = parameters.NGR |}
                  aib_reference_common  {| SAI = parameters.``AI2 Root Reference`` |}
                ]
                [ 
                    edcLiquidDischarge parameters
                ]
    

