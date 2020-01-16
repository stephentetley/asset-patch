// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace OutstationPatcher

module OutstationTemplate =
    
    open System

    open AssetPatch.TemplatePatcher.Template
    open AssetPatch.TemplateCatalogue
    open AssetPatch.TemplateCatalogue.Ctossy
    open AssetPatch.TemplateCatalogue.Netwtl
    open AssetPatch.Lib.Common
    open AssetPatch.Lib.OSGB36

    open OutstationPatcher.InputData


    /// Note - this is very flaky as ExcelProvider seems to have difficulty 
    /// with Excel's type casting.
    let getInstallDate (source : string) : DateTime = 
        match tryGetUSDate source with
        | Some date -> date
        | None -> new DateTime(year=1970, month=1, day=1)




    let os_aib_reference (ai2_ref : string) = 
        aib_reference 
            [   s4_aib_reference ()
                ai2_aib_reference ai2_ref                
            ]



    let osLevel5Template (parameters : WorkListRow) : System = 
        let installDate = getInstallDate parameters.``Install Date``

        telemetry_system parameters.``System Code`` parameters.``System Name``
            [ east_north_common parameters.NGR
              os_aib_reference parameters.``AI2 Equipment SAI Number``
              ctossy 
                [ system_type "REMOTE TELEMETRY SYSTEM"
                ]
            ]
            _no_assemblies_
            [ telemetry_outstation parameters.``Outstation Name``
                [ east_north_common parameters.NGR
                  aib_reference 
                    [ s4_aib_reference () 
                      ai2_aib_reference parameters.``AI2 Equipment SAI Number``
                      ai2_aib_reference parameters.``AI2 Equipment PLI Code``
                    ]
                  netwtl 
                    [
                    ]    
                ]
                _no_subordinate_equipment_
                [ manufacturer parameters.Manufacturer
                  model parameters.Model
                  serial_number parameters.``Serial Number``
                  construction_year installDate.Year
                  construction_month installDate.Month
                ]
            ]
    
    let osLevel4Template (parameters : WorkListRow) : Process = 
        telemetry
            [ east_north_common parameters.NGR
                // aib_reference_common   
            ]
            [   
                osLevel5Template parameters
            ]

    let osLevel3Template (parameters : WorkListRow) : ProcessGroup = 
        networks
            [ east_north_common parameters.NGR
                // aib_reference_common
            ]
            [ 
                osLevel4Template parameters
            ]

    let osLevel2Template (parameters : WorkListRow) : Function = 
        control_and_automation 
            [ east_north_common parameters.NGR
              // aib_reference_common parameters.
            ]
            [ 
                osLevel3Template parameters
            ]
    

