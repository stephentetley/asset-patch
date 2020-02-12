#r "netstandard"
#r "System.Text.Encoding.dll"
open System.IO


#I @"C:\Users\stephen\.nuget\packages\ExcelProvider\1.0.1\lib\netstandard2.0"
#r "ExcelProvider.Runtime.dll"

#I @"C:\Users\stephen\.nuget\packages\ExcelProvider\1.0.1\typeproviders\fsharp41\netstandard2.0"
#r "ExcelDataReader.DataSet.dll"
#r "ExcelDataReader.dll"
#r "ExcelProvider.DesignTime.dll"

#I @"C:\Users\stephen\.nuget\packages\system.io.packaging\4.5.0\lib\netstandard1.3"
#r "System.IO.Packaging"
#I @"C:\Users\stephen\.nuget\packages\DocumentFormat.OpenXml\2.9.1\lib\netstandard1.3"
#r "DocumentFormat.OpenXml"


#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"


open FSharp.Core

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"

#load "..\src\AssetPatch\Base\Addendum.fs"
#load "..\src\AssetPatch\Base\Common.fs"
#load "..\src\AssetPatch\Base\AssocList.fs"
#load "..\src\AssetPatch\Base\FuncLocPath.fs"
#load "..\src\AssetPatch\Base\ValuaValue.fs"
#load "..\src\AssetPatch\Base\Aiw\ChangeFile.fs"
#load "..\src\AssetPatch\Base\Aiw\Acronyms.fs"
#load "..\src\AssetPatch\Base\Aiw\ChangeFileParser.fs"
#load "..\src\AssetPatch\Base\Aiw\ChangeFilePrinter.fs"
#load "..\src\AssetPatch\TemplatePatcher\Base\TemplateHierarchy.fs"
#load "..\src\AssetPatch\TemplatePatcher\Base\Template.fs"
#load "..\src\AssetPatch\TemplatePatcher\Base\CompilerMonad.fs"
#load "..\src\AssetPatch\TemplatePatcher\Aiw\Base.fs"
#load "..\src\AssetPatch\TemplatePatcher\Aiw\PatchTypes.fs"
#load "..\src\AssetPatch\TemplatePatcher\Aiw\PatchWriter.fs"
#load "..\src\AssetPatch\TemplatePatcher\Aiw\EmitPhase1.fs"
#load "..\src\AssetPatch\TemplatePatcher\Aiw\EmitPhase2.fs"
#load "..\src\AssetPatch\TemplatePatcher\Aiw\EmitNewAttributes.fs"
#load "..\src\AssetPatch\TemplatePatcher\Aiw\PatchCompiler.fs"
#load "..\src\AssetPatch\Lib\Common.fs"
#load "..\src\AssetPatch\Lib\OSGB36.fs"
#load "..\src\AssetPatch\TemplateCatalogue\Root.fs"
#load "..\src\AssetPatch\TemplateCatalogue\Lstn.fs"
#load "..\src\AssetPatch\TemplateCatalogue\Netw.fs"
open AssetPatch.Base.FuncLocPath
open AssetPatch.TemplatePatcher.Aiw.Base
open AssetPatch.TemplatePatcher.Base.Template
open AssetPatch.TemplatePatcher.Base.CompilerMonad
open AssetPatch.TemplatePatcher.Aiw.EmitPhase1
open AssetPatch.TemplatePatcher.Aiw.PatchCompiler
open AssetPatch.TemplateCatalogue


let outputDirectory (child : string) : string = 
    match child with 
    | null | "" -> Path.Combine(__SOURCE_DIRECTORY__, @"..\output")
    | _ -> Path.Combine(__SOURCE_DIRECTORY__, @"..\output", child)


type RowParams = 
    { Code : string
      Name : string 
      Easting : int
      Northing : int
      EquiFlocSaiNumber : string option
      EquiPliNumber : string option
    }


let edcTemplate (parameters : RowParams) : Function = 
    let east_north_common = 
        east_north [ easting parameters.Easting; northing parameters.Northing ]
    
    environmental_discharge 
        [ east_north_common 
          aib_reference [ s4_aib_reference () ] 
        ]
        [ 
          liquid_discharge
            [ east_north_common
              aib_reference [ s4_aib_reference () ] 
            ]
            [   
              regulatory_monitoring
                [ east_north_common 
                  aib_reference [ s4_aib_reference () ]    
                ]
                [   
                  montoring_system "SYS01" "EA Event Duration Monitoring"
                    [ east_north_common 
                      aib_reference [ s4_aib_reference () ]
                    ]
                    _no_assemblies_
                    [ 
                      Lstn.lstn_level_transmitter "Storm Overflow Level Monitor Loop"
                        System.DateTime.Now
                        ""

                        [ east_north_common
                          aib_reference [ s4_aib_reference () ]
                        ]
                        _no_subordinate_equipment_
                        [ manufacturer "SIEMENS"
                          model "HYDRORANGER 200" 
                        ]
                    ]
                ]
            ]
        ]



let test01 () = 
    let worklist = 
        [ ("KRI03", {Code = "KRI03"; Name = "Kriddle SPS"; Easting = 492729; Northing = 477323; EquiFlocSaiNumber = Some "SAI00043252"; EquiPliNumber = Some "PLI00002001" } )
        ] 
        |> List.map (fun (name, v) -> (FuncLocPath.Create name, v))
    let env : AiwEnv = 
        { UserName = "TETLEYS"
          EquiIndices = None
        }
    runCompiler env
        <| compile {
                 let! worklist1 = applyFlocTemplate worklist edcTemplate
                 let! (phase1Data, newEquis) = functionListEmitPhase1 worklist1
                 do! writePhase1All (outputDirectory "discharge") "env_discharge" phase1Data newEquis
                 return ()
             }

// This template has optional elements that are possible but a bit ugly...
// We have to represent option elements as a list that might have zero-or-one
// elements then `yield!` them.
// Single (non-optional) elements need to use `yield` when in the same list.
// The symbolic combinators (&&=) and (??=) are largely superfluous.
// 
let caaTemplate (parameters : RowParams) : Site = 
    let east_north_common = 
        east_north [ easting parameters.Easting; northing parameters.Northing ]
    
    _site parameters.Code parameters.Name
        [ east_north_common 
          aib_reference [ s4_aib_reference () ] 
        ]
        [
            control_and_automation 
                [ east_north_common 
                  aib_reference [ s4_aib_reference () ] 
                ]
                [ 
                  networks
                    [ east_north_common
                      aib_reference [ s4_aib_reference () ] 
                    ]
                    [   
                      telemetry
                        [ east_north_common 
                          aib_reference [ s4_aib_reference () ]    
                        ]
                        [   
                          telemetry_system "SYS01" "Telemetry System"
                            [ east_north_common 
                              aib_reference [ s4_aib_reference () ]
                            ]
                            _no_assemblies_
                            [ 
                              Netw.telemetry_outstation "Telemetry Outstation"
                                System.DateTime.Now
                                ""
                                [ east_north_common
                                  aib_reference                             
                                    [ s4_aib_reference ()
                                        // See elsewhere how to deal with optionals...
                                      // applyOptional ai2_aib_reference parameters.EquiFlocSaiNumber
                                      // applyOptional ai2_aib_reference parameters.EquiPliNumber
                                    ]
                                ]
                                _no_subordinate_equipment_
                                [ manufacturer "METASPHERE"
                                  model "MMIM" 
                                  serial_number "TO BE DETERMINED"
                                ]
                            ]
                        ]            
                    ]
                ]
            ]

let test02 () = 
    let worklist = 
        [ {Code = "SPT60"; Name = "Stephen SPS"; Easting = 492729; Northing = 477323; EquiFlocSaiNumber = Some "SAI00043252"; EquiPliNumber = Some "PLI00002001"} 
        ] |> List.map (fun r1 -> (FuncLocPath.Create r1.Code, r1))
    
    let env : AiwEnv = 
        { UserName = "TETLEYS"
          EquiIndices = None
        }
    runCompiler env
       <| compile {
                let! worklist1 = applyFlocTemplate worklist caaTemplate
                let! (phase1Data, newEquis) = siteListEmitPhase1 worklist1
                do! writePhase1All (outputDirectory "caa-patches") "control_automation" phase1Data newEquis
                return ()
            }


