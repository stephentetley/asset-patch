// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Uxl


module PatchTypes =
    
    open System

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath


    /// This represents one row
    type MmopChangeRequest = 
        { Description: string
          ChangeRequestType: string
          FunctionalLocation: FuncLocPath option
          EquipmentId: string option                  // Generally numeric
          ProcessRequestor: string
        }
        member x.SortKey
            with get(): string = 
                let part1 = Option.defaultValue "ZZZZZ" <| Option.map (fun floc -> floc.ToString()) x.FunctionalLocation
                let part2 = Option.defaultValue "ZZZZZ" x.EquipmentId
                part1 + "!!" + part2

        member x.ToAssocs() = 
            [ ("Description (Long)",            x.Description) 
            ; ("Priority",                      "")
            ; ("Due Date",                      "")
            ; ("Reason",                        "")
            ; ("Type of Change Request",        x.ChangeRequestType)
            ; ("Change Request Group",          "")
            ; ("MBOM-Material",                 "")
            ; ("MBOM-Plant",                    "")
            ; ("MBOM-Usage",                    "")
            ; ("FL-Functional Location",        
                    Option.defaultValue "" <| Option.map (fun floc -> floc.ToString()) x.FunctionalLocation)
            ; ("EQ-Equipment",                  Option.defaultValue "" x.EquipmentId)
            ; ("Process Requestor",             x.ProcessRequestor)
            ] |> AssocList.ofList


    type MmopNewFuncLoc = 
        { FunctionalLocation: FuncLocPath
          Description: string 
          FunLocCategory: int
          StructureIndicator: string
          StartupDate: DateTime
          ObjectType: string
        }
        member x.ToAssocs() = 
            [ ("Functional Location",           x.FunctionalLocation.ToString())
            ; ("Masked Func Loc",               x.FunctionalLocation.ToString())
            ; ("Description (medium)",          x.Description)
            ; ("FunctLocCat.",                  x.FunLocCategory.ToString())
            ; ("StrIndicator",                  x.StructureIndicator)
            ; ("Object type",                   x.ObjectType)
            ; ("Gross Weight",                    "")
            ; ("Unit of weight",                    "")
            ; ("Start-up date",                 x.StartupDate |> showS4Date)
            ; ("Currency",                    "")
            ; ("Acquistion date",                    "")
            ; ("ConstructYear",                    "")
            ; ("Company Code",                    "")
            ; ("Position",                    "")
            ; ("SupFunctLoc.",                    "")
            ; ("EquipInstall.",                    "")
            ; ("Status of an object",                    "")
            ; ("Status without stsno",                    "")
            ] |> AssocList.ofList


    type MmopNewFlocMultilingualText = 
        { FunctionalLocation: FuncLocPath
          Description: string 
          LongText: string
        }
        member x.ToAssocs() = 
            [ ("Functional Location",           x.FunctionalLocation.ToString())
            ; ("Delete Indicator",              "")
            ; ("Description",                   x.Description) 
            ; ("Language",                      "")
            ; ("Long Text",                     x.LongText)
            ] |> AssocList.ofList


    type MmopFlocClassification = 
        { FunctionalLocation: FuncLocPath
          Class: string
          CharacteristicName: string
          CharacteristicValue: string
        }
        member x.ToAssocs() = 
            [ ("Functional Location",           x.FunctionalLocation.ToString())
            ; ("Deletion Ind",                  "")
            ; ("Class Type",                    "003")
            ; ("Class",                         x.Class)
            ; ("Status",                        "")
            ; ("Characteristics",               x.CharacteristicName)
            ; ("Char Value",                    x.CharacteristicValue)
            ; ("Ch.Deletion Ind.",              "")
            ] |> AssocList.ofList


    type MmopNewEqui = 
        { EquiId: string
          EquiCategory: string  
          Description: string 
          StartupDate: DateTime
          FunctionalLocation: FuncLocPath
        }
        member x.ToAssocs() = 
            [ ("Equiment",                      x.EquiId)
            ; ("EquipCategory",                 x.EquiCategory)
            ; ("Description (medium)",          x.Description)
            ; ("Object type",              "")
            ; ("Gross Weight",              "")
            ; ("Unit of weight",              "")
            ; ("Start-up date",                 x.StartupDate |> showS4Date)
            ; ("AcquistnValue",              "")
            ; ("Currency",              "")
            ; ("Acquistion date",              "")
            ; ("Manufacturer",              "")
            ; ("Model number",              "")
            ; ("ManufPartNo.",              "")
            ; ("ManufSerialNo.",              "")
            ; ("ManufCountry",              "")
            ; ("ConstructYear",              "")
            ; ("ConstructMth",              "")
            ; ("Company Code",              "")         
            ; ("Functional loc.",               x.FunctionalLocation.ToString())
            ; ("Superord.Equip.",              "")
            ; ("Position",              "")
            ; ("TechIdentNo.",              "")
            ; ("Status of an object",              "")
            ; ("Status without stsno",              "")
            ] |> AssocList.ofList

    type MmopNewEquiMultilingualText = 
        { EquiId: string
          Description: string 
          LongText: string
        }
        member x.ToAssocs() = 
            [ ("Equipment",                     x.EquiId)
            ; ("Delete Indicator",              "")
            ; ("Language",                      "")
            ; ("Description (medium)",          x.Description)
            ; ("Long Text",                     x.LongText)
            ] |> AssocList.ofList

    type MmopEquiClassification = 
        { EquiId: string
          Class: string
          CharacteristicName: string
          CharacteristicValue: string
        }
        member x.ToAssocs() = 
            [ ("Equipment",                     x.EquiId)
            ; ("Delete Ind.",                   "")
            ; ("Class Type",                    "002")
            ; ("Class",                         x.Class)
            ; ("Status",                        "")
            ; ("Characteristics",               x.CharacteristicName)
            ; ("Char Value",                    x.CharacteristicValue)  
            ; ("Ch. Delete Ind.",               "")
            ] |> AssocList.ofList