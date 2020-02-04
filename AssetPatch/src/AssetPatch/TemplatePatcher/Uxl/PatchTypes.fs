// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Uxl


module PatchTypes =
    
    open System

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath

    
    type EquipmentId = string


    let private optionalDate (source: DateTime option):  string = 
        match source with
        | None -> ""
        | Some dt -> showS4Date dt

    let private optionalInt (source: int option):  string = 
        match source with
        | None -> ""
        | Some i -> i.ToString()
        
    let private optionalFloc (source: FuncLocPath option):  string = 
        match source with
        | Some floc -> floc.ToString()
        | None -> ""

    let private choice1Floc (source: Choice<FuncLocPath, EquipmentId>):  string = 
        match source with
        | Choice1Of2 floc -> floc.ToString()
        | Choice2Of2 _ -> ""

    let private choice2Equi (source: Choice<FuncLocPath, EquipmentId>):  string = 
        match source with
        | Choice1Of2 _ -> ""
        | Choice2Of2 equiId -> equiId

    let private optionalString (source: string option):  string = 
         Option.defaultValue "" source

    let private optionalBool (source: bool option):  string = 
        match source with
        | Some true -> "X"
        | _ -> ""

    /// This represents one row (MMOP configuration)
    /// MBOM-* fields are left out
    type ChangeRequestDetails = 
        { DescriptionLong: string
          Priority: string
          DueDate: DateTime option
          Reason: string
          ChangeRequestType: string
          ChangeRequestGroup: string
          FuncLocOrEquipment:  Choice<FuncLocPath, EquipmentId>
          ProcessRequester: string
        }
        member x.SortKey
            with get(): string = 
                match x.FuncLocOrEquipment with
                | Choice1Of2 floc -> "A:" + floc.ToString()
                | Choice2Of2 equiId -> "B:" + equiId
                
        member x.ToAssocs() = 
            [ ("Description (Long)",            x.DescriptionLong) 
            ; ("Priority",                      x.Priority)
            ; ("Due Date",                      optionalDate x.DueDate)
            ; ("Reason",                        x.Reason)
            ; ("Type of Change Request",        x.ChangeRequestType)
            ; ("Change Request Group",          x.ChangeRequestGroup)
            ; ("MBOM-Material",                 "")
            ; ("MBOM-Plant",                    "")
            ; ("MBOM-Usage",                    "")            
            ; ("MBOM-Alternative",              "")
            ; ("FL-Functional Location",        choice1Floc x.FuncLocOrEquipment)
            ; ("EQ-Equipment",                  choice2Equi x.FuncLocOrEquipment)
            ; ("Process Requestor",             x.ProcessRequester)
            ] |> AssocList.ofList


    type FunctionalLocationData = 
        { FunctionalLocation: FuncLocPath
          DescriptionMedium: string 
          FunLocCategory: int option
          StructureIndicator: string          
          ObjectType: string
          StartupDate: DateTime option
          ConstructYear: int option
          ConstructMonth: int option
          SuperiorFuncLoc: FuncLocPath option
          EquipInstall: bool option
          StatusOfAnObject: string
          StatusWithoutStatusNum: string
        }
  
        member x.ToAssocs() = 
            [ ("Functional Location",           x.FunctionalLocation.ToString())
            ; ("Masked Func Loc",               x.FunctionalLocation.ToString())
            ; ("Description (medium)",          x.DescriptionMedium)
            ; ("FunctLocCat.",                  optionalInt x.FunLocCategory)
            ; ("StrIndicator",                  x.StructureIndicator)
            ; ("Object type",                   x.ObjectType)
            ; ("Gross Weight",                  "")
            ; ("Unit of weight",                "")
            ; ("Start-up date",                 optionalDate x.StartupDate)
            ; ("Currency",                      "")
            ; ("Acquistion date",               "")
            ; ("ConstructYear",                 optionalInt x.ConstructYear)
            ; ("ConstructMth",                  optionalInt x.ConstructMonth)
            ; ("Company Code",                  "")
            ; ("Position",                      "")
            ; ("SupFunctLoc.",                  optionalFloc x.SuperiorFuncLoc)
            ; ("EquipInstall.",                 optionalBool x.EquipInstall)
            ; ("Status of an object",           x.StatusOfAnObject)
            ; ("Status without stsno",          x.StatusWithoutStatusNum)
            ] |> AssocList.ofList


    type FlocMultilingualText = 
        { FunctionalLocation: FuncLocPath
          DeleteIndicator: bool
          Language: string
          Description: string 
          LongText: string
        }
        member x.ToAssocs() = 
            [ ("Functional Location",           x.FunctionalLocation.ToString())
            ; ("Delete Indicator",              if x.DeleteIndicator then "X" else "")            
            ; ("Language",                      x.Language)
            ; ("Description",                   x.Description) 
            ; ("Long Text",                     x.LongText)
            ] |> AssocList.ofList


    type FlocClassification = 
        { FunctionalLocation: FuncLocPath
          ClassDeletionInd: bool
          Class: string
          Status: string
          CharacteristicName: string
          CharacteristicValue: string
          CharDeletionInd: bool
        }
        member x.ToAssocs() = 
            [ ("Functional Location",           x.FunctionalLocation.ToString())
            ; ("Deletion Ind",                  showS4Bool x.ClassDeletionInd)
            ; ("Class Type",                    "003")
            ; ("Class",                         x.Class)
            ; ("Status",                        x.Status)
            ; ("Characteristics",               x.CharacteristicName)
            ; ("Char Value",                    x.CharacteristicValue)
            ; ("Ch.Deletion Ind.",              showS4Bool x.CharDeletionInd)
            ] |> AssocList.ofList


    type EquimentData = 
        { EquipmentId: EquipmentId
          EquipCategory: string  
          DescriptionMedium: string 
          ObjectType: string
          StartupDate: DateTime option
          Manufacturer: string
          ModelNumber: string
          ManufPartNumber: string
          ManufSerialNumber: string
          ConstructionYear: int option
          ConstructionMonth: int option
          CompanyCode: string
          FunctionalLocation: FuncLocPath option
          SuperordEquip: string
          StatusOfAnObject: string
          StatusWithoutStatusNum: string
        }
        member x.ToAssocs() = 
            [ ("Equiment",                      x.EquipmentId)
            ; ("EquipCategory",                 x.EquipCategory)
            ; ("Description (medium)",          x.DescriptionMedium)
            ; ("Object type",                   x.ObjectType)
            ; ("Gross Weight",                  "")
            ; ("Unit of weight",                "")
            ; ("Start-up date",                 optionalDate x.StartupDate)
            ; ("AcquistnValue",                 "")
            ; ("Currency",                      "")
            ; ("Acquistion date",               "")
            ; ("Manufacturer",                  x.Manufacturer)
            ; ("Model number",                  x.ModelNumber)
            ; ("ManufPartNo.",                  x.ManufPartNumber)
            ; ("ManufSerialNo.",                x.ManufSerialNumber)
            ; ("ManufCountry",                  "")
            ; ("ConstructYear",                 optionalInt x.ConstructionYear)
            ; ("ConstructMth",                  optionalInt x.ConstructionMonth)
            ; ("Company Code",                  x.CompanyCode)         
            ; ("Functional loc.",               optionalFloc x.FunctionalLocation)
            ; ("Superord.Equip.",               x.SuperordEquip)
            ; ("Position",                      "")
            ; ("TechIdentNo.",                  "")
            ; ("Status of an object",           x.StatusOfAnObject)
            ; ("Status without stsno",          x.StatusWithoutStatusNum)
            ] |> AssocList.ofList

    type EquiMultilingualText = 
        { EquipmentId: string
          DeleteIndicator: bool
          Language: string
          DescriptionMedium: string 
          LongText: string
        }
        member x.ToAssocs() = 
            [ ("Equipment",                     x.EquipmentId)
            ; ("Delete Indicator",              if x.DeleteIndicator then "X" else "")
            ; ("Language",                      x.Language)
            ; ("Description (medium)",          x.DescriptionMedium) 
            ; ("Long Text",                     x.LongText)
            ] |> AssocList.ofList

    type EquiClassification = 
        { EquipmentId: EquipmentId
          ClassDeleteInd: bool
          Class: string
          Status: string
          CharacteristicName: string
          CharacteristicValue: string
          CharDeleteInd: bool
        }
        member x.ToAssocs() = 
            [ ("Equipment",                     x.EquipmentId)
            ; ("Delete Ind.",                   showS4Bool x.ClassDeleteInd)
            ; ("Class Type",                    "002")
            ; ("Class",                         x.Class)
            ; ("Status",                        x.Status)
            ; ("Characteristics",               x.CharacteristicName)
            ; ("Char Value",                    x.CharacteristicValue)  
            ; ("Ch. Delete Ind.",               showS4Bool x.CharDeleteInd)
            ] |> AssocList.ofList



