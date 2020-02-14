// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.Base.Uxl


module FileTypes =
    
    open System

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.CsvFile
    open AssetPatch.Base.FuncLocPath

    // Common type to generate Create (from Templates) and Rewrite files.
    // Generally "cell values" should permit no value / empty as Rewrite files
    // only specify what has changed.


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
    /// `FL-Functional Location` and `EQ-Equipment` are coalesced as we must 
    /// have one or the other.
    type ChangeRequestDetails = 
        { DescriptionLong: string
          Priority: string
          DueDate: DateTime option
          Reason: string
          TypeOfChangeRequest: string
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
            ; ("Type of Change Request",        x.TypeOfChangeRequest)
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
          ConstructionYear: int option
          ConstructionMonth: int option
          CompanyCode: string
          SuperiorFuncLoc: string   // this is only printed, so it is a string
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
            ; ("Start-up date",                 optionalS4Date x.StartupDate)
            ; ("Currency",                      "")
            ; ("Acquistion date",               "")
            ; ("ConstructYear",                 optionalInt x.ConstructionYear)
            ; ("ConstructMth",                  optionalInt x.ConstructionMonth)
            ; ("Company Code",                  x.CompanyCode)
            ; ("Position",                      "")
            ; ("SupFunctLoc.",                  x.SuperiorFuncLoc)
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
            ; ("Delete Indicator",              showS4Bool x.DeleteIndicator)
            ; ("Language",                      x.Language)
            ; ("Description",                   x.Description) 
            ; ("Long Text",                     x.LongText)
            ] |> AssocList.ofList

    /// ClassType is always "003"
    type FlocClassification = 
        { FunctionalLocation: FuncLocPath
          ClassDeletionInd: bool option
          Class: string
          Status: string
          CharacteristicName: string
          CharacteristicValue: string
          CharDeletionInd: bool option
        }
        member x.ToAssocs() = 
            [ ("Functional Location",           x.FunctionalLocation.ToString())
            ; ("Deletion Ind",                  optionalBool x.ClassDeletionInd)
            ; ("Class Type",                    "003")
            ; ("Class",                         x.Class)
            ; ("Status",                        x.Status)
            ; ("Characteristics",               x.CharacteristicName)
            ; ("Char Value",                    x.CharacteristicValue)
            ; ("Ch.Deletion Ind.",              optionalBool x.CharDeletionInd)
            ] |> AssocList.ofList


    type EquimentData = 
        { EquipmentId: EquipmentId
          EquipCategory: string  
          DescriptionMedium: string 
          ObjectType: string
          StartupDate: DateTime option
          Manufacturer: string
          ModelNumber: string
          ManufPartNum: string
          ManufSerialNum: string
          ConstructionYear: int option
          ConstructionMonth: int option
          CompanyCode: string
          FunctionalLocation: string // this is only printed, so it is a string
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
            ; ("Start-up date",                 optionalS4Date x.StartupDate)
            ; ("AcquistnValue",                 "")
            ; ("Currency",                      "")
            ; ("Acquistion date",               "")
            ; ("Manufacturer",                  x.Manufacturer)
            ; ("Model number",                  x.ModelNumber)
            ; ("ManufPartNo.",                  x.ManufPartNum)
            ; ("ManufSerialNo.",                unknownIfBlank x.ManufSerialNum)
            ; ("ManufCountry",                  "")
            ; ("ConstructYear",                 optionalInt x.ConstructionYear)
            ; ("ConstructMth",                  optionalInt x.ConstructionMonth)
            ; ("Company Code",                  x.CompanyCode)         
            ; ("Functional loc.",               x.FunctionalLocation)
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



    let private makeCsvFile (rows : AssocList<string, string> list) : Result<CsvFileWithHeaders, string> = 
        match Seq.tryHead rows with
        | None -> Error "makeCsv - empty"
        | Some row1 ->
            let headers = AssocList.keys row1
            let hs = List.ofArray headers
            let csvRows = List.map (AssocList.values << AssocList.select hs) rows |> List.toArray
            Ok { Headers = headers; Rows = csvRows }


    // ************************************************************************
    // Change Request Details csv

    /// Render data for the `Change Request Details` tab
    let private makeChangeRequestDetails (rows : ChangeRequestDetails list) : Result<CsvFileWithHeaders, string> = 
        rows
            |> List.sortBy (fun (x: ChangeRequestDetails) -> x.SortKey)
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write data for the `Change Request Details` tab
    /// The input list should not be empty (any change needs 
    /// this data filling out).
    let writeChangeRequestDetails (changeRequests : ChangeRequestDetails list)
                                    (outPath: string) : Result<unit, string> = 

        match changeRequests with
        | [] -> Error "writeChangeRequestDetails - empty input"
        | _ -> 
            makeChangeRequestDetails changeRequests 
                |> Result.map (fun v -> writeCsvFile csvDefaults v outPath)

            
    // ************************************************************************
    // FuncLocs csv

    /// Render data for the `Functional Location Data` tab
    let private makeFunctionalLocationData (rows : FunctionalLocationData list) : Result<CsvFileWithHeaders, string> = 
        rows
            |> List.sortBy (fun row -> row.FunctionalLocation.ToString()) 
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write data for the `Functional Location Data` tab
    let writeFunctionalLocationData (source : FunctionalLocationData list)
                                (outPath: string) : Result<unit, string> = 
        match source with
        | [] ->  Ok ()
        | _ -> 
            makeFunctionalLocationData source
                |> Result.map (fun v -> writeCsvFile csvDefaults v outPath)


    // ************************************************************************
    // FuncLocs multilingual text csv

    /// Render data for the `FL-Multilingual Text` tab
    let private makeFlocMultilingualText (rows : FlocMultilingualText list) : Result<CsvFileWithHeaders, string> = 
        rows
            |> List.sortBy (fun row -> row.FunctionalLocation.ToString()) 
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write data for the `FL-Multilingual Text` tab
    let writeFlocMultilingualText (source : FlocMultilingualText list)
                                    (outPath: string) : Result<unit, string> = 
        match source with
        | [] -> Ok ()
        | _ -> 
            makeFlocMultilingualText source
                |> Result.map (fun v -> writeCsvFile csvDefaults v outPath)
            

    // ************************************************************************
    // FuncLocs class/characteristics csv

    /// Render data for the `FL-Classification` tab
    let private makeFlocClassification (rows : FlocClassification list) : Result<CsvFileWithHeaders, string> = 
        rows
            |> List.sortBy (fun row -> row.FunctionalLocation.ToString()) 
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write data for the `FL-Classification` tab
    let writeFlocClassification (source : FlocClassification list)
                                        (outPath: string) : Result<unit, string> = 
        match source with
        | [] -> Ok ()
        | _ -> 
            makeFlocClassification source
                |> Result.map (fun v -> writeCsvFile csvDefaults v outPath)


    // ************************************************************************
    // Equipment csv

    /// Render data for the `Equipment Data` tab
    let private makeEquipmentData (rows : EquimentData list) : Result<CsvFileWithHeaders, string> = 
        rows
            |> List.sortBy (fun row -> row.EquipmentId) 
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write data for the `Equipment Data` tab
    let writeEquipmentData (source : EquimentData list)
                                (outPath: string) : Result<unit, string> = 
        match source with
        | [] -> Ok ()
        | _ -> 
            makeEquipmentData source
                |> Result.map (fun v -> writeCsvFile csvDefaults v outPath)


    // ************************************************************************
    // Equipment multilingual csv

    /// Render data for the `EQ-Multilingual Text` tab
    let private makeEquiMultilingualText (rows : EquiMultilingualText list) : Result<CsvFileWithHeaders, string> = 
        rows
            |> List.sortBy (fun row -> row.EquipmentId) 
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write data for the `EQ-Multilingual Text` tab
    let writeEquiMultilingualText (source : EquiMultilingualText list)
                                    (outPath: string) : Result<unit, string> = 

        match source with
        | [] -> Ok ()
        | _ -> 
            makeEquiMultilingualText source
                |> Result.map (fun v -> writeCsvFile csvDefaults v outPath)
            


    // ************************************************************************
    // Equipment class/characteristics csv

    /// Render data for the `FL-Classification` tab
    let private makeEquiClassification (rows : EquiClassification list) : Result<CsvFileWithHeaders, string> = 
        rows
            |> List.sortBy (fun row -> row.EquipmentId) 
            |> List.map (fun x -> x.ToAssocs())      
            |> makeCsvFile 

    /// Write data for the EQ-Classification` tab
    let writeEquiClassification (source : EquiClassification list)
                                (outPath: string) : Result<unit, string> = 
        match source with
        | [] -> Ok ()
        | _ -> 
            makeEquiClassification source
                |> Result.map (fun v -> writeCsvFile csvDefaults v outPath)
            