// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Uxl


module Emitter =
    
    open FSharp.Core

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.Base.Uxl.FileTypes
    open AssetPatch.TemplatePatcher.Base.Hierarchy
    open AssetPatch.TemplatePatcher.Base.GenerateMonad
    open AssetPatch.TemplatePatcher.Uxl.Base
    


    type EquiData =
        { Equipment: EquimentData
          MultilingualText: EquiMultilingualText
          EquiClassifications: EquiClassification list
        }

    type MmopCreateData = 
        { ChangeRequests : ChangeRequestDetails list
          NewFuncLocs: FunctionalLocationData list
          NewFlocMultilingualTexts: FlocMultilingualText list
          NewFlocClassifications: FlocClassification list
          NewEquipments: EquimentData list
          NewEquiMultilingualTexts: EquiMultilingualText list
          NewEquiClassifications: EquiClassification list          
        }

        member x.IsEmpty
            with get () : bool = 
                x.ChangeRequests.IsEmpty 
                    && x.NewFuncLocs.IsEmpty
                    && x.NewFlocMultilingualTexts.IsEmpty 
                    && x.NewFlocClassifications.IsEmpty
                    && x.NewEquipments.IsEmpty
                    && x.NewEquiMultilingualTexts.IsEmpty
                    && x.NewEquiClassifications.IsEmpty
    
        member x.RemoveDups() : MmopCreateData = 
            let flocClassDistinctKey (v: FlocClassification) : string = 
                v.FunctionalLocation.ToString() + "!!" + v.Class + "!!" + v.CharacteristicName + "!!" + v.CharacteristicValue

            let equiClassDistinctKey (v: EquiClassification) : string = 
                v.EquipmentId + "!!" + v.Class + "!!" + v.CharacteristicName + "!!" + v.CharacteristicValue

            { ChangeRequests = x.ChangeRequests |> List.distinctBy (fun v -> v.SortKey)
              NewFuncLocs = x.NewFuncLocs |> List.distinctBy (fun v -> v.FunctionalLocation.ToString())
              NewFlocMultilingualTexts = x.NewFlocMultilingualTexts |> List.distinctBy (fun v -> v.FunctionalLocation.ToString())
              NewFlocClassifications = x.NewFlocClassifications |> List.distinctBy flocClassDistinctKey
              NewEquipments = x.NewEquipments |> List.distinctBy (fun v -> v.EquipmentId)
              NewEquiMultilingualTexts = x.NewEquiMultilingualTexts |> List.distinctBy (fun v -> v.EquipmentId)
              NewEquiClassifications = x.NewEquiClassifications |> List.distinctBy equiClassDistinctKey
             }

        static member Concat (source : MmopCreateData list) : MmopCreateData = 
            { ChangeRequests            = source |> List.map (fun x -> x.ChangeRequests) |> List.concat
              NewFuncLocs               = source |> List.map (fun x -> x.NewFuncLocs) |> List.concat
              NewFlocMultilingualTexts  = source |> List.map (fun x -> x.NewFlocMultilingualTexts) |> List.concat
              NewFlocClassifications    = source |> List.map (fun x -> x.NewFlocClassifications) |> List.concat
              NewEquipments             = source |> List.map (fun x -> x.NewEquipments) |> List.concat
              NewEquiMultilingualTexts  = source |> List.map (fun x -> x.NewEquiMultilingualTexts) |> List.concat
              NewEquiClassifications    = source |> List.map (fun x -> x.NewEquiClassifications) |> List.concat
            }

    
    // ************************************************************************
    // Translation

    

    let private makeChangeRequestDetails (props: ChangeRequestProperties) (item: Choice<FuncLocPath, EquipmentId>) : ChangeRequestDetails = 
            { DescriptionLong = props.Description
              Priority = ""
              DueDate = None
              Reason = ""
              TypeOfChangeRequest = props.ChangeType
              ChangeRequestGroup = ""
              FuncLocOrEquipment = item
              ProcessRequester = props.Requester
            }
        
    // ************************************************************************
    // Translation - equipment

    let equiClassification (source: S4EquiClassification) : EquiClassification = 
        { EquipmentId = source.EquiId
          ClassDeleteInd = false
          Class = source.ClassName
          Status = ""
          CharacteristicName = source.CharName
          CharacteristicValue = source.CharValue.UxlValue
          CharDeleteInd = false
        }


    let makeEquiMultilingualText (equiId: string)
                                    (description: string)
                                    (longText : string) : EquiMultilingualText = 
        { EquipmentId = equiId
          DeleteIndicator = false
          Language = ""
          DescriptionMedium = description
          LongText = longText
        }

    let private makeEquipmentData1 (equipment : S4Equipment) : EquimentData = 
        let superOrd = Option.defaultValue "" equipment.SuperEquiId
        { _SortKey = equipmentSortKey equipment.EquiId superOrd
          EquipmentId = equipment.EquiId
          EquipCategory = equipment.Category
          DescriptionMedium = equipment.Description
          ObjectType = equipment.ObjectType
          StartupDate = Some equipment.StartupDate
          Manufacturer = Option.defaultValue "TO BE DETERMINED" equipment.Manufacturer
          ModelNumber = Option.defaultValue "TO BE DETERMINED" equipment.Model
          ManufPartNum = ""
          ManufSerialNum = Option.defaultValue "UNKNOWN" equipment.SerialNumber
          CompanyCode = ""
          ConstructionYear = Some equipment.StartupDate.Year
          ConstructionMonth = Some equipment.StartupDate.Month
          FunctionalLocation = equipment.FuncLoc.ToString()
          SuperordEquip = superOrd
          StatusOfAnObject = ""
          StatusWithoutStatusNum = ""
        }

    let private makeEquiData (source : S4Equipment) : EquiData = 
        { Equipment = makeEquipmentData1 source 
          MultilingualText = makeEquiMultilingualText source.EquiId "" source.MemoLine
          EquiClassifications = 
              List.map equiClassification source.Classifications 
        }

    /// Recursive version of makeMmopNewEqui1
    let makeEquiDataWithKids (source : S4Equipment) : EquiData list = 
        let rec work kids cont = 
            match kids with
            | [] -> cont []
            | (x :: xs) -> 
                let v1 = makeEquiData x 
                work kids (fun vs -> cont (v1 :: vs))
        let equi = makeEquiData source             
        work source.SuboridinateEquipment (fun xs -> (equi :: xs))

    let private equipmentToMmopCreateData (props: ChangeRequestProperties)
                                            (source : S4Equipment) : MmopCreateData = 
        
        let equiData = makeEquiDataWithKids source
        let equiIds = 
            equiData|> List.map (fun (x: EquiData) -> Choice2Of2 x.Equipment.EquipmentId)
        let changeRequests = List.map (makeChangeRequestDetails props) equiIds
        { 
            ChangeRequests = changeRequests
            NewFuncLocs = []
            NewFlocMultilingualTexts = []
            NewFlocClassifications = []
            NewEquipments = 
                equiData|> List.map (fun (x: EquiData) -> x.Equipment)

            NewEquiMultilingualTexts = 
                equiData|> List.map (fun (x: EquiData) -> x.MultilingualText)

            NewEquiClassifications = 
                equiData|> List.map (fun (x: EquiData) -> x.EquiClassifications) |> List.concat
        }
    
    // ************************************************************************
    // Translation - Func locs
    
    let flocClassification (source: S4FlocClassification) : FlocClassification = 
           { FunctionalLocation = source.FuncLoc
             ClassDeletionInd = None
             Class = source.ClassName
             Status = ""
             CharacteristicName = source.CharName
             CharacteristicValue = source.CharValue.UxlValue
             CharDeletionInd = None
           }

    // No multilingual text for flocs (may change)

    let makeFunctionalLocationData (path : FuncLocPath) 
                                    (props : FuncLocProperties)
                                    (description : string) 
                                    (objectType : string)  : FunctionalLocationData = 
        { FunctionalLocation = path
          DescriptionMedium = description
          FunLocCategory = Some path.Level
          StructureIndicator = props.StructureIndicator
          ObjectType = objectType
          StartupDate = Some props.StartupDate
          ConstructionYear = Some props.StartupDate.Year
          ConstructionMonth = Some props.StartupDate.Month
          CompanyCode = ""
          SuperiorFuncLoc = 
                match parent path with 
                | None -> ""
                | Some x -> x.ToString()
          EquipInstall = path.Level >= 5 |> Some
          StatusOfAnObject = ""
          StatusWithoutStatusNum = ""
        }

    let private funclocToMmopCreateData (props: ChangeRequestProperties) 
                                        (path : FuncLocPath) 
                                        (flocProps : FuncLocProperties)
                                        (description : string) 
                                        (objectType : string)
                                        (classes : S4FlocClassification list) : MmopCreateData = 
        let floc = makeFunctionalLocationData path flocProps description objectType
        let classChars : FlocClassification list = List.map flocClassification classes 
        let changeRequest = makeChangeRequestDetails props (Choice1Of2 path)
        { ChangeRequests = [changeRequest]
          NewFuncLocs = [floc]
          NewFlocMultilingualTexts = []
          NewFlocClassifications = classChars
          NewEquipments = []
          NewEquiMultilingualTexts = []
          NewEquiClassifications = []   
        }

    let private flocClassesToMmopCreateData (props: ChangeRequestProperties) 
                                            (classes : S4FlocClassification list) : MmopCreateData = 
        let classChars : FlocClassification list = List.map flocClassification classes 
        let changeRequests = 
            classes |> List.map (fun (x: S4FlocClassification) -> makeChangeRequestDetails props (Choice1Of2 x.FuncLoc))
        { ChangeRequests = changeRequests
          NewFuncLocs = []
          NewFlocMultilingualTexts = []
          NewFlocClassifications = classChars
          NewEquipments = []
          NewEquiMultilingualTexts = []
          NewEquiClassifications = []   
        }

    let private equiClassesToMmopCreateData (props: ChangeRequestProperties) 
                                            (classes : S4EquiClassification list) : MmopCreateData = 
        let classChars : EquiClassification list = List.map equiClassification classes 
        let changeRequests = 
            classes |> List.map (fun (x: S4EquiClassification) -> makeChangeRequestDetails props (Choice2Of2 x.EquiId))
        { ChangeRequests = changeRequests
          NewFuncLocs = []
          NewFlocMultilingualTexts = []
          NewFlocClassifications = []
          NewEquipments = []
          NewEquiMultilingualTexts = []
          NewEquiClassifications = classChars   
        }


    // ************************************************************************
    // User API

    let flocClassificationsEmitMmopCreate (source : S4FlocClassification list) : UxlGenerate<MmopCreateData> = 
        generate { 
            let! props = askChangeRequestProperties()
            return flocClassesToMmopCreateData props source 
        }

    let equiClassificationsEmitMmopCreate (source : S4EquiClassification list) : UxlGenerate<MmopCreateData> = 
        generate { 
            let! props = askChangeRequestProperties()
            return equiClassesToMmopCreateData props source 
        }

    let equipmentEmitMmopCreate (source : S4Equipment) : UxlGenerate<MmopCreateData> = 
        generate { 
            let! props = askChangeRequestProperties()
            return equipmentToMmopCreateData props source
        }
    
    let equipmentListEmitMmopCreate (source : S4Equipment list) : UxlGenerate<MmopCreateData> = 
        mapM equipmentEmitMmopCreate source |>> MmopCreateData.Concat

    let functionalLocationEmitMmopCreate (source : S4FunctionalLocation) : UxlGenerate<MmopCreateData> = 
        let create1 (src : S4FunctionalLocation) = 
            generate {
                let! d1 = equipmentListEmitMmopCreate source.Equipment
                do! liftAction (fun () -> printfn "source.Equipment.Length %i" source.Equipment.Length)
                let! props = askChangeRequestProperties ()
                let d2 = funclocToMmopCreateData props src.FuncLoc src.FlocProperties 
                                    src.Description src.ObjectType src.Classifications
                return MmopCreateData.Concat [d1; d2]
            }

        let rec work xs cont = 
            match xs with 
            | [] -> cont []
            | x :: rs -> 
                generate { 
                    let! v1 = create1 x 
                    let kids = x.SubFlocs
                    return! work (kids @ rs) (fun vs -> cont (v1 :: vs))
                }
        
        generate {
            let! d1 = create1 source
            let! ds = work source.SubFlocs (fun xs -> mreturn xs)
            return MmopCreateData.Concat (d1 :: ds)
        }


