﻿// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Uxl


module Emitter =
    
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.Base.TemplateHierarchy
    open AssetPatch.TemplatePatcher.Base.CompilerMonad
    
    open AssetPatch.TemplatePatcher.Uxl.Base
    open AssetPatch.TemplatePatcher.Uxl.PatchTypes

    type EquiData =
        { Equipment: MmopNewEqui
          MultilingualText: MmopNewEquiMultilingualText
          EquiClassifications: MmopEquiClassification list
        }

    type MmopCreateData = 
        { ChangeRequests : MmopChangeRequest list
          NewFuncLocs: MmopNewFuncLoc list
          NewFlocMultilingualTexts: MmopNewFlocMultilingualText list
          NewFlocClassifications: MmopFlocClassification list
          NewEquipments: MmopNewEqui list
          NewEquiMultilingualTexts: MmopNewEquiMultilingualText list
          NewEquiClassifications: MmopEquiClassification list          
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
            let flocClassDistinctKey (v: MmopFlocClassification) : string = 
                v.FunctionalLocation.ToString() + "!!" + v.Class + "!!" + v.CharacteristicName + "!!" + v.CharacteristicValue

            let equiClassDistinctKey (v: MmopEquiClassification) : string = 
                v.EquiId + "!!" + v.Class + "!!" + v.CharacteristicName + "!!" + v.CharacteristicValue

            { ChangeRequests = x.ChangeRequests |> List.distinctBy (fun v -> v.SortKey)
              NewFuncLocs = x.NewFuncLocs |> List.distinctBy (fun v -> v.FunctionalLocation.ToString())
              NewFlocMultilingualTexts = x.NewFlocMultilingualTexts |> List.distinctBy (fun v -> v.FunctionalLocation.ToString())
              NewFlocClassifications = x.NewFlocClassifications |> List.distinctBy flocClassDistinctKey
              NewEquipments = x.NewEquipments |> List.distinctBy (fun v -> v.EquiId)
              NewEquiMultilingualTexts = x.NewEquiMultilingualTexts |> List.distinctBy (fun v -> v.EquiId)
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

    
    let makeMmopChangeRequestFloc (floc: FuncLocPath) : UxlCompilerMonad<MmopChangeRequest> = 
        compile {
            let! descr = getChangeRequestDescription ()
            let! changeType = getChangeRequestType ()
            let! requester = getProcessRequester ()
            return { 
                Description = descr
                ChangeRequestType = changeType
                FunctionalLocation = Some floc
                EquipmentId = None
                ProcessRequestor = requester
                }
        }

    let makeMmopChangeRequestEqui (equiId: string) : UxlCompilerMonad<MmopChangeRequest> = 
        compile {
            let! descr = getChangeRequestDescription ()
            let! changeType = getChangeRequestType ()
            let! requester = getProcessRequester ()
            return { 
                Description = descr
                ChangeRequestType = changeType
                FunctionalLocation = None
                EquipmentId = Some equiId
                ProcessRequestor = requester
                }
        }

    let makeMmopFlocClassification (funcLoc : FuncLocPath)
                                    (className: string)
                                    (charac : S4Characteristic) : MmopFlocClassification = 
        { FunctionalLocation = funcLoc
          Class = className
          CharacteristicName = charac.Name
          CharacteristicValue = charac.Value.UxlValue
        }

    let makeMmopFlocClassifications (funcLoc : FuncLocPath)
                                    (flocClass : S4Class) : MmopFlocClassification list = 
        List.map (makeMmopFlocClassification funcLoc flocClass.ClassName) flocClass.Characteristics


    let makeMmopEquiMultilingualText (equiId: string)
                                     (description: string)
                                     (longText : string) : MmopNewEquiMultilingualText = 
        { EquiId = equiId
          Description = description
          LongText = longText
        }


    let makeMmopEquiClassification (equiId: string)
                                    (className: string)
                                    (charac : S4Characteristic) : MmopEquiClassification = 
        { EquiId = equiId
          Class = className
          CharacteristicName = charac.Name
          CharacteristicValue = charac.Value.UxlValue
        }

    let makeMmopEquiClassifications (equiId: string)
                                    (equiClass : S4Class) : MmopEquiClassification list = 
        List.map (makeMmopEquiClassification equiId equiClass.ClassName) equiClass.Characteristics

    let private makeMmopNewEqui1 (equiId: string) (equipment : S4Equipment) : MmopNewEqui = 
        { EquiId = equiId
          EquiCategory = equipment.Category
          Description = equipment.Description
          StartupDate = equipment.FlocProperties.StartupDate
          Manufacturer = Option.defaultValue "TO BE DETERMINED" equipment.Manufacturer
          Model = Option.defaultValue "TO BE DETERMINED" equipment.Model
          SerialNumber = Option.defaultValue "UNKNOWN" equipment.SerialNumber
          FunctionalLocation = equipment.FuncLoc
        }

    let private makeEquiData (equiId: string) (source : S4Equipment) : EquiData = 
        { Equipment = makeMmopNewEqui1 equiId source 
          MultilingualText = makeMmopEquiMultilingualText equiId "" source.MemoLine
          EquiClassifications = 
              List.map (makeMmopEquiClassifications equiId) source.Classes |> List.concat
        }



    /// Recursive version of makeMmopNewEqui1
    let makeEquiDataWithKids (source : S4Equipment) : UxlCompilerMonad<EquiData list> = 
            let rec work kids cont = 
                match kids with
                | [] -> cont []
                | (x :: xs) -> 
                    newEquimentId () >>= fun equiId -> 
                    let v1 = makeEquiData equiId x 
                    work kids (fun vs -> cont (v1 :: vs))
            newEquimentId () >>= fun equiId -> 
            let equi = makeEquiData equiId source             
            work source.SuboridnateEquipment (fun xs -> mreturn (equi :: xs))

    let private equipmentToMmopCreateData (source : S4Equipment) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            let! equiData = makeEquiDataWithKids source
            let equiIds = 
                equiData|> List.map (fun (x: EquiData) -> x.Equipment.EquiId)
            let! changeRequests = 
                mapM makeMmopChangeRequestEqui equiIds
            return { 
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
        }


    let makeMmopNewFuncLoc (path : FuncLocPath) 
                        (props : FuncLocProperties)
                        (description : string) 
                        (objectType : string)  : MmopNewFuncLoc = 
        { FunctionalLocation = path
          Description = description
          FunLocCategory = path.Level
          StructureIndicator = props.StructureIndicator
          StartupDate = props.StartupDate
          ObjectType = objectType
        }



    let private funclocToMmopCreateData (path : FuncLocPath) 
                                        (props : FuncLocProperties)
                                        (description : string) 
                                        (objectType : string)
                                        (classes : S4Class list) : UxlCompilerMonad<MmopCreateData> = 
        let collect xs = List.foldBack (fun (c1, vs1)  (cs,vs) -> (c1 ::cs, vs1 @ vs)) xs ([],[])
        compile {
            let floc = makeMmopNewFuncLoc path props description objectType
            let classChars : MmopFlocClassification list = 
                List.map (makeMmopFlocClassifications path) classes |> List.concat
            let! changeRequest = makeMmopChangeRequestFloc path
            return { 
                ChangeRequests = [changeRequest]
                NewFuncLocs = [floc]
                NewFlocMultilingualTexts = []
                NewFlocClassifications = classChars
                NewEquipments = []
                NewEquiMultilingualTexts = []
                NewEquiClassifications = []   
            }
        }

    // ************************************************************************
    // User API

    let equipmentEmitMmopCreate (source : S4Equipment) : UxlCompilerMonad<MmopCreateData> = 
        equipmentToMmopCreateData source

    
    let equipmentListEmitMmopCreate (source : S4Equipment list) : UxlCompilerMonad<MmopCreateData> = 
        mapM equipmentEmitMmopCreate source |>> MmopCreateData.Concat

    let componentEmitMmopCreate (source : S4Component) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            let! flocMcd = 
                funclocToMmopCreateData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! equiMcd = equipmentListEmitMmopCreate source.Equipment
            return MmopCreateData.Concat [flocMcd; equiMcd]
        }

    let componentListEmitMmopCreate (source : S4Component list) : UxlCompilerMonad<MmopCreateData> = 
        mapM componentEmitMmopCreate source |>> MmopCreateData.Concat

    let itemEmitMmopCreate (source : S4Item) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            let! flocMcd1 = 
                funclocToMmopCreateData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! flocMcdSubs = componentListEmitMmopCreate source.Components
            let! equiMcd = equipmentListEmitMmopCreate source.Equipment
            return MmopCreateData.Concat [flocMcd1; flocMcdSubs; equiMcd]
        }

    let itemListEmitMmopCreate (source : S4Item list) : UxlCompilerMonad<MmopCreateData> = 
        mapM itemEmitMmopCreate source |>> MmopCreateData.Concat

    let assemblyEmitMmopCreate (source : S4Assembly) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            let! flocMcd1 = 
                funclocToMmopCreateData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! flocMcdSubs = itemListEmitMmopCreate source.Items
            let! equiMcd = equipmentListEmitMmopCreate source.Equipment
            return MmopCreateData.Concat [flocMcd1; flocMcdSubs; equiMcd]
        }

    let assemblyListEmitMmopCreate (source : S4Assembly list) : UxlCompilerMonad<MmopCreateData> = 
        mapM assemblyEmitMmopCreate source |>> MmopCreateData.Concat

    let systemEmitMmopCreate (source : S4System) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            let! flocMcd1 = 
                funclocToMmopCreateData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! flocMcdSubs = assemblyListEmitMmopCreate source.Assemblies
            let! equiMcd = equipmentListEmitMmopCreate source.Equipment
            return MmopCreateData.Concat [flocMcd1; flocMcdSubs; equiMcd]
        }

    let systemListEmitMmopCreate (source : S4System list) : UxlCompilerMonad<MmopCreateData> = 
        mapM systemEmitMmopCreate source |>> MmopCreateData.Concat

    let processEmitMmopCreate (source : S4Process) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            let! flocMcd1 = 
                funclocToMmopCreateData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! flocMcdSubs = systemListEmitMmopCreate source.Systems
            return MmopCreateData.Concat [flocMcd1; flocMcdSubs]
        }

    let processListEmitMmopCreate (source : S4Process list) : UxlCompilerMonad<MmopCreateData> = 
        mapM processEmitMmopCreate source |>> MmopCreateData.Concat

    let processGroupEmitMmopCreate (source : S4ProcessGroup) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            let! flocMcd1 = 
                funclocToMmopCreateData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! flocMcdSubs = processListEmitMmopCreate source.Processes
            return MmopCreateData.Concat [flocMcd1; flocMcdSubs]
        }

    let processGroupListEmitMmopCreate (source : S4ProcessGroup list) : UxlCompilerMonad<MmopCreateData> = 
        mapM processGroupEmitMmopCreate source |>> MmopCreateData.Concat

    let functionEmitMmopCreate (source : S4Function) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            let! flocMcd1 = 
                funclocToMmopCreateData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! flocMcdSubs = processGroupListEmitMmopCreate source.ProcessGroups
            return MmopCreateData.Concat [flocMcd1; flocMcdSubs]
        }

    let functionListEmitMmopCreate (source : S4Function list) : UxlCompilerMonad<MmopCreateData> = 
        mapM functionEmitMmopCreate source |>> MmopCreateData.Concat

    let siteEmitMmopCreate (source : S4Site) : UxlCompilerMonad<MmopCreateData> = 
        compile {
            let! flocMcd1 = 
                funclocToMmopCreateData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! flocMcdSubs = functionListEmitMmopCreate source.Functions
            return MmopCreateData.Concat [flocMcd1; flocMcdSubs]
        }

    let siteListEmitMmopCreate (source : S4Site list) : UxlCompilerMonad<MmopCreateData> = 
        mapM siteEmitMmopCreate source |>> MmopCreateData.Concat