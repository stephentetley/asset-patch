﻿// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.RewritePatcher.Base

module Uxl = 

    open System.IO

    
    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.Base.ValuaValue
    open AssetPatch.Base.Uxl.FileTypes
    open AssetPatch.RewritePatcher.Base.UpdateTypes
    open AssetPatch.RewritePatcher.Base.Rewrite    
    open AssetPatch.RewritePatcher.Base.RewriteMonad

    
    let equipmentSortKey (equiId: string) (superId: string) = 
        superId.PadLeft(16, '0') + equiId.PadLeft(16, '0')


    // ************************************************************************/
    // Gen file name

    let private genFileName (directory : string)
                            (filePrefix : string) 
                            (namePart : string): RewriteMonad<string, 'uenv> = 
        rewriter {
            let name1 = sprintf "%s_%s.csv" (safeName filePrefix) (safeName namePart)
            return Path.Combine(directory, name1)
        } 



    // ************************************************************************
    // Functional Locations

    type FuncLocChanges = 
        { FuncLocDataChanges : FunctionalLocationData list
          MultilingualTextChanges : FlocMultilingualText list
          ClassificationChanges : FlocClassification list
        }

    let appendFuncLocChanges (e1: FuncLocChanges) (e2: FuncLocChanges): FuncLocChanges = 
        { FuncLocDataChanges = e1.FuncLocDataChanges @ e2.FuncLocDataChanges
          MultilingualTextChanges = e1.MultilingualTextChanges @ e2.MultilingualTextChanges
          ClassificationChanges = e1.ClassificationChanges @ e2.ClassificationChanges
        }

    let concatFuncLocChanges (changes: FuncLocChanges list): FuncLocChanges = 
        let zero = 
            { FuncLocDataChanges = []
              MultilingualTextChanges = []
              ClassificationChanges = []
            }        
        List.fold appendFuncLocChanges zero changes

    let private blankFunctionalLocationData (funcLoc: FuncLocPath): FunctionalLocationData = 
        { FunctionalLocation = funcLoc
          DescriptionMedium = ""
          FunLocCategory = None
          StructureIndicator = ""
          ObjectType = ""
          StartupDate = None
          ConstructionYear = None
          ConstructionMonth = None
          CompanyCode = ""
          SuperiorFuncLoc = ""
          EquipInstall = None
          StatusOfAnObject = ""
          StatusWithoutStatusNum = ""
        }

    let private updateFlocProperty (prop: FlocProperty) 
                                       (value: ValuaValue) 
                                       (flocData: FunctionalLocationData) : FunctionalLocationData = 
           match prop with
           | FlocProperty.DescriptionMedium -> { flocData with DescriptionMedium = uxlStringValue value }
           | FlocProperty.FunLocCategory -> { flocData with FunLocCategory = uxlIntOptionValue value }
           | FlocProperty.StructureIndicator -> { flocData with StructureIndicator = uxlStringValue value }
           | FlocProperty.ObjectType -> { flocData with ObjectType = uxlStringValue value }
           | FlocProperty.StartupDate -> { flocData with StartupDate = uxlDataTimeOptionValue value }
           | FlocProperty.ConstructionYear -> { flocData with ConstructionYear = uxlIntOptionValue value }
           | FlocProperty.ConstructionMonth -> { flocData with ConstructionMonth = uxlIntOptionValue value }
           | FlocProperty.CompanyCode -> { flocData with CompanyCode = uxlStringValue value }
           | FlocProperty.SuperiorFuncLoc -> { flocData with SuperiorFuncLoc = uxlStringValue value }
           | FlocProperty.EquipInstall -> printfn "**WARNING** - EquipInstall not implemented"; { flocData with EquipInstall = None }
           | FlocProperty.StatusOfAnObject -> { flocData with StatusOfAnObject = uxlStringValue value }
           | FlocProperty.StatusWithoutStatusNum -> { flocData with StatusWithoutStatusNum = uxlStringValue value }

    let private updateFlocProperties (changes: (FlocProperty *  ValuaValue) list)
                                    (flocData: FunctionalLocationData) : FunctionalLocationData = 
        List.fold(fun ac (prop, value) -> updateFlocProperty prop value ac) flocData changes
    
    let flocDeleteClass (funcLoc: FuncLocPath) (className: string) : FlocClassification = 
        { FunctionalLocation = funcLoc
          ClassDeletionInd = Some true
          Class = className
          Status = ""
          CharacteristicName = ""
          CharacteristicValue = ""
          CharDeletionInd = None
        }

    let flocDeleteChar (funcLoc: FuncLocPath) (className: string) (charName: string) : FlocClassification =
        { FunctionalLocation = funcLoc
          ClassDeletionInd = None
          Class = className
          Status = ""
          CharacteristicName = charName
          CharacteristicValue = ""
          CharDeletionInd = Some true
        }


    let flocUpdateProperties (funcLoc: FuncLocPath) (changes: (FlocProperty * ValuaValue) list) : FunctionalLocationData = 
        blankFunctionalLocationData funcLoc
            |> updateFlocProperties changes

    let flocUpdateChar (funcLoc: FuncLocPath) (className: string) 
                        (charName: string) (value: ValuaValue) : FlocClassification = 
        { FunctionalLocation = funcLoc
          ClassDeletionInd = None
          Class = className
          Status = ""
          CharacteristicName = charName
          CharacteristicValue = Option.defaultValue "" value.CharacteristicValue
          CharDeletionInd = None
        }

    let flocDeleteMultilingual (funcLoc: FuncLocPath) : FlocMultilingualText = 
        { FunctionalLocation = funcLoc
          DeleteIndicator = true
          Language = ""
          Description = ""
          LongText = ""
        }

    let flocUpdateMultilingualText (funcLoc: FuncLocPath) 
                                    (text: string) : FlocMultilingualText = 
        { FunctionalLocation = funcLoc
          DeleteIndicator = false
          Language = ""
          Description = ""
          LongText = text
        }

    let functionalLocationChanges1 (source: FlocChange) : FuncLocChanges = 
        match source with
        | FlocChange.DeleteMultilingualText(funcLoc) -> 
            { FuncLocDataChanges = []
              MultilingualTextChanges = [flocDeleteMultilingual funcLoc]
              ClassificationChanges = []
            }

        | FlocChange.DeleteClass(funcLoc, className) -> 
            { FuncLocDataChanges = []
              MultilingualTextChanges = []
              ClassificationChanges = [flocDeleteClass funcLoc className]
            }

        | FlocChange.DeleteChar(funcLoc, className, charName) -> 
            { FuncLocDataChanges = []
              MultilingualTextChanges = []
              ClassificationChanges = [flocDeleteChar funcLoc className charName]
            }
     
        | FlocChange.UpdateProperties(funcLoc, changes) ->
            { FuncLocDataChanges = [flocUpdateProperties funcLoc changes]
              MultilingualTextChanges = []
              ClassificationChanges = []
            }
        
        | FlocChange.UpdateChar(funcLoc, className, charName, value) -> 
            { FuncLocDataChanges = []
              MultilingualTextChanges = []
              ClassificationChanges = [flocUpdateChar funcLoc className charName value]
            }
        
        | FlocChange.UpdateMultilingualText(funcLoc, text) -> 
            { FuncLocDataChanges = []
              MultilingualTextChanges = [flocUpdateMultilingualText funcLoc text]
              ClassificationChanges = []
            }


    let functionalLocationChanges (source: FlocChange list) : FuncLocChanges = 
        groupFlocPropertyChanges source
            |> List.map functionalLocationChanges1
            |> concatFuncLocChanges
            
    let funcLocChangeFlocs (changes: FuncLocChanges): FuncLocPath list = 
        let xs = changes.FuncLocDataChanges |> List.map (fun x -> x.FunctionalLocation)
        let ys = changes.MultilingualTextChanges |> List.map (fun x -> x.FunctionalLocation)
        let zs = changes.ClassificationChanges |> List.map (fun x -> x.FunctionalLocation)
        xs @ ys @ zs 
            |> List.distinct |> List.sort

    // ************************************************************************
    // Equipment

    type EquipmentChanges = 
        { EquipmentDataChanges : EquimentData list
          MultilingualTextChanges : EquiMultilingualText list
          ClassificationChanges : EquiClassification list
        }

    let appendEquipmentChanges (e1: EquipmentChanges) (e2: EquipmentChanges): EquipmentChanges = 
        { EquipmentDataChanges = e1.EquipmentDataChanges @ e2.EquipmentDataChanges
          MultilingualTextChanges = e1.MultilingualTextChanges @ e2.MultilingualTextChanges
          ClassificationChanges = e1.ClassificationChanges @ e2.ClassificationChanges
        }

    let concatEquipmentChanges (changes: EquipmentChanges list): EquipmentChanges = 
        let zero = 
            { EquipmentDataChanges = []
              MultilingualTextChanges = []
              ClassificationChanges = []
            }        
        List.fold appendEquipmentChanges zero changes


    let private blankEquipmentData (equiId: string) (sortKey: string) : EquimentData = 
        { _SortKey = sortKey
          EquipmentId = equiId
          EquipCategory = "" 
          DescriptionMedium = ""
          ObjectType = ""
          StartupDate = None
          Manufacturer = ""
          ModelNumber = ""
          ManufPartNum = ""
          ManufSerialNum = ""
          ConstructionYear = None
          ConstructionMonth = None
          CompanyCode = ""
          FunctionalLocation = ""
          SuperordEquip = ""
          StatusOfAnObject = ""
          StatusWithoutStatusNum = ""
        }

            
    let private updateEquiProperty (prop: EquiProperty) 
                                    (value: ValuaValue) 
                                    (equiData: EquimentData) : EquimentData = 
        match prop with
        | EquiProperty.EquipCategory -> { equiData with EquipCategory = uxlStringValue value }
        | EquiProperty.DescriptionMedium -> { equiData with DescriptionMedium = uxlStringValue value }
        | EquiProperty.ObjectType -> { equiData with ObjectType = uxlStringValue value }
        | EquiProperty.StartupDate -> { equiData with StartupDate = uxlDataTimeOptionValue value }
        | EquiProperty.Manufacturer -> { equiData with Manufacturer = uxlStringValue value }
        | EquiProperty.ModelNumber -> { equiData with ModelNumber = uxlStringValue value }
        | EquiProperty.ManufPartNum -> { equiData with ModelNumber = uxlStringValue value }
        | EquiProperty.ManufSerialNum -> { equiData with ManufPartNum = uxlStringValue value }
        | EquiProperty.ConstructionYear -> { equiData with ConstructionYear = uxlIntOptionValue value }
        | EquiProperty.ConstructionMonth -> { equiData with ConstructionMonth = uxlIntOptionValue value }
        | EquiProperty.CompanyCode -> { equiData with CompanyCode = uxlStringValue value }
        | EquiProperty.FunctionalLocation -> { equiData with FunctionalLocation = uxlStringValue value }
        | EquiProperty.SuperordEquip -> { equiData with SuperordEquip = uxlStringValue value }
        | EquiProperty.StatusOfAnObject -> { equiData with StatusOfAnObject = uxlStringValue value }
        | EquiProperty.StatusWithoutStatusNum -> { equiData with StatusWithoutStatusNum = uxlStringValue value }


    let private updateEquiProperties (changes: (EquiProperty *  ValuaValue) list)
                                    (equiData: EquimentData) : EquimentData = 
        List.fold(fun ac (prop, value) -> updateEquiProperty prop value ac) equiData changes
    
    let equiDeleteClass (equiId: string) (className: string) : EquiClassification = 
        { EquipmentId = equiId
          ClassDeleteInd = true
          Class = className
          Status = ""
          CharacteristicName = ""
          CharacteristicValue = ""
          CharDeleteInd = false
        }

    let equiDeleteChar (equiId: string) (className: string) (charName: string) : EquiClassification =
        { EquipmentId = equiId
          ClassDeleteInd = false
          Class = className
          Status = ""
          CharacteristicName = charName
          CharacteristicValue = ""
          CharDeleteInd = true
        }

    let equiUpdateProperties (equiId: string) (superId: string) (changes: (EquiProperty * ValuaValue) list) : EquimentData = 
        let sortKey = equipmentSortKey equiId superId
        blankEquipmentData equiId sortKey |> updateEquiProperties changes
        

    let equiUpdateChar (equiId: string) (className: string) 
                        (charName: string) (value: ValuaValue) : EquiClassification = 
        { EquipmentId = equiId
          ClassDeleteInd = false
          Class = className
          Status = ""
          CharacteristicName = charName
          CharacteristicValue = Option.defaultValue "" value.CharacteristicValue
          CharDeleteInd = false
        }

    let equiDeleteMultilingualText (equiId: string) : EquiMultilingualText = 
           { EquipmentId = equiId
             DeleteIndicator = true
             Language = ""
             DescriptionMedium = ""
             LongText = ""
           }

    let equiUpdateMultilingualText (equiId: string) 
                                    (text: string) : EquiMultilingualText = 
        { EquipmentId = equiId
          DeleteIndicator = false
          Language = ""
          DescriptionMedium = ""
          LongText = text
        }

    let equipmentChanges1 (source: EquiChange) : EquipmentChanges = 
        match source with
        | EquiChange.DeleteMultilingualText(equiId) -> 
            { EquipmentDataChanges = []
              MultilingualTextChanges = [equiDeleteMultilingualText equiId]
              ClassificationChanges = []
            }

        | EquiChange.DeleteClass(equiId, className) -> 
            { EquipmentDataChanges = []
              MultilingualTextChanges = []
              ClassificationChanges = [equiDeleteClass equiId className]
            }

        | EquiChange.DeleteChar(equiId, className, charName) -> 
            { EquipmentDataChanges = []
              MultilingualTextChanges = []
              ClassificationChanges = [equiDeleteChar equiId className charName]
            }
     
        | EquiChange.UpdateProperties(equiId, superId, changes) ->
            { EquipmentDataChanges = [equiUpdateProperties equiId superId changes]
              MultilingualTextChanges = []
              ClassificationChanges = []
            }
        
        | EquiChange.UpdateChar(equiId, className, charName, value) -> 
            { EquipmentDataChanges = []
              MultilingualTextChanges = []
              ClassificationChanges = [equiUpdateChar equiId className charName value]
            }
        
        | EquiChange.UpdateMultilingualText(equiId, text) -> 
            { EquipmentDataChanges = []
              MultilingualTextChanges = [equiUpdateMultilingualText equiId text]
              ClassificationChanges = []
            }

    let equipmentChanges (source: EquiChange list) : EquipmentChanges = 
        groupEquiPropertyChanges source
            |> List.map equipmentChanges1
            |> concatEquipmentChanges

    let equipmentChangeIds (changes: EquipmentChanges): EquipmentId list = 
        let xs = changes.EquipmentDataChanges |> List.map (fun x -> x.EquipmentId)
        let ys = changes.MultilingualTextChanges |> List.map (fun x -> x.EquipmentId)
        let zs = changes.ClassificationChanges |> List.map (fun x -> x.EquipmentId)
        xs @ ys @ zs 
            |> List.distinct |> List.sort


    let rewriteEquiAll (rw: EquiRewrite<'src>)
                        (sources: 'src list) : RewriteMonad<EquipmentChanges, 'uenv> = 
        rewriteAll rw sources
            |>> fun (_, changes) -> equipmentChanges changes




    // ************************************************************************
    // Write patches

    let makeChangeRequestDetails1 (item : Choice<FuncLocPath, EquipmentId>) : RewriteMonad<ChangeRequestDetails, 'uenv> = 
        rewriter {
            let! requester = askProcessRequester ()
            let! changeType = askTypeOfChangeRequest ()
            let! description = askChangeRequestDescription ()
            return { 
                DescriptionLong = description
                Priority = ""
                DueDate = None
                Reason = ""
                TypeOfChangeRequest = changeType
                ChangeRequestGroup = ""
                FuncLocOrEquipment = item
                ProcessRequester = requester
            }
        }

    let writeChangeRequestDetails (flocs: FuncLocPath list) 
                                    (equiIds: EquipmentId list)
                                    (directory : string)
                                    (filePrefix : string) : RewriteMonad<Unit, 'uenv> = 
        
        rewriter {
            let! changeRequestItems = 
                let xs = List.map Choice1Of2 flocs
                let ys = List.map Choice2Of2 equiIds
                mapM makeChangeRequestDetails1 (xs @ ys)
        
            let! outPath01 = genFileName directory filePrefix "01_change_request_details_tab"
            do! liftResult <| writeChangeRequestDetails changeRequestItems outPath01
            return ()
        }

    let writeFuncLocPatches (changes: FuncLocChanges)
                                (directory : string)
                                (filePrefix : string) : RewriteMonad<Unit, 'uenv> = 
        
        rewriter {
            // FuncLoccs
            let! outPath02 = genFileName directory filePrefix "02_function_location_data_tab"
            do! liftResult <| writeFunctionalLocationData changes.FuncLocDataChanges outPath02
            
            let! outPath03 = genFileName directory filePrefix "03_fl_mulitlingual_text_tab"
            do!liftResult <|  writeFlocMultilingualText changes.MultilingualTextChanges outPath03
            
            let! outPath04 = genFileName directory filePrefix "04_fl_classification_tab"
            do! liftResult <| writeFlocClassification changes.ClassificationChanges outPath04
            return ()
        }



    let writeEquipmentPatches (changes: EquipmentChanges)
                                (directory : string)
                                (filePrefix : string) : RewriteMonad<Unit, 'uenv> = 
        
        rewriter {
            // Equipment
            let! outPath05 = genFileName directory filePrefix "05_equipment_data_tab"
            do! liftResult <| writeEquipmentData changes.EquipmentDataChanges outPath05
            
            let! outPath06 = genFileName directory filePrefix "06_eq_mulitlingual_text_tab"
            do! liftResult <| writeEquiMultilingualText changes.MultilingualTextChanges outPath06
            
            let! outPath07 = genFileName directory filePrefix "07_eq_classification_tab"
            do! liftResult <| writeEquiClassification changes.ClassificationChanges outPath07
            return ()
        }


    let writeChangeRequestAndFuncLocPatches (changes: FuncLocChanges)
                                            (directory : string)
                                            (filePrefix : string) : RewriteMonad<Unit, 'uenv> =         
        rewriter {
            let flocs = funcLocChangeFlocs changes
            do! writeChangeRequestDetails flocs [] directory filePrefix
            do! writeFuncLocPatches changes directory filePrefix
            return ()
        }

    let writeChangeRequestAndEquipmentPatches (changes: EquipmentChanges)
                                (directory : string)
                                (filePrefix : string) : RewriteMonad<Unit, 'uenv> =         
        rewriter {
            let equiIds = equipmentChangeIds changes
            do! writeChangeRequestDetails [] equiIds directory filePrefix
            do! writeEquipmentPatches changes directory filePrefix
            return ()
        }

    let writeChangeRequestFull (flocChanges: FuncLocChanges)
                                (equiChanges: EquipmentChanges)
                                (directory : string)
                                (filePrefix : string) : RewriteMonad<Unit, 'uenv> =         
        rewriter {
            let flocs = funcLocChangeFlocs flocChanges
            let equiIds = equipmentChangeIds equiChanges
            do! writeChangeRequestDetails flocs equiIds directory filePrefix
            do! writeFuncLocPatches flocChanges directory filePrefix
            do! writeEquipmentPatches equiChanges directory filePrefix
            return ()
        }