// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.RewritePatcher.Base

module Uxl = 

    open System

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.Base.ValuaValue
    open AssetPatch.Base.Uxl.FileTypes
    open AssetPatch.RewritePatcher.Base.UpdateTypes

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
            


    // ************************************************************************
    // Equipment

    type EquipmentChanges = 
        { EquimentDataChanges : EquimentData list
          MultilingualTextChanges : EquiMultilingualText list
          ClassificationChanges : EquiClassification list
        }

    let appendEquipmentChanges (e1: EquipmentChanges) (e2: EquipmentChanges): EquipmentChanges = 
        { EquimentDataChanges = e1.EquimentDataChanges @ e2.EquimentDataChanges
          MultilingualTextChanges = e1.MultilingualTextChanges @ e2.MultilingualTextChanges
          ClassificationChanges = e1.ClassificationChanges @ e2.ClassificationChanges
        }

    let concatEquipmentChanges (changes: EquipmentChanges list): EquipmentChanges = 
        let zero = 
            { EquimentDataChanges = []
              MultilingualTextChanges = []
              ClassificationChanges = []
            }        
        List.fold appendEquipmentChanges zero changes


    let private blankEquimentData (equiId: string): EquimentData = 
        { EquipmentId = equiId
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

    let equiUpdateProperties (equiId: string) (changes: (EquiProperty * ValuaValue) list) : EquimentData = 
        blankEquimentData equiId
            |> updateEquiProperties changes

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
        | EquiChange.DeleteClass(equiId, className) -> 
            { EquimentDataChanges = []
              MultilingualTextChanges = []
              ClassificationChanges = [equiDeleteClass equiId className]
            }

        | EquiChange.DeleteChar(equiId, className, charName) -> 
            { EquimentDataChanges = []
              MultilingualTextChanges = []
              ClassificationChanges = [equiDeleteChar equiId className charName]
            }
     
        | EquiChange.UpdateProperties(equiId, changes) ->
            { EquimentDataChanges = [equiUpdateProperties equiId changes]
              MultilingualTextChanges = []
              ClassificationChanges = []
            }
        
        | EquiChange.UpdateChar(equiId, className, charName, value) -> 
            { EquimentDataChanges = []
              MultilingualTextChanges = []
              ClassificationChanges = [equiUpdateChar equiId className charName value]
            }
        
        | EquiChange.UpdateMultilingualText(equiId, text) -> 
            { EquimentDataChanges = []
              MultilingualTextChanges = [equiUpdateMultilingualText equiId text]
              ClassificationChanges = []
            }

    let equipmentChanges (source: EquiChange list) : EquipmentChanges = 
        groupEquiPropertyChanges source
            |> List.map equipmentChanges1
            |> concatEquipmentChanges