// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Aiw




module Emitter =
    
    open System.IO
        
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.Base.Aiw.ChangeFile
    open AssetPatch.TemplatePatcher.Base.Hierarchy
    open AssetPatch.TemplatePatcher.Base.GenerateMonad
    open AssetPatch.TemplatePatcher.Aiw.Base
    open AssetPatch.TemplatePatcher.Aiw.FileTypes


    /// Write a file for each level of floc, update and approve in Workbench
    /// This is more robust than linking orphan flocs with a patch which might
    /// not work in practice due to DB validation.
    type FlocCreateData = 
        { NewFuncLocs: NewFuncLoc list
          NewFlocClasses: NewClassFloc list
          NewFlocCharacteristics: NewValuaFloc list
        }

        static member Empty () : FlocCreateData = 
            { NewFuncLocs = []
              NewFlocClasses = []
              NewFlocCharacteristics = []
            }

        member x.IsEmpty
            with get () : bool = 
                x.NewFuncLocs.IsEmpty 
                    && x.NewFlocClasses.IsEmpty
                    && x.NewFlocCharacteristics.IsEmpty
                
        member x.RemoveDups(): FlocCreateData = 
            let funcLocDistinctKey (v: NewFuncLoc) : string =
                v.FunctionLocation.ToString()

            let flocClassDistinctKey (v: NewClassFloc) : string =
                v.FuncLoc.ToString() + "!!" + v.Class

            /// Allowed different instances of same field (e.g. AIB_AI2_REFERENCE), but not same instances of same field
            let flocCharDistinctKey (v: NewValuaFloc) : string = 
                v.FuncLoc.ToString() + "!!" + v.CharacteristicID + "!!" + v.Value.DescriptionValue

            { NewFuncLocs = x.NewFuncLocs |> List.distinctBy funcLocDistinctKey
              NewFlocClasses = x.NewFlocClasses |> List.distinctBy flocClassDistinctKey
              NewFlocCharacteristics = x.NewFlocCharacteristics |> List.distinctBy flocCharDistinctKey
            }

        static member Concat (source : FlocCreateData list) : FlocCreateData = 
            { NewFuncLocs               = source |> List.map (fun x -> x.NewFuncLocs) |> List.concat
              NewFlocClasses            = source |> List.map (fun x -> x.NewFlocClasses) |> List.concat
              NewFlocCharacteristics    = source |> List.map (fun x -> x.NewFlocCharacteristics) |> List.concat
            }

        member x.GetLevel (level: int): FlocCreateData = 
            { NewFuncLocs               = x.NewFuncLocs |> List.filter (fun x -> x._Level = level)
              NewFlocClasses            = x.NewFlocClasses |> List.filter (fun x -> x._Level = level) 
              NewFlocCharacteristics    = x.NewFlocCharacteristics |> List.filter (fun x -> x._Level = level) 
            }

    // Note cannot have two equipments with the same name at the same floc
    type EquiCreateData =
        { NewEquipment: NewEqui list }
        
        static member Empty () : EquiCreateData = 
            { NewEquipment = []
            }

        member x.IsEmpty
            with get () : bool = x.NewEquipment.IsEmpty 

        member x.RemoveDups(): EquiCreateData = 
            let equiDistinctKey (v: NewEqui) : string = v.FuncLoc.ToString() + "!!" + v.Description
            { NewEquipment = x.NewEquipment |> List.distinctBy equiDistinctKey }

        static member Concat (source : EquiCreateData list) : EquiCreateData = 
            { NewEquipment = source |> List.map (fun x -> x.NewEquipment) |> List.concat }

    type EquiCreateClassifications = 
        { NewEquiClasses: NewClassEqui list
          NewEquiCharacteristics: NewValuaEqui list
          NewEquiMultilingualTexts: NewEqmltxt list
        }

        static member Empty () : EquiCreateClassifications = 
            { NewEquiClasses = []
              NewEquiCharacteristics = []
              NewEquiMultilingualTexts = []
            }

        member x.IsEmpty
            with get () : bool = 
                x.NewEquiClasses.IsEmpty
                    && x.NewEquiCharacteristics.IsEmpty

        member x.RemoveDups(): EquiCreateClassifications = 
            let equiClassDistinctKey (v: NewClassEqui) : string =
                v.EquipmentId + "!!" + v.Class

            /// Allowed different instances of same field (e.g. AIB_AI2_REFERENCE), but not same instances of same field
            let equiCharDistinctKey (v: NewValuaEqui) : string = 
                v.EquipmentId + "!!" + v.CharacteristicID + "!!" + v.Value.DescriptionValue

            let equiMltxtDistinctKey (v: NewEqmltxt) : string = v.EquipmentId

            { NewEquiClasses = x.NewEquiClasses |> List.distinctBy equiClassDistinctKey
              NewEquiCharacteristics = x.NewEquiCharacteristics |> List.distinctBy equiCharDistinctKey
              NewEquiMultilingualTexts = x.NewEquiMultilingualTexts |> List.distinctBy equiMltxtDistinctKey
            }

        static member Concat (source : EquiCreateClassifications list) : EquiCreateClassifications = 
            { NewEquiClasses            = source |> List.map (fun x -> x.NewEquiClasses) |> List.concat
              NewEquiCharacteristics    = source |> List.map (fun x -> x.NewEquiCharacteristics) |> List.concat
              NewEquiMultilingualTexts  = source |> List.map (fun x -> x.NewEquiMultilingualTexts) |> List.concat
            }

    // ************************************************************************
    // Translation - equipment
    
    
    /// Count is deduced by caller
    let private makeNewValuaEqui (count: int) (source: S4EquiClassification) : NewValuaEqui = 
        { EquipmentId = source.EquiId
          ClassType = IntegerString.OfString "002"
          CharacteristicID = source.CharName
          ValueCount = count
          Value = source.CharValue
        }

    let private makeNewClassEqui (source : S4EquiClassification) : NewClassEqui = 
        { EquipmentId = source.EquiId
          Class = source.ClassName
          Status = 1
        }

    let makeEquiMultilingualText (equiId: string)
                                (description: string)
                                (longText : string) : NewEqmltxt = 
        { EquipmentId = equiId
          Description = description
          LongText = longText
          MoreTextExists = false
        }

    // Not recursive - source might have subordinate equipment
    let private makeNewEqui (source : S4Equipment) : NewEqui = 
        let commonProps : CommonProperties = 
            { CompanyCode = source.FlocProperties.CompanyCode 
              ControllingArea = source.FlocProperties.ControllingArea 
              PlantCode = source.FlocProperties.MaintenancePlant
              UserStatus = source.FlocProperties.ObjectStatus
            }
        let startupDate = source.FlocProperties.StartupDate
        { Description = source.Description
          FuncLoc = source.FuncLoc
          Category = source.Category
          ObjectType = source.ObjectType
          Manufacturer = 
                Option.defaultValue "TO BE DETERMINED" source.Manufacturer
          Model = Option.defaultValue "TO BE DETERMINED" source.Model
          SerialNumber = Option.defaultValue "" source.SerialNumber
          StartupDate = source.FlocProperties.StartupDate
          ConstructionYear = 
                    Option.defaultValue (uint16 startupDate.Year) source.ConstructionYear
          ConstructionMonth = 
                Option.defaultValue (uint8 startupDate.Month) source.ConstructionMonth
          MaintenancePlant = source.FlocProperties.MaintenancePlant
          Currency = source.FlocProperties.Currency
          CommonProps = commonProps
        }

    /// Recursive version
    /// TODO - This is not adequate, we will have to stratify subequipment instead
    let makeEquiDataWithKidsPhase1 (source : S4Equipment) : NewEqui list = 
        let rec work xs cont = 
            match xs with
            | [] -> cont []
            | (x :: rs) -> 
                let v1 = makeNewEqui x 
                let kids = x.SuboridinateEquipment
                work (rs @ kids) (fun vs -> cont (v1 :: vs))
        let equi = makeNewEqui source             
        work source.SuboridinateEquipment (fun xs -> (equi :: xs))


    let private equipmentToEquiCreateData (source : S4Equipment) : EquiCreateData = 
        { NewEquipment = makeEquiDataWithKidsPhase1 source
        }


    let private groupForEquiCharacteristics (classifications : S4EquiClassification list) : (S4EquiClassification list) list = 
        classifications
            |> List.groupBy (fun x -> x.EquiId + "!!" + x.ClassName + "!!" + x.CharName)
            |> List.map snd

    let private equipmentToEquiCreateClassifications (source : S4Equipment) : EquiCreateClassifications =         
        let makeGrouped (xs : S4EquiClassification list) : NewValuaEqui list = 
            xs |> List.mapi (fun i x -> makeNewValuaEqui (i+1) x)

        let classes : NewClassEqui list = List.map makeNewClassEqui source.Classifications
        let characteristics : NewValuaEqui list = 
            source.Classifications 
                |> groupForEquiCharacteristics 
                |> List.map makeGrouped 
                |> List.concat
        let mltxt = makeEquiMultilingualText source.EquiId "" source.MultilingualText
        { NewEquiClasses = classes
          NewEquiCharacteristics = characteristics
          NewEquiMultilingualTexts = [mltxt]
        }

    // ************************************************************************
    // Translation - functional location

    /// Count is deduced by caller
    let private makeNewValuaFloc(count: int) (source: S4FlocClassification) : NewValuaFloc = 
        { _Level = source.FuncLoc.Level
          FuncLoc = source.FuncLoc
          ClassType = IntegerString.OfString "002"
          CharacteristicID = source.CharName
          ValueCount = count
          Value = source.CharValue
        }

    let private makeNewClassFloc (source : S4FlocClassification) : NewClassFloc = 
        { _Level = source.FuncLoc.Level
          FuncLoc = source.FuncLoc
          Class = source.ClassName
          Status = 1
        }



    let makeNewFuncLoc (path : FuncLocPath) 
                        (props : FuncLocProperties)
                        (description : string) 
                        (objectType : string) : NewFuncLoc = 
        let commonProps : CommonProperties = 
            { ControllingArea = props.ControllingArea
              CompanyCode = props.CompanyCode
              PlantCode = props.MaintenancePlant
              UserStatus = props.ObjectStatus }

        { _Level = path.Level
          FunctionLocation = path
          Description = description
          ObjectType = objectType
          Category = uint32 path.Level
          ObjectStatus = props.ObjectStatus
          StartupDate = props.StartupDate
          StructureIndicator = props.StructureIndicator
          CommonProps = commonProps
        }

    let private groupForFlocCharacteristics (classifications : S4FlocClassification list) : (S4FlocClassification list) list = 
        classifications
            |> List.groupBy (fun x -> x.FuncLoc.ToString() + "!!" + x.ClassName + "!!" + x.CharName)
            |> List.map snd

    let private funclocToFlocCreateData (path : FuncLocPath) 
                                        (flocProps : FuncLocProperties)
                                        (description : string) 
                                        (objectType : string)
                                        (classifications : S4FlocClassification list) : FlocCreateData = 
        
        let makeGrouped (xs : S4FlocClassification list) : NewValuaFloc list = 
            xs |> List.mapi (fun i x -> makeNewValuaFloc (i+1) x)

        let floc = makeNewFuncLoc path flocProps description objectType
        let classes : NewClassFloc list = List.map makeNewClassFloc classifications 
        let characteristics : NewValuaFloc list = 
            classifications 
                |> groupForFlocCharacteristics 
                |> List.map makeGrouped 
                |> List.concat
        { NewFuncLocs = [floc]
          NewFlocClasses = classes
          NewFlocCharacteristics = characteristics
        }

    // ************************************************************************
    // User API

    let equiEmitEquiCreateData (source : S4Equipment) : AiwGenerate<EquiCreateData> = 
        generate { 
            return equipmentToEquiCreateData source
        }

    let equiListEmitEquiCreateData (source : S4Equipment list) : AiwGenerate<EquiCreateData> = 
        mapM equiEmitEquiCreateData source |>> EquiCreateData.Concat


    let equiEmitEquiCreateClassifications (source : S4Equipment) : AiwGenerate<EquiCreateClassifications> = 
        generate { 
            return equipmentToEquiCreateClassifications source
        }

    let equiListEquiCreateClassifications (source : S4Equipment list) : AiwGenerate<EquiCreateClassifications> = 
        mapM equiEmitEquiCreateClassifications source |>> EquiCreateClassifications.Concat

    /// This creates all levels...
    let flocEmitFlocCreateData (source : S4FunctionalLocation) : AiwGenerate<FlocCreateData> = 
        let create1 (src : S4FunctionalLocation) = 
            funclocToFlocCreateData src.FuncLoc src.FlocProperties 
                                    src.Description src.ObjectType src.Classifications
             
        let rec work xs cont = 
            match xs with 
            | [] -> cont []
            | x :: rs -> 
                let v1 = create1 x 
                let kids = x.SubFlocs
                work (kids @ rs) (fun vs -> cont (v1 :: vs))
                
        let d1 = create1 source
        let ds = work source.SubFlocs (fun xs -> xs)
        FlocCreateData.Concat (d1 :: ds) |> mreturn
        

    /// This creates all levels...
    let flocEmitEquiCreateData (source : S4FunctionalLocation) : AiwGenerate<EquiCreateData> = 
        let create1 (src : S4FunctionalLocation) : EquiCreateData list = 
            List.map equipmentToEquiCreateData src.Equipment
             
        let rec work xs cont = 
            match xs with 
            | [] -> cont []
            | x :: rs -> 
                let equis1 = create1 x 
                let kids = x.SubFlocs
                work (kids @ rs) (fun vs -> cont (equis1 @ vs))
                
        let ds1 = create1 source
        let ds = work source.SubFlocs (fun xs -> xs)
        EquiCreateData.Concat (ds1 @ ds) |> mreturn
        
    /// This creates all levels...
    let flocEmitEquiCreateClassifications (source : S4FunctionalLocation) : AiwGenerate<EquiCreateClassifications> =                     
        let create1 (src : S4FunctionalLocation) : EquiCreateClassifications list = 
            List.map equipmentToEquiCreateClassifications src.Equipment
             
        let rec work xs cont = 
            match xs with 
            | [] -> cont []
            | x :: rs -> 
                let equis1 = create1 x 
                let kids = x.SubFlocs
                work (kids @ rs) (fun vs -> cont (equis1 @ vs))
                
        let ds1 = create1 source
        let ds = work source.SubFlocs (fun xs -> xs)
        EquiCreateClassifications.Concat (ds1 @ ds) |> mreturn