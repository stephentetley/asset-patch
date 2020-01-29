// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher



module EmitPhase1 =
    
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.PatchTypes
    open AssetPatch.TemplatePatcher.TemplateHierarchy
    open AssetPatch.TemplatePatcher.CompilerMonad

    type Phase1FlocData = 
        { FuncLocs : NewFuncLoc list
          FuncLocLinks : LinkFuncLoc list
          ClassFlocs : NewClassFloc list
          ValuaFlocs : NewValuaFloc list
        }

        static member Empty() : Phase1FlocData = 
            { FuncLocs = []
              FuncLocLinks = []
              ClassFlocs  = []
              ValuaFlocs = []
            }

        member x.IsEmpty 
            with get () : bool = 
                x.FuncLocs.IsEmpty && x.FuncLocLinks.IsEmpty
                    && x.ClassFlocs.IsEmpty && x.ValuaFlocs.IsEmpty

        member x.RemoveDups() : Phase1FlocData = 
            { FuncLocs = x.FuncLocs |> List.distinctBy (fun x -> x.FunctionLocation)
              FuncLocLinks = x.FuncLocLinks |> List.distinctBy (fun x -> x.FunctionLocation)
              ClassFlocs = x.ClassFlocs |> List.distinctBy (fun x -> x.FuncLoc.ToString() + "!!" + x.Class)
              ValuaFlocs = x.ValuaFlocs |> List.distinctBy (fun x -> x.FuncLoc.ToString() + "!!" + x.CharacteristicID + "!!" + x.ValueCount.ToString())
            }

        static member Concat (source : Phase1FlocData list) : Phase1FlocData = 
            { FuncLocs = source |> List.map (fun x -> x.FuncLocs) |> List.concat
              FuncLocLinks = source |> List.map (fun x -> x.FuncLocLinks) |> List.concat
              ClassFlocs = source |> List.map (fun x -> x.ClassFlocs) |> List.concat
              ValuaFlocs = source |> List.map (fun x -> x.ValuaFlocs) |> List.concat
            }

    // ************************************************************************
    // Translation - Classifications and characteristics
    
    
    let makeNewValuaFloc (funcLoc : FuncLocPath)
                            (count : int) 
                            (charac : S4Characteristic) : NewValuaFloc = 
        { FuncLoc = funcLoc
          ClassType = IntegerString.OfString "003"
          CharacteristicID = charac.Name
          ValueCount = count
          Value = charac.Value
        }
    
    
    let makeNewValuaFlocs (flocPath : FuncLocPath)
                            (characteristics : S4Characteristic list) : NewValuaFloc list =       
        let makeGrouped (chars : S4Characteristic list) : NewValuaFloc list = 
            chars |> List.mapi (fun i x -> makeNewValuaFloc flocPath (i+1) x)
     
        let chars = sortedCharacteristics characteristics
        List.map makeGrouped chars |> List.concat

    
    
    let makeNewClassFloc (funcLoc : FuncLocPath)  
                                    (clazz : S4Class) : NewClassFloc = 
        { FuncLoc = funcLoc
          Class = clazz.ClassName
          Status = 1
        }

    /// ClassFloc and ValuaFloc
    let makeClassAndValuaFlocPatches  (flocPath : FuncLocPath)
                                    (clazz : S4Class) : NewClassFloc * NewValuaFloc list = 
        let ce = makeNewClassFloc flocPath clazz
        let vs = makeNewValuaFlocs flocPath clazz.Characteristics
        (ce, vs)


    // ************************************************************************
    // Equipment and larger

    let private makeNewEqui1 (equipment : S4Equipment) : NewEqui = 
        let commonProps : CommonProperties = 
            { CompanyCode = equipment.FlocProperties.CompanyCode 
              ControllingArea = equipment.FlocProperties.ControllingArea 
              PlantCode = equipment.FlocProperties.MaintenancePlant
              UserStatus = equipment.FlocProperties.ObjectStatus
            }
        let startupDate = equipment.FlocProperties.StartupDate
        { Description = equipment.Description
          FuncLoc = equipment.FuncLoc
          Category = equipment.Category
          ObjectType = equipment.ObjectType
          Manufacturer = 
                Option.defaultValue "TO BE DETERMINED" equipment.Manufacturer
          Model = Option.defaultValue "TO BE DETERMINED" equipment.Model
          SerialNumber = Option.defaultValue "" equipment.SerialNumber
          StartupDate = equipment.FlocProperties.StartupDate
          ConstructionYear = 
                    Option.defaultValue (uint16 startupDate.Year) equipment.ConstructionYear
          ConstructionMonth = 
                Option.defaultValue (uint8 startupDate.Month) equipment.ConstructionMonth
          MaintenancePlant = equipment.FlocProperties.MaintenancePlant
          Currency = equipment.FlocProperties.Currency
          CommonProps = commonProps
        }
        
    /// Recursive version of equipmentToNewEqui1
    let makeNewEqui (source : S4Equipment) : NewEqui list = 
            let rec work kids cont = 
                match kids with
                | [] -> cont []
                | (x :: xs) -> 
                    let v1 = makeNewEqui1 x
                    work kids (fun vs -> cont(v1 :: vs))
            let ans1 = makeNewEqui1 source
            work source.SuboridnateEquipment (fun xs -> ans1 :: xs)
                
            

    let makeNewFuncLoc (path : FuncLocPath) 
                    (props : FuncLocProperties)
                    (description : string) 
                    (objectType : string)  : NewFuncLoc = 
        let commonProps : CommonProperties = 
            { ControllingArea = props.ControllingArea
              CompanyCode = props.CompanyCode
              PlantCode = props.MaintenancePlant
              UserStatus = props.ObjectStatus }

        { FunctionLocation = path
          Description = description
          ObjectType = objectType
          Category = uint32 path.Level
          ObjectStatus = props.ObjectStatus
          StartupDate = props.StartupDate
          StructureIndicator = props.StructureIndicator
          CommonProps = commonProps
        }
        

    let makeFuncLocLink (path : FuncLocPath) 
                    (props : FuncLocProperties)
                    (description : string) 
                    (objectType : string)  : LinkFuncLoc option = 
        if path.Level > 1 then
            { FunctionLocation = path
              Description = description
              ObjectType = objectType
              Category = uint32 path.Level
              ObjectStatus = props.ObjectStatus
              StartupDate = props.StartupDate
              StructureIndicator = props.StructureIndicator
              InstallationAllowed = if path.Level >= 5 then true else false
            } |> Some
        else  None

    let private funclocToPhase1FlocData (path : FuncLocPath) 
                                (props : FuncLocProperties)
                                (description : string) 
                                (objectType : string)
                                (classes : S4Class list) : CompilerMonad<Phase1FlocData> = 
        let collect xs = List.foldBack (fun (c1, vs1)  (cs,vs) -> (c1 ::cs, vs1 @ vs)) xs ([],[])
        compile {
            let floc = makeNewFuncLoc path props description objectType
            let link = makeFuncLocLink path props description objectType
            let (cs, vs) = List.map (makeClassAndValuaFlocPatches path) classes |> collect
            return { 
                FuncLocs = [floc]
                FuncLocLinks = Option.toList link
                ClassFlocs = cs
                ValuaFlocs = vs
            }
        }


    

    // ************************************************************************
    // User API

    let equipmentEmitNewEquis (source : S4Equipment) : CompilerMonad<NewEqui list> = 
        mreturn (makeNewEqui source)

    let equipmentListEmitNewEquis (source : S4Equipment list) : CompilerMonad<NewEqui list> = 
        mapM equipmentEmitNewEquis source |>> List.concat


    let componentEmitPhase1 (source : S4Component) : CompilerMonad<Phase1FlocData * NewEqui list> = 
        compile {
            let! flocPatches = 
                funclocToPhase1FlocData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! equiPatches = equipmentListEmitNewEquis source.Equipment
            return (flocPatches, equiPatches) 
        }

    let componentListEmitPhase1 (source : S4Component list) : CompilerMonad<Phase1FlocData * NewEqui list> = 
        compile {
            let! (xss, yss) = unzipMapM componentEmitPhase1 source
            return (Phase1FlocData.Concat xss, List.concat yss)
        }

    let itemEmitPhase1 (source : S4Item) : CompilerMonad<Phase1FlocData * NewEqui list> =  
        compile {
            let! flocPatches = 
                funclocToPhase1FlocData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! equiPatches = equipmentListEmitNewEquis source.Equipment
            let! (flocPatches2, equiPatches2) = componentListEmitPhase1 source.Components
            return (Phase1FlocData.Concat [flocPatches; flocPatches2], equiPatches @ equiPatches2)
        }

    let itemListEmitPhase1 (source : S4Item list) : CompilerMonad<Phase1FlocData * NewEqui list> = 
        compile {
            let! (xss, yss) = unzipMapM itemEmitPhase1 source
            return (Phase1FlocData.Concat xss, List.concat yss)
        }

    let assemblyEmitPhase1 (source : S4Assembly) : CompilerMonad<Phase1FlocData * NewEqui list> = 
        compile {
            let! flocPatches = 
                funclocToPhase1FlocData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! equiPatches = equipmentListEmitNewEquis source.Equipment
            let! (flocPatches2, equiPatches2) = itemListEmitPhase1 source.Items
            return (Phase1FlocData.Concat [flocPatches; flocPatches2], equiPatches @ equiPatches2)
        }

    let assemblyListEmitPhase1 (source : S4Assembly list) : CompilerMonad<Phase1FlocData * NewEqui list> = 
        compile {
            let! (xss, yss) = unzipMapM assemblyEmitPhase1 source
            return (Phase1FlocData.Concat xss, List.concat yss)
        }

    let systemEmitPhase1 (source : S4System) : CompilerMonad<Phase1FlocData * NewEqui list> = 
        compile {
            let! flocPatches = 
                funclocToPhase1FlocData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! equiPatches = equipmentListEmitNewEquis source.Equipment
            let! (flocPatches2, equiPatches2) = assemblyListEmitPhase1 source.Assemblies
            return (Phase1FlocData.Concat [flocPatches; flocPatches2], equiPatches @ equiPatches2)
        }

    let systemListEmitPhase1 (source : S4System list) : CompilerMonad<Phase1FlocData * NewEqui list> = 
        compile {
            let! (xss, yss) = unzipMapM systemEmitPhase1 source
            return (Phase1FlocData.Concat xss, List.concat yss)
        }

    let processEmitPhase1 (source : S4Process) : CompilerMonad<Phase1FlocData * NewEqui list> = 
        compile {
            let! flocPatches = 
                funclocToPhase1FlocData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! (flocPatches2, equiPatches2) = systemListEmitPhase1 source.Systems
            return (Phase1FlocData.Concat [flocPatches; flocPatches2], equiPatches2)
        }

    let processListEmitPhase1 (source : S4Process list) : CompilerMonad<Phase1FlocData * NewEqui list> = 
        compile {
            let! (xss, yss) = unzipMapM processEmitPhase1 source
            return (Phase1FlocData.Concat xss, List.concat yss)
        }

    let processGroupEmitPhase1 (source : S4ProcessGroup) : CompilerMonad<Phase1FlocData * NewEqui list> = 
        compile {
            let! flocPatches = 
                funclocToPhase1FlocData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! (flocPatches2, equiPatches2) = processListEmitPhase1 source.Processes
            return (Phase1FlocData.Concat [flocPatches; flocPatches2], equiPatches2)
        }

    let processGroupListEmitPhase1 (source : S4ProcessGroup list) : CompilerMonad<Phase1FlocData * NewEqui list> = 
        compile {
            let! (xss, yss) = unzipMapM processGroupEmitPhase1 source
            return (Phase1FlocData.Concat xss, List.concat yss)
        }

    let functionEmitPhase1 (source : S4Function) : CompilerMonad<Phase1FlocData * NewEqui list> = 
        compile {
            let! flocPatches = 
                funclocToPhase1FlocData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! (flocPatches2, equiPatches2) = processGroupListEmitPhase1 source.ProcessGroups
            return (Phase1FlocData.Concat [flocPatches; flocPatches2], equiPatches2)
        }

    let functionListEmitPhase1 (source : S4Function list) : CompilerMonad<Phase1FlocData * NewEqui list> = 
        compile {
            let! (xss, yss) = unzipMapM functionEmitPhase1 source
            return (Phase1FlocData.Concat xss, List.concat yss)
        }
        
    let siteEmitPhase1 (source : S4Site) : CompilerMonad<Phase1FlocData * NewEqui list> = 
        compile {
            let! flocPatches = 
                funclocToPhase1FlocData source.FuncLoc source.FlocProperties source.Description source.ObjectType source.Classes
            let! (flocPatches2, equiPatches2) = functionListEmitPhase1 source.Functions
            return (Phase1FlocData.Concat [flocPatches; flocPatches2], equiPatches2)
        }

    let siteListEmitPhase1 (source : S4Site list) : CompilerMonad<Phase1FlocData * NewEqui list> = 
        compile {
            let! (xss, yss) = unzipMapM siteEmitPhase1 source
            return (Phase1FlocData.Concat xss, List.concat yss)
        }