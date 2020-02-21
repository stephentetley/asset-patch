// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Base



module AltTemplate =
    
    open System

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.Base.ValuaValue 
    open AssetPatch.TemplatePatcher.Base.AltHierarchy   

    type EquipmentId = string 

    type TemplateEnv = 
        { StartupDate : DateTime
          ObjectStatus : string
          StructureIndicator : string
          CompanyCode : uint32
          MaintenancePlant : uint32
          ControllingArea : uint32
          Currency : string
        }

    let defaultEnv () : TemplateEnv = 
        { StartupDate = DateTime.Now
          ObjectStatus = "UCON"
          StructureIndicator = "YW-GS"
          CompanyCode = 2100u
          MaintenancePlant = 2100u          
          ControllingArea = 1000u
          Currency = "GBP"
        }

    type TemplateState = 
        { NameIndex: int }


    /// 'env has common props "structure indicator" etc.
    type Template<'a> = 
        | Template of (TemplateEnv -> TemplateState -> 'a * TemplateState)


    let inline private apply1 (ma : Template<'a>) 
                                (env : TemplateEnv) 
                                (st: TemplateState) : 'a * TemplateState = 
        let (Template fn) = ma in fn env st

    let mreturn (x:'a) : Template<'a> = 
        Template <| fun _ st -> (x, st)


    let inline private bindM (ma : Template<'a>) 
                             (fn : 'a -> Template<'b>) : Template<'b> =
        Template <| fun env st -> 
            let (a, s1) = apply1 ma env st in apply1 (fn a) env s1
         
    let inline private delayM (fn : unit -> Template<'a>) : Template<'a> = 
        bindM (mreturn ()) fn 
    
    type TemplateBuilder() = 
        member self.Return x        = mreturn x
        member self.Bind (p,f)      = bindM p f
        member self.Delay fn        = delayM fn
        member self.ReturnFrom(ma)  = ma


    let (template : TemplateBuilder) = new TemplateBuilder()


    let runTemplate (ma : Template<'a>) 
                    (env : TemplateEnv) 
                    (nameStart: int) : 'a * int = 
        let stateZero = { NameIndex = nameStart }
        let (a, s1) = apply1 ma env stateZero 
        (a, s1.NameIndex)

    let private ( |>> ) (ma : Template<'a>) (fn : 'a -> 'b) : Template<'b> = 
        Template <| fun env st ->
            let (a, s1) =  apply1 ma env st in (fn a, s1)

    let getFlocProps() : Template<FuncLocProperties> = 
        Template <| fun env st -> 
            let props : FuncLocProperties = 
                { StartupDate = env.StartupDate
                  StructureIndicator = env.StructureIndicator
                  MaintenancePlant = env.MaintenancePlant
                  ObjectStatus = env.ObjectStatus
                  ControllingArea = env.ControllingArea
                  CompanyCode = env.CompanyCode
                  Currency = env.Currency
                }
            (props, st)

    let private unlistBy (fn: 'a -> Template<'b>) (source: 'a list) : Template<'b list> = 
        Template <| fun env state -> 
            let rec work xs st cont = 
                match xs with 
                | [] -> cont [] st
                | x :: rest -> 
                    let (a,s1) = apply1 (fn x) env st
                    work rest s1 (fun vs s2 -> cont (a :: vs) s2)
            work source state (fun xs st -> (xs, st))


    let freshEquiId() : Template<string> = 
        Template <| fun env st -> 
            let ix = st.NameIndex 
            let name = sprintf "$%i" ix
            (name, { st with NameIndex = ix + 1 })


    type SiteCode = string
    
    [<Struct>]
    type Site = 
        | Site of (SiteCode -> Template<S4FunctionalLocation>)
    
    let internal getSite (x: Site) : SiteCode -> Template<S4FunctionalLocation> = 
        let (Site f) = x in f

    [<Struct>]
    type Function = 
        | Function of (FuncLocPath -> Template<S4FunctionalLocation>)
    
    let internal getFunction (x: Function) : FuncLocPath -> Template<S4FunctionalLocation> = 
        let (Function f) = x in f

    [<Struct>]
    type ProcessGroup = 
        | ProcessGroup of (FuncLocPath -> Template<S4FunctionalLocation>)

    let internal getProcessGroup (x: ProcessGroup) : FuncLocPath -> Template<S4FunctionalLocation> = 
        let (ProcessGroup f) = x in f
        
    [<Struct>]
    type Process = 
        | Process of (FuncLocPath -> Template<S4FunctionalLocation>)

    let internal getProcess (x: Process) : FuncLocPath -> Template<S4FunctionalLocation> = 
        let (Process f) = x in f

    [<Struct>]
    type System = 
        | System of (FuncLocPath -> Template<S4FunctionalLocation>)

    let internal getSystem (x: System) : FuncLocPath -> Template<S4FunctionalLocation> = 
        let (System f) = x in f

    [<Struct>]
    type Assembly = 
        | Assembly of (FuncLocPath -> Template<S4FunctionalLocation>)

    let internal getAssembly (x: Assembly) : FuncLocPath -> Template<S4FunctionalLocation> = 
        let (Assembly f) = x in f

    [<Struct>]
    type Item = 
        | Item of (FuncLocPath -> Template<S4FunctionalLocation>)

    let internal getItem (x: Item) : FuncLocPath -> Template<S4FunctionalLocation> = 
        let (Item f) = x in f

    [<Struct>]
    type Component = 
        | Component of (FuncLocPath -> Template<S4FunctionalLocation>)
    
    let internal getComponent (x: Component) : FuncLocPath -> Template<S4FunctionalLocation> = 
        let (Component f) = x in f


    
    type ClassName = string

    [<Struct>]
    type FlocClass = 
        | FlocClass of (FuncLocPath -> Template<S4FlocClassification list>)

    let internal getFlocClass (x: FlocClass) : FuncLocPath -> Template<S4FlocClassification list> = 
        let (FlocClass f) = x in f


    [<Struct>]
    type FlocCharacteristic = 
        | FlocCharacteristic of (FuncLocPath -> ClassName -> Template<S4FlocClassification>)
    
    let internal getFlocCharacteristic (x: FlocCharacteristic) : FuncLocPath -> ClassName -> Template<S4FlocClassification> = 
        let (FlocCharacteristic f) = x in f


    /// Note - this is more compliciated than the others.
    /// EquiId is generated by the Template Monad
    [<Struct>]
    type Equipment = 
        | Equipment of (EquipmentId option -> FuncLocPath -> Template<S4Equipment>)

    let internal getEquipment (x: Equipment) : EquipmentId option -> FuncLocPath -> Template<S4Equipment> = 
        let (Equipment f) = x in f


    [<Struct>]
    type EquiClass = 
        | EquiClass of (EquipmentId -> Template<S4EquiClassification list>)

    let internal getEquiClass (x: EquiClass) : EquipmentId -> Template<S4EquiClassification list> = 
        let (EquiClass f) = x in f


    [<Struct>]
    type EquiCharacteristic = 
        | EquiCharacteristic of (EquipmentId -> ClassName -> Template<S4EquiClassification>)

    let internal getEquiCharacteristic (x: EquiCharacteristic) : EquipmentId -> ClassName -> Template<S4EquiClassification> = 
        let (EquiCharacteristic f) = x in f

    // ************************************************************************
    // Low level builder functions 

    let _equiCharacteristic (name : string) (value : ValuaValue) : EquiCharacteristic = 
        EquiCharacteristic <| fun equiId className -> 
            mreturn { 
                EquiId = equiId
                ClassName = className
                CharName = name
                CharValue = value
                }

    let _flocCharacteristic (name : string) (value : ValuaValue) : FlocCharacteristic = 
        FlocCharacteristic <| fun funcloc className -> 
            mreturn { 
                FuncLoc = funcloc
                ClassName = className
                CharName = name
                CharValue = value
                }

    let _equiClass (name : string) (values : EquiCharacteristic list) : EquiClass = 
        EquiClass <| fun equiId ->         
            unlistBy (fun x -> getEquiCharacteristic x equiId name) values
            

    let _flocClass (name : string) (values : FlocCharacteristic list) : FlocClass = 
        FlocClass <| fun funcloc ->         
            unlistBy (fun x -> getFlocCharacteristic x funcloc name) values
            
    let _equipment (description : string) 
                    (category : string) 
                    (objectType : string)
                    (zzClass : string)
                    (startupDate: DateTime)
                    (memoLine : string)
                    (classes : EquiClass list) 
                    (subordinateEquipment : Equipment list)  : Equipment = 
        Equipment <| fun parentEqui parentFloc -> 
            template {
                let! equiId = freshEquiId ()
                let! props = getFlocProps ()
                let! cs = unlistBy (fun x -> getEquiClass x equiId) classes |>> List.concat
                let! es = unlistBy (fun x -> getEquipment x (Some equiId) parentFloc) subordinateEquipment
                return {
                    EquiId = equiId
                    SuperEquiId = parentEqui
                    FuncLoc = parentFloc
                    FlocProperties = props
                    Description = description
                    Category = category
                    ZZClass = zzClass
                    ObjectType = objectType
                    StartupDate = startupDate
                    Manufacturer = None
                    Model = None
                    SerialNumber = None
                    ConstructionYear = None
                    ConstructionMonth = None
                    Classifications = cs 
                    SuboridinateEquipment = es 
                    MemoLine = memoLine
                }
            } 

    let _component (levelCode: string) (description : string) (objectType : string)
                    (classes : FlocClass list)  
                    (equipment : Equipment list) : Component = 
        Component <| fun parentCode -> 
            template { 
                let floc = parentCode |> extend levelCode
                let! props = getFlocProps ()
                let! cs = unlistBy (fun x -> getFlocClass x floc) classes |>> List.concat
                let! es = unlistBy (fun x -> getEquipment x None floc) equipment
                return {
                    FuncLoc = floc
                    FlocProperties = props
                    Description = description
                    ObjectType = objectType
                    Classifications = cs
                    SubFlocs = []
                    Equipment = es
                }
            }

    let _item (levelCode: string) (description : string) (objectType : string)
                    (classes : FlocClass list) 
                    (components : Component list) 
                    (equipment : Equipment list) : Item = 
        Item <| fun parentCode -> 
            template { 
                let floc = parentCode |> extend levelCode
                let! props = getFlocProps ()
                let! cs = unlistBy (fun x -> getFlocClass x floc) classes |>> List.concat
                let! xs = unlistBy (fun x -> getComponent x floc) components
                let! es = unlistBy (fun x -> getEquipment x None floc) equipment
                return {
                    FuncLoc = floc
                    FlocProperties = props
                    Description = description
                    ObjectType = objectType
                    Classifications = cs
                    SubFlocs = xs
                    Equipment = es
                }
            }

    let _assembly (levelCode: string) (description : string) (objectType : string)
                    (classes : FlocClass list) 
                    (items : Item list) 
                    (equipment : Equipment list) : Assembly = 
        Assembly <| fun parentCode -> 
            template { 
                let floc = parentCode |> extend levelCode
                let! props = getFlocProps ()
                let! cs = unlistBy (fun x -> getFlocClass x floc) classes |>> List.concat
                let! xs = unlistBy (fun x -> getItem x floc) items
                let! es = unlistBy (fun x -> getEquipment x None floc) equipment
                return {
                    FuncLoc = floc
                    FlocProperties = props
                    Description = description
                    ObjectType = objectType
                    Classifications = cs
                    SubFlocs = xs
                    Equipment = es
                }
            }

    let _system (levelCode: string) (description : string) (objectType : string)
                    (classes : FlocClass list) 
                    (assemblies : Assembly list) 
                    (equipment : Equipment list) : System = 
        System <| fun parentCode -> 
            template { 
                let floc = parentCode |> extend levelCode
                let! props = getFlocProps ()
                let! cs = unlistBy (fun x -> getFlocClass x floc) classes |>> List.concat
                let! xs = unlistBy (fun x -> getAssembly x floc) assemblies
                let! es = unlistBy (fun x -> getEquipment x None floc) equipment
                return {
                    FuncLoc = floc
                    FlocProperties = props
                    Description = description
                    ObjectType = objectType
                    Classifications = cs
                    SubFlocs = xs
                    Equipment = es
                }
            }

    let _process (levelCode: string) (description : string) (objectType : string)
                        (classes : FlocClass list) 
                        (systems : System list) : Process = 
        Process <| fun parentCode -> 
            template { 
                let floc = parentCode |> extend levelCode
                let! props = getFlocProps ()
                let! cs = unlistBy (fun x -> getFlocClass x floc) classes |>> List.concat
                let! xs = unlistBy (fun x -> getSystem x floc) systems
                return {
                    FuncLoc = floc
                    FlocProperties = props
                    Description = description
                    ObjectType = objectType
                    Classifications = cs
                    SubFlocs = xs
                    Equipment = []
                }
            }

    let _processGroup (levelCode: string) (description : string) (objectType : string)
                    (classes : FlocClass list) 
                    (processes : Process list) : ProcessGroup = 
        ProcessGroup <| fun parentCode -> 
            template { 
                let floc = parentCode |> extend levelCode
                let! props = getFlocProps ()
                let! cs = unlistBy (fun x -> getFlocClass x floc) classes |>> List.concat
                let! xs = unlistBy (fun x -> getProcess x floc) processes
                return {
                    FuncLoc = floc
                    FlocProperties = props
                    Description = description
                    ObjectType = objectType
                    Classifications = cs
                    SubFlocs = xs
                    Equipment = []
                }
            }

    let _function (levelCode: string) (description : string) (objectType : string)
                        (classes : FlocClass list) 
                        (processGroups : ProcessGroup list) : Function = 
        Function <| fun parentCode -> 
            template { 
                let floc = parentCode |> extend levelCode
                let! props = getFlocProps ()
                let! cs = unlistBy (fun x -> getFlocClass x floc) classes |>> List.concat
                let! xs = unlistBy (fun x -> getProcessGroup x floc) processGroups
                return {
                    FuncLoc = floc
                    FlocProperties = props
                    Description = description
                    ObjectType = objectType
                    Classifications = cs
                    SubFlocs = xs
                    Equipment = []
                }
            }
            
    let _site (description : string) 
                (classes : FlocClass list) 
                (functions : Function list) : Site = 
        Site <| fun siteCode -> 
            template {
                let floc = FuncLocPath.Create siteCode
                let! props = getFlocProps ()
                let! cs = unlistBy (fun x -> getFlocClass x floc) classes |>> List.concat
                let! xs = unlistBy (fun x -> getFunction x floc) functions
                return { 
                    FuncLoc = floc
                    FlocProperties = props
                    Description = description
                    ObjectType = "SITE"
                    Classifications = cs
                    SubFlocs = xs
                    Equipment = []
                }
            }