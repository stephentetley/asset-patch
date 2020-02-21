// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Base



module Template =
    
    open System

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.Base.ValuaValue 
    open AssetPatch.TemplatePatcher.Base.Hierarchy   

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

    /// Template is Reader + State (fresh ids) + Error.
    /// Error turns out to be useful.
    /// 'env has common props "structure indicator" etc.
    type Template<'a> = 
        | Template of (TemplateEnv -> TemplateState -> Result<'a * TemplateState, ErrMsg>)


    let inline private apply1 (ma : Template<'a>) 
                                (env : TemplateEnv) 
                                (st: TemplateState) : Result<'a * TemplateState, ErrMsg> = 
        let (Template fn) = ma in fn env st

    let mreturn (x:'a) : Template<'a> = 
        Template <| fun _ st -> Ok (x, st)


    let inline private bindM (ma : Template<'a>) 
                             (fn : 'a -> Template<'b>) : Template<'b> =
        Template <| fun env st -> 
            match apply1 ma env st with
            | Error msg -> Error msg
            | Ok (a, s1) -> apply1 (fn a) env s1
         
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
                    (nameStart: int) : Result<'a * int, ErrMsg> = 
        let stateZero = { NameIndex = nameStart }
        match apply1 ma env stateZero  with
        | Error msg -> Error msg
        | Ok (a, s1) -> Ok (a, s1.NameIndex)

    let private ( |>> ) (ma : Template<'a>) (fn : 'a -> 'b) : Template<'b> = 
        Template <| fun env st ->
            match apply1 ma env st with
            | Error msg -> Error msg
            | Ok (a, s1) -> Ok (fn a, s1)

    let private apM (mf: Template<'a -> 'b>) (ma: Template<'a>) : Template<'b> = 
        Template <| fun env st ->
            match apply1 mf env st with
            | Error msg -> Error msg
            | Ok (f, s1) -> 
                match apply1 ma env s1 with
                | Error msg -> Error msg
                | Ok (a, s2) -> Ok (f a, s2)

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
            Ok (props, st)

    let private unlistBy (fn: 'a -> Template<'b>) (source: 'a list) : Template<'b list> = 
        Template <| fun env state -> 
            let rec work xs st fk sk = 
                match xs with 
                | [] -> sk [] st
                | x :: rest -> 
                    match apply1 (fn x) env st with
                    | Error msg -> fk msg
                    | Ok (a, s1) -> 
                        work rest s1 fk (fun vs s2 -> sk (a :: vs) s2)
            work source state (fun msg -> Error msg) (fun xs st -> Ok (xs, st))


    let freshEquiId() : Template<string> = 
        Template <| fun env st -> 
            let ix = st.NameIndex 
            let name = sprintf "$%i" ix
            Ok (name, { st with NameIndex = ix + 1 })

    let templateError (msg: string) : Template<'a> = 
        Template <| fun _ _ -> Error msg

    type EnvTransformer = TemplateEnv -> TemplateEnv
    
    let local (modify : EnvTransformer) (ma : Template<'a>) : Template<'a> = 
        Template <| fun env -> 
            apply1 ma (modify env)
    
    let locals (modifications : EnvTransformer list) (ma : Template<'a>) : Template<'a> = 
        let trafo = List.foldBack (fun f acc -> acc >> f) modifications id
        Template <| fun env -> 
            apply1 ma (trafo env) 
    
                
    let startupDate (date : DateTime) : EnvTransformer = 
        fun env -> { env with StartupDate = date }




   // *************************************************************************
   // Template 'objects'

    type SiteCode = string
    
    [<Struct>]
    type Site = 
        | Site of (SiteCode -> Template<S4FunctionalLocation>)
    
    let internal getSite (x: Site) : SiteCode -> Template<S4FunctionalLocation> = 
        let (Site f) = x in f

    let siteError (msg: string) : Site = 
        Site <| fun _ -> templateError msg

    [<Struct>]
    type Function = 
        | Function of (FuncLocPath -> Template<S4FunctionalLocation>)
    
    let internal getFunction (x: Function) : FuncLocPath -> Template<S4FunctionalLocation> = 
        let (Function f) = x in f

    let functionError (msg: string) : Function = 
        Function <| fun _ -> templateError msg

    [<Struct>]
    type ProcessGroup = 
        | ProcessGroup of (FuncLocPath -> Template<S4FunctionalLocation>)

    let internal getProcessGroup (x: ProcessGroup) : FuncLocPath -> Template<S4FunctionalLocation> = 
        let (ProcessGroup f) = x in f
       
    let processGroupError (msg: string) : ProcessGroup = 
        ProcessGroup <| fun _ -> templateError msg

    [<Struct>]
    type Process = 
        | Process of (FuncLocPath -> Template<S4FunctionalLocation>)

    let internal getProcess (x: Process) : FuncLocPath -> Template<S4FunctionalLocation> = 
        let (Process f) = x in f

    let processError (msg: string) : Process = 
        Process <| fun _ -> templateError msg

    [<Struct>]
    type System = 
        | System of (FuncLocPath -> Template<S4FunctionalLocation>)

    let internal getSystem (x: System) : FuncLocPath -> Template<S4FunctionalLocation> = 
        let (System f) = x in f

    let systemError (msg: string) : System = 
        System <| fun _ -> templateError msg

    [<Struct>]
    type Assembly = 
        | Assembly of (FuncLocPath -> Template<S4FunctionalLocation>)

    let internal getAssembly (x: Assembly) : FuncLocPath -> Template<S4FunctionalLocation> = 
        let (Assembly f) = x in f

    let assemblyError (msg: string) : Assembly = 
        Assembly <| fun _ -> templateError msg

    [<Struct>]
    type Item = 
        | Item of (FuncLocPath -> Template<S4FunctionalLocation>)

    let internal getItem (x: Item) : FuncLocPath -> Template<S4FunctionalLocation> = 
        let (Item f) = x in f

    let itemError (msg: string) : Item = 
        Item <| fun _ -> templateError msg

    [<Struct>]
    type Component = 
        | Component of (FuncLocPath -> Template<S4FunctionalLocation>)
    
    let internal getComponent (x: Component) : FuncLocPath -> Template<S4FunctionalLocation> = 
        let (Component f) = x in f

    let componentError (msg: string) : Component = 
        Component <| fun _ -> templateError msg

    
    type ClassName = string

    [<Struct>]
    type FlocClass = 
        internal | FlocClass of (FuncLocPath -> Template<S4FlocClassification list>)

    let internal getFlocClass (x: FlocClass) : FuncLocPath -> Template<S4FlocClassification list> = 
        let (FlocClass f) = x in f

    let flocClassError (msg: string) : FlocClass = 
        FlocClass <| fun _ -> templateError msg


    [<Struct>]
    type FlocCharacteristic = 
        | FlocCharacteristic of (FuncLocPath -> ClassName -> Template<S4FlocClassification>)
    
    let internal getFlocCharacteristic (x: FlocCharacteristic) : FuncLocPath -> ClassName -> Template<S4FlocClassification> = 
        let (FlocCharacteristic f) = x in f

    let flocCharacteristicError (msg: string) : FlocCharacteristic = 
        FlocCharacteristic <| fun _ _ -> templateError msg

    /// Note - EquiId is generated by the Template Monad
    /// The supplied `EquipmentId option` is the superordinate equiment (if exists)
    [<Struct>]
    type Equipment = 
        | Equipment of (EquipmentId option -> FuncLocPath -> Template<S4Equipment>)

    let internal getEquipment (x: Equipment) : EquipmentId option -> FuncLocPath -> Template<S4Equipment> = 
        let (Equipment f) = x in f

    let equipmentError (msg: string) : Equipment = 
        Equipment <| fun _ _ -> templateError msg


    type EquipmentAttribute = Template<S4Equipment -> S4Equipment>

    let private applyAttribute (e1 : Equipment) (attrib : EquipmentAttribute) : Equipment = 
        Equipment <| fun parentId floc -> 
            let template = getEquipment e1 parentId floc
            apM attrib template

    let private setAttributes (e1 : Equipment) (attribs : EquipmentAttribute list) : Equipment = 
        List.fold applyAttribute e1 attribs

    let internal equipmentAttribute (update : S4Equipment -> S4Equipment) : EquipmentAttribute =
        Template <| fun _ st -> Ok (update, st)

    [<Struct>]
    type EquiClass = 
        | EquiClass of (EquipmentId -> Template<S4EquiClassification list>)

    let internal getEquiClass (x: EquiClass) : EquipmentId -> Template<S4EquiClassification list> = 
        let (EquiClass f) = x in f

    let equiClassError (msg: string) : EquiClass = 
        EquiClass <| fun _ -> templateError msg

    [<Struct>]
    type EquiCharacteristic = 
        | EquiCharacteristic of (EquipmentId -> ClassName -> Template<S4EquiClassification>)

    let internal getEquiCharacteristic (x: EquiCharacteristic) : EquipmentId -> ClassName -> Template<S4EquiClassification> = 
        let (EquiCharacteristic f) = x in f


    let equiCharacteristicError (msg: string) : EquiCharacteristic = 
        EquiCharacteristic <| fun _ _ -> templateError msg

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
                    (attributes : EquipmentAttribute list)
                    (classes : EquiClass list) 
                    (subordinateEquipment : Equipment list)  : Equipment = 
        (Equipment <| fun parentEqui parentFloc ->              
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
                    Class = zzClass
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
            }) |>  fun e1 -> setAttributes e1 attributes
        
            

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