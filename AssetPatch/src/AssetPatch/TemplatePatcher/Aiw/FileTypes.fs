﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Aiw

/// File Types specfically pertain to `create` patches
/// So this is not in the `AssetPatch.Base.Aiw` namespace

module FileTypes =
    
    open System

    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.Base.ValuaValue
    open AssetPatch.Base.Aiw.ChangeFile
    open AssetPatch.Base.Aiw.ChangeFilePrinter
    open AssetPatch.TemplatePatcher.Base.GenerateMonad
    open AssetPatch.TemplatePatcher.Aiw.Base
   
    

    let private makeAssocs (items : (string * string * string) list) : AssocList<string,string> = 
        items |> List.map (fun (x,_,y) -> (x,y)) |> AssocList.ofList



    // ************************************************************************
    // FuncLoc



    // The other way is to look at differences to an existing root funcloc
    // Then only 8 fields change:
    //
    // 1   FUNCLOC
    // 2   TXTMI
    // 38  FLTYP
    // 42  IEQUI
    // 56  FLOC_REF  {- Magic -}
    // 62  EQART
    // 63  JOBJN_FL  {- Magic -}
    // 94  TPLMA1
    // 95  TPLMA
    
    /// Shared by Floc and Equi
    /// Note - more params may need to be added when we better understand the data
    type CommonProperties = 
        { CompanyCode : uint32
          ControllingArea : uint32
          PlantCode : uint32
          UserStatus : string
        }

    // ************************************************************************
    // Create FuncLoc

    type NewFuncLoc = 
      { _Level: int
        FunctionLocation : FuncLocPath
        Description : string
        ObjectType : string
        Category : uint32
        ObjectStatus : string
        StartupDate : DateTime
        StructureIndicator : string
        CommonProps : CommonProperties
      }
        member x.Level with get () : int = x.FunctionLocation.Level

        member x.ToAssocs() : AssocList<string, string> =   
            let parentFloc = 
                match parent x.FunctionLocation with
                | None -> ""
                | Some f0 -> f0.ToString()

            makeAssocs
                [ ("ABCKZFLOC",     "ABC Indicator",                    "")
                ; ("GSBE_FLOC",     "Business Area",                    "")
                ; ("BUKRSFLOC",     "Company Code",                     x.CommonProps.CompanyCode.ToString())
                ; ("KOKR_FLOC",     "Controlling Area",                 x.CommonProps.ControllingArea.ToString())
                ; ("KOST_FLOC",     "Cost Center",                      "")    
                ; ("TXTMI",         "Description (medium text)",        x.Description)
                ; ("USTA_FLOC",     "Display lines for user status",    x.CommonProps.UserStatus)               
                ; ("IEQUII",        "EquipInst. Origin",                "D")
                ; ("FLTYP",         "FuncLocCategory",                  x.Category.ToString())
                ; ("FUNCLOC",       "Function Location",                "")     // Must be blank
                ; ("IEQUI",         "Installation allowed",             "")
                ; ("STOR_FLOC",     "Location",                         "")
                ; ("GEWRKFLOC",     "Main work center",                 "DEFAULT")
                ; ("INGR_FLOC",     "Maint Planner Group",              "")
                ; ("SWERK_FL",      "Maintenance Plant",                x.CommonProps.PlantCode.ToString())
                ; ("FLOC_REF",      "Masked Functional Location",       x.FunctionLocation.ToString())
                ; ("OBJTYFLOC",     "Object Type",                      "")
                ; ("EQART",         "Object Type",                      x.ObjectType)
                ; ("PLNT_FLOC",     "Planning Plant",                   x.CommonProps.PlantCode.ToString())
                ; ("BEBER_FL",      "Plant Section",                    "")
                ; ("WERGWFLOC",     "Plant for WorkCenter",             x.CommonProps.PlantCode.ToString())
                ; ("TRPNR",         "Reference Location",               "")
                ; ("INBDT",         "Start-up date",                    x.StartupDate |> showS4Date)
                ; ("STATTEXT",      "Status",                           "CRTE")
                ; ("STSM_FLOC",     "Status Profile",                   "ZFLOCST")
                ; ("USTW_FLOC",     "Status of an object",              x.ObjectStatus)
                ; ("USWO_FLOC",     "Status without status number",     "")
                ; ("TPLKZ_FLC",     "Structure indicator",              x.StructureIndicator)
                ; ("TPLMA1",        "Superior FL for CR Processing",    parentFloc)
                ; ("TPLMA",         "Superior FunctLoc",                parentFloc)
                ; ("PROI_FLOC",     "WBS Element",                      "")
                ; ("ARBPLFLOC",     "Work center",                      "DEFAULT")
                ]


    let writeNewFuncLocs (source : NewFuncLoc list)
                            (outpath: string) : AiwGenerate<unit> = 
        match source with
        | [] -> mreturn ()
        | _ -> 
            generate {
                let rows = 
                    source
                        |> List.sortBy (fun row -> row.FunctionLocation.ToString()) 
                        |> List.map (fun x -> x.ToAssocs())  
                let! changes = 
                    makeChangeFile EntityType.FuncLoc "Asset Patch Create FuncLocs" rows
                do! liftAction <| fun () -> writeAiwChangeFile changes outpath
                return ()
            }

    

    // ************************************************************************
    // ClassFloc


    type NewClassFloc = 
        { _Level: int
          FuncLoc : FuncLocPath
          Class : string
          Status : int
        }

        member x.ToAssocs() : AssocList<string, string> =      
            makeAssocs
                [ ("FUNCLOC",       "Functional Location",      x.FuncLoc.ToString())
                ; ("CLASS",         "Class",                    x.Class)
                ; ("CLASSTYPE",     "Class Type",               "003")
                ; ("CLSTATUS1",     "Status",                   x.Status.ToString())
                ]

    let writeNewClassFlocs (source : NewClassFloc list)
                            (outpath: string) : AiwGenerate<unit> = 
        match source with
        | [] -> mreturn ()
        | _ -> 
            generate {
                let rows = 
                    source
                        |> List.sortBy (fun row -> row.FuncLoc.ToString()) 
                        |> List.map (fun x -> x.ToAssocs())  
                let! changes = 
                    makeChangeFile EntityType.FuncLoc "Asset Patch Create ClassFlocs" rows
                do! liftAction <| fun () -> writeAiwChangeFile changes outpath
                return ()
            }


    // ************************************************************************
    // ValuaFloc

    type NewValuaFloc = 
        { _Level: int
          FuncLoc : FuncLocPath
          ClassType : IntegerString
          CharacteristicID : string        
          ValueCount : int
          Value : ValuaValue
        }

        /// Note - Numeric values print TEXTBEZ and ATFLV
        /// textual values print ATWRT and TEXTBEZ
        /// ... This may not be wise - TEXTBEZ seems inconsistent
        /// in the target system.
        member x.ToAssocs() : AssocList<string, string> =   
            let atwrt = x.Value.CharacteristicValue |> Option.defaultValue ""         
            let atflv = x.Value.NumericValueFrom |> Option.defaultValue ""
            let atflb = x.Value.NumericValueTo |> Option.defaultValue ""
            makeAssocs
                [ ("FUNCLOC",       "Function Location",                x.FuncLoc.ToString())
                ; ("CLASSTYPE",     "Class Type",                       x.ClassType.Number)
                ; ("CHARID",        "Characteristic ID",                x.CharacteristicID)
                ; ("ATWRT",         "Characteristic Value",             atwrt)
                ; ("ATCOD",         "Code",                             "1")        // Always 1 "EQ"
                ; ("TEXTBEZ",       "Description",                      x.Value.DescriptionValue)
                ; ("VALCNT",        "Int count values",                 sprintf "%04i" x.ValueCount)
                ; ("ATFLV",         "Value from",                       atflv)
                ; ("ATFLB",         "Value to",                         atflb)
                ]

    let writeNewValuaFlocs (source : NewValuaFloc list)
                            (outpath: string) : AiwGenerate<unit> = 
        match source with
        | [] -> mreturn ()
        | _ -> 
            generate {
                let rows = 
                    source
                        |> List.sortBy (fun row -> row.FuncLoc.ToString()) 
                        |> List.map (fun x -> x.ToAssocs())  
                let! changes = 
                    makeChangeFile EntityType.FuncLoc "Asset Patch Create ValuaFlocs" rows
                do! liftAction <| fun () -> writeAiwChangeFile changes outpath
                return ()
            }

    // ************************************************************************
    // Equi

    type NewEqui = 
        { Description : string
          FuncLoc : FuncLocPath
          Category : string           // e.g. I for instrument
          ObjectType : string
          Manufacturer : string
          Model : string
          SerialNumber : string
          ConstructionYear : uint16
          ConstructionMonth : uint8
          StartupDate : DateTime
          MaintenancePlant : uint32
          Currency : string
          CommonProps : CommonProperties
        }

        member x.ToAssocs() : AssocList<string, string> =         
            makeAssocs
                [ ("BUKR_EILO",     "Company Code",                     x.CommonProps.CompanyCode.ToString())
                ; ("BAUMM_EQI",     "Construction month",               (sprintf "%02i" x.ConstructionMonth))
                ; ("BAUJJ",         "Construction year",                x.ConstructionYear.ToString())
                ; ("KOKR_EILO",     "Controlling Area",                 x.CommonProps.ControllingArea.ToString())
                ; ("WAERS",         "Currency",                         x.Currency)
                ; ("TXTMI",         "Description (medium text)",        x.Description)  
                ; ("USTA_EQUI",     "Display lines for user status",    x.CommonProps.UserStatus)
                ; ("EQTYP",         "Equipment category",               x.Category)   
                ; ("TPLN_EILO",     "Functional Location",              x.FuncLoc.ToString())
                ; ("SERGE",         "ManufSerialNumber",                x.SerialNumber)
                ; ("HERST",         "Manufacturer",                     x.Manufacturer)
                ; ("TYPBZ",         "Model number",                     x.Model)
                ; ("OBJT_EQUI",     "Object Type",                      "")
                ; ("EQART_EQU",     "Object Type",                      x.ObjectType)
                ; ("INBDT",         "Start-up date",                    x.StartupDate |> showS4Date)
                ; ("USTW_EQUI",     "Status of an object",              "UCON")
                ]

    let writeNewEquis (source : NewEqui list)
                            (outpath: string) : AiwGenerate<unit> = 
        match source with
        | [] -> mreturn ()
        | _ -> 
            generate {
                let rows = 
                    source
                        |> List.sortBy (fun row -> row.FuncLoc.ToString()) 
                        |> List.map (fun x -> x.ToAssocs())  
                let! changes = 
                    makeChangeFile EntityType.FuncLoc "Asset Patch Create Equis" rows
                do! liftAction <| fun () -> writeAiwChangeFile changes outpath
                return ()
            }

    // ************************************************************************
    // ClassEqui
    


    type NewClassEqui = 
        { EquipmentId : string
          Class : string
          Status : int
        }

        member x.ToAssocs() : AssocList<string, string> = 
            makeAssocs
                [ ("EQUI",          "Equipment",                x.EquipmentId.ToString())
                ; ("CLASS",         "Class",                    x.Class)
                ; ("CLASSTYPE",     "Class Type",               "002")
                ; ("CLSTATUS1",     "Status",                   x.Status.ToString())
                ]

    let writeNewClassEquis (source : NewClassEqui list)
                            (outpath: string) : AiwGenerate<unit> = 
        match source with
        | [] -> mreturn ()
        | _ -> 
            generate {
                let rows = 
                    source
                        |> List.sortBy (fun row -> row.EquipmentId) 
                        |> List.map (fun x -> x.ToAssocs())  
                let! changes = 
                    makeChangeFile EntityType.FuncLoc "Asset Patch Create ClassEquis" rows
                do! liftAction <| fun () -> writeAiwChangeFile changes outpath
                return ()
            }

    // ************************************************************************
    // ValuaEqui


    /// ValueCount is the number of instances for this charcteristic 
    /// in a class.
    type NewValuaEqui = 
        { EquipmentId : string
          ClassType : IntegerString
          CharacteristicID : string
          ValueCount : int
          Value : ValuaValue
        }
    

        /// Note - CharacteristicValue is used twice.
        /// Be careful about TEXTBEZ - it is inconsistent in the 
        /// target system
        member x.ToAssocs() : AssocList<string, string> = 
            let atwrt = x.Value.CharacteristicValue |> Option.defaultValue ""         
            let atflv = x.Value.NumericValueFrom    |> Option.defaultValue ""
            let atflb = x.Value.NumericValueTo      |> Option.defaultValue ""
            makeAssocs
                [ ("EQUI",          "Equipment",                x.EquipmentId.ToString())
                ; ("CLASSTYPE",     "Class Type",               x.ClassType.Number)
                ; ("CHARID",        "Characteristic ID",        x.CharacteristicID)
                ; ("ATWRT",         "Characteristic Value",     atwrt)
                ; ("ATCOD",         "Code",                     "1")        // Always 1 "EQ"
                ; ("TEXTBEZ",       "Description",              x.Value.DescriptionValue)
                ; ("VALCNT",        "Int counter values",       sprintf "%04i" x.ValueCount)
                ; ("ATFLV",         "Value from",               atflv)
                ; ("ATFLB",         "Value to",                 atflb)
                ]
            
    let writeNewValuaEquis (source : NewValuaEqui list)
                            (outpath: string) : AiwGenerate<unit> = 
        match source with
        | [] -> mreturn ()
        | _ -> 
            generate {
                let rows = 
                    source
                        |> List.sortBy (fun row -> row.EquipmentId) 
                        |> List.map (fun x -> x.ToAssocs())  
                let! changes = 
                    makeChangeFile EntityType.FuncLoc "Asset Patch Create ValuaEquis" rows
                do! liftAction <| fun () -> writeAiwChangeFile changes outpath
                return ()
            }

    // ************************************************************************
    // Eqmltxt


    /// LanguageKey is always "EN"
    type NewEqmltxt = 
        { EquipmentId : string
          Description : string
          LongText : string
          MoreTextExists : bool
        }
    

        /// Note - CharacteristicValue is used twice.
        member x.ToAssocs() : AssocList<string, string> = 
            makeAssocs
                [ ("EQUI",          "Equipment",                    x.EquipmentId.ToString())
                ; ("SHORTXT",       "Description (medium text)",    x.Description)
                ; ("LANGUCODE",     "Language Key",                 "EN")
                ; ("EQ_LTXT",       "Long Text",                    x.LongText)
                ; ("LTXTIND",       "More Text Exists",             (if x.MoreTextExists then "X" else ""))        // Always 1 "EQ"
                ]
            
    let writeNewEqmltxts (source : NewEqmltxt list)
                            (outpath: string) : AiwGenerate<unit> = 
        match source with
        | [] -> mreturn ()
        | _ -> 
            generate {
                let rows = 
                    source
                        |> List.sortBy (fun row -> row.EquipmentId) 
                        |> List.map (fun x -> x.ToAssocs())  
                let! changes = 
                    makeChangeFile EntityType.FuncLoc "Asset Patch Create Eqmltxt" rows
                do! liftAction <| fun () -> writeAiwChangeFile changes outpath
                return ()
            }