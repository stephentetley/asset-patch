// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace DisposeEquiPatcher

module DisposeEquiPatcher =

    open System

    open AssetPatch.Base
    open AssetPatch.Base.ChangeFile
    open AssetPatch.Base.Acronyms
    open AssetPatch.Base.Printer
    open AssetPatch.Base.FuncLocPath
    open DisposeEquiPatcher.InputData


    let private makeAssocs (items : (string * string * string) list) : AssocList<string,string> = 
        items |> List.map (fun (x,_,y) -> (x,y)) |> AssocList.ofList


    type DisposeEqui = 
        { EquipmentId: string           
          FuncLoc : FuncLocPath
          DescriptionMediumText : string
          ObjectType: string
          Status: string
        }

        member x.ToAssocs() : AssocList<string, string> =         
            makeAssocs
                [ ("EQUI",          "Equipment",                        x.EquipmentId)
                ; ("TXTMI",         "Description (medium text)",        x.DescriptionMediumText)  
                ; ("USTA_EQUI",     "Display lines for user status",    x.Status)
                ; ("TPLN_EILO",     "Functional Location",              x.FuncLoc.ToString())
                ; ("EQART_EQU",     "Object Type",                      x.ObjectType)
                ; ("USTW_EQUI",     "Status of an object",              x.Status)
                ]


    let private inputRowToDisposeEqui (row: WorkListRow) : DisposeEqui = 
        { EquipmentId = row.``S4 Equipment Id``         
          FuncLoc = row.``Functional Location`` |> FuncLocPath.Create
          DescriptionMediumText = row.``Name ``
          ObjectType = row.``Object Type``
          Status = "DISP"
        }


    /// At least one row exists 
    let private getHeaderRow (rows : AssocList<string, string> list) : HeaderRow = 
        match rows with
        | [] -> failwith "getHeaderRow - empty list"
        | row1 :: _ -> row1 |> AssocList.keys |> HeaderRow

    let private makeHeader (user : string) 
                            (variantName : string)
                            (timestamp : DateTime) : FileHeader = 
        { 
            FileType = Upload 
            DataModel = U1
            EntityType = EntityType.Equi
            Variant = variantName
            User = user
            DateTime = timestamp 
        }
        

    let private makeChangeFile (username: string) (rows : DisposeEqui list) : ChangeFile = 
        let timestamp = DateTime.Now
        let assocRows = rows |> List.map (fun x -> x.ToAssocs())
        let headerRow = getHeaderRow assocRows
        let header = makeHeader username "Variant TODO" timestamp 
        { 
            Header = header
            Selection = None
            HeaderDescriptions = getHeaderDescriptions EntityType.Equi headerRow |> Some
            HeaderRow = headerRow
            DataRows = List.map DataRow.FromAssocList assocRows        
        }

    type DisposeEquiPatcherOptions = 
        { UserName : string 
          WorkListPath : string
          OutputFile : string
          }

    let runDisposeEquiPatcher (opts : DisposeEquiPatcherOptions) : Result<unit, string> = 
        let worklist = readWorkList opts.WorkListPath |> List.map inputRowToDisposeEqui
        let changeFile = makeChangeFile opts.UserName worklist
        writeChangeFile opts.OutputFile changeFile |> Ok
