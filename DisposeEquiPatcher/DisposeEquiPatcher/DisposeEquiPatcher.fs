// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace DisposeEquiPatcher

module DisposeEquiPatcher =

    open AssetPatch.Base
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


    let inputRowToDisposeEqui (row: WorkListRow) : DisposeEqui = 
        failwith "TODO"



