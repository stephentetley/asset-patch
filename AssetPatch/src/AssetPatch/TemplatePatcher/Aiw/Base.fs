// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Aiw

// EquiIndex is a workaround because we don't know Equipment number (EQUI)
// until we have activated the Equi patch and downloaded the generated EQUI numbers.

module Base =
    
    open System.IO
    
    open AssetPatch.Base
    open AssetPatch.Base.Common
    open AssetPatch.Base.FuncLocPath
    open AssetPatch.Base.Aiw.ChangeFileParser
    open AssetPatch.TemplatePatcher.Base.CompilerMonad



    /// Equi could be a dollar number
    type EquiIndex = 
        { Equi : string 
          Txtmi : string
          TplnEilo : string
        }

    type EquiKey = 
        { Description : string 
          FuncLoc : FuncLocPath }

    type EquiDollarId = string

    type EquiMap = Map<EquiKey, EquiDollarId>

    let emptyEquiMap : EquiMap = Map.empty

    let buildEquiMap (indices : EquiIndex list) : EquiMap = 
        let add1 (acc : EquiMap) (ix : EquiIndex) = 
            let key = { Description = ix.Txtmi; FuncLoc = FuncLocPath.Create ix.TplnEilo }
            Map.add key ix.Equi acc
        List.fold add1 Map.empty indices

    let private extractEquiIndex (row : AssocList<string, string>) : EquiIndex option = 
        match AssocList.tryFind3 "EQUI" "TXTMI" "TPLN_EILO" row with
        | Some (a,b,c) -> 
            match a with
            | null | "" -> None
            | _ -> Some { Equi  = a;  Txtmi = b; TplnEilo = c }
        | None -> None
    
    let readEquiDownload (path : string) : Result<EquiMap, ErrMsg> = 
        match readAiwChangeFile path with
        | Error msg -> Error msg
        | Ok changes -> 
            changes 
                |> fun x -> x.RowAssocs()
                |> List.map extractEquiIndex
                |> List.choose id
                |> buildEquiMap
                |> Ok

    let tryFindEquiNum (description : string) 
                (funcLoc : FuncLocPath) 
                (indices : EquiMap) : string option = 
        Map.tryFind { Description = description; FuncLoc = funcLoc } indices

    type AiwEnv = 
        { UserName: string 
          EquiIndices: EquiMap option }

    type AiwCompilerMonad<'a> = CompilerMonad<'a, AiwEnv>


    
    let getEquiNumber (description: string) (funcLoc: FuncLocPath) : AiwCompilerMonad<string> = 
        compile {
            let! uenv = asksUserEnv (fun env -> env.EquiIndices)
            match uenv with
            | None -> return! throwError "No EquiMap set up. This is required for equipment."
            | Some equiMap ->             
                match tryFindEquiNum description funcLoc equiMap with
                 | None -> return! throwError (sprintf "Missing equipment - %s '%s'" (funcLoc.ToString()) description)
                 | Some a -> return a
        }
