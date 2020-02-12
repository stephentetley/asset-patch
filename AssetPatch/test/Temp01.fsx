// Copyright (c) Stephen Tetley 2019

#r "netstandard"
#r "System.Text.Encoding.dll"
open System
open System.IO

#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"
open FParsec

open FSharp.Core


#load "..\src\AssetPatch\Base\Addendum.fs"
#load "..\src\AssetPatch\Base\Common.fs"
#load "..\src\AssetPatch\Base\AssocList.fs"
#load "..\src\AssetPatch\Base\FuncLocPath.fs"
#load "..\src\AssetPatch\Base\Aiw\ChangeFile.fs"
#load "..\src\AssetPatch\Base\Aiw\Acronyms.fs"
#load "..\src\AssetPatch\Base\Aiw\ChangeFileParser.fs"
#load "..\src\AssetPatch\Base\Aiw\ChangeFilePrinter.fs"
#load "..\src\AssetPatch\TemplatePatcher\Base\CommonTypes.fs"
#load "..\src\AssetPatch\TemplatePatcher\Base\TemplateHierarchy.fs"
#load "..\src\AssetPatch\TemplatePatcher\Base\Template.fs"
#load "..\src\AssetPatch\TemplatePatcher\Base\CompilerMonad.fs"
#load "..\src\AssetPatch\TemplatePatcher\Aiw\Base.fs"
open AssetPatch.Base
open AssetPatch.Base.Aiw.ChangeFileParser
open AssetPatch.TemplatePatcher.Aiw.Base

type EquiIndex = 
    { Equi : uint32 
      Txtmi : string
      TplnEilo : string
    }

let extractEquiIndex (row : AssocList<string, string>) : EquiIndex option = 
    match AssocList.tryFind3 "EQUI" "TXTMI" "TPLN_EILO" row with
    | Some (a,b,c) -> 
        try 
            let num = uint32 a 
            Some { Equi  = num;  Txtmi = b; TplnEilo = c }
        with
        | _ -> None
    | None -> None

let demo01 () = 
    readAiwChangeFile @"G:\work\Projects\assets\asset_patch\samples\equi_sample_to_derive_indexing2.txt"
        |> Result.map (fun x -> x.RowAssocs () |> List.map extractEquiIndex |> List.choose id)
        |> Result.map (List.iter (printfn "%O"))

let demo02 () = 
    readEquiDownload @"G:\work\Projects\assets\asset_patch\samples\equi_sample_to_derive_indexing2.txt"
   