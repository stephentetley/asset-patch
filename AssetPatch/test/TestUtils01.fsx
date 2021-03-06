﻿// Copyright (c) Stephen Tetley 2019

#r "netstandard"
#r "System.Text.Encoding.dll"
open System
open System.IO

#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"
open FParsec

open FSharp.Core


#load "..\..\AssetPatch\src\AssetPatch\Base\Addendum.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\Common.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\AssocList.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\Aiw\ChangeFile.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\Aiw\Acronyms.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\Aiw\ChangeFileParser.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\Aiw\ChangeFilePrinter.fs"
#load "..\..\AssetPatch\src\AssetPatch\Analysis\AbsChangeFile.fs"
#load "..\..\AssetPatch\src\AssetPatch\Analysis\Utilities\TidyChangeFile.fs"
open AssetPatch.Base.Aiw.ChangeFileParser
open AssetPatch.Anaylsis.Utilities.TidyChangeFile




let outputFile (relFileName : string) : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\output", relFileName)


let tidyChangeFile01 () = 
    let src = @"G:\work\Projects\assets\asset_patch\file_download_edm\ACO01_funcloc_file_download.txt"
    let dest = outputFile "ACO01_funcloc_tidy.txt"
    tidyChangeFile ["FUNCLOC"; "TXTMI"] [] src dest


let testParser (file : string) = 
    match runParserOnFile (parseAiwChangeFile ()) () file Text.Encoding.UTF8 with
    | FParsec.CharParsers.ParserResult.Failure (str,_,_) -> Result.Error str
    | FParsec.CharParsers.ParserResult.Success (ans,_,_) -> Result.Ok ans

let temp01 () = 
    let file = @"G:\work\Projects\assets\asset_patch\file_download_edm\control_automation_04_equi.txt"
    testParser file

let temp02 () = 
    let file = @"G:\work\Projects\assets\asset_patch\file_download_edm\ACO01_funcloc_file_download.txt"
    testParser file

let temp03 () = 
    let file = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_worklist1_mocked_download.txt"
    testParser file

let temp04 () = 
    let file = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\workings\file_download_telemetry_systems.txt"
    testParser file

