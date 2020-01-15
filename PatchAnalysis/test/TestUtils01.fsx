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

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"

#I @"C:\Users\stephen\.nuget\packages\markdowndoc\1.0.1-alpha-20191014\lib\netstandard2.0"
#r "MarkdownDoc.dll"

#load "..\..\AssetPatch\src\AssetPatch\Base\Addendum.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\Common.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\AssocList.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\ChangeFile.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\Acronyms.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\AbsChangeFile.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\Parser.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\Printer.fs"
#load "..\src\PatchAnalysis\Utilities\ChangeFileReport.fs"
#load "..\src\PatchAnalysis\Utilities\TidyChangeFile.fs"
open AssetPatch.Base.Parser
open PatchAnaylsis.Utilities.ChangeFileReport
open PatchAnaylsis.Utilities.TidyChangeFile


let outputDirectory () : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\output")

let outputFile (relFileName : string) : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\output", relFileName)

/// This is calculated from the folder where pandoc is invoked in
/// eg: X:\coding\work\asset-patch\output to X:\coding\libs\markdown-css-master\"
let pathToCss = @"..\..\..\libs\markdown-css-master\github.css"

let outputChangeReport1 (sourcePath : string) = 
    changeFileReport pathToCss (outputDirectory ()) sourcePath


let reportChangeFiles (dir: string) = 
    let sources = 
        System.IO.Directory.GetFiles( path = dir, searchPattern = "*.txt")
    Array.iter (outputChangeReport1 >> ignore) sources

let reportChangeFiles01 () = 
    reportChangeFiles @"G:\work\Projects\assets\asset_patch\env_discharge_2019\patch_output"




let tidyChangeFile01 () = 
    let src = @"G:\work\Projects\assets\asset_patch\file_download_edm\ACO01_funcloc_file_download.txt"
    let dest = outputFile "ACO01_funcloc_tidy.txt"
    tidyChangeFile ["FUNCLOC"; "TXTMI"] [] src dest


let temp01 () = 
    let file = @"G:\work\Projects\assets\asset_patch\file_download_edm\control_automation_04_equi.txt"
    changeFileReport pathToCss (outputDirectory ()) file

let testParser (file : string) = 
    match runParserOnFile (parseChangeFile ()) () file Text.Encoding.UTF8 with
    | FParsec.CharParsers.ParserResult.Failure (str,_,_) -> Result.Error str
    | FParsec.CharParsers.ParserResult.Success (ans,_,_) -> Result.Ok ans

let temp02 () = 
    let file = @"G:\work\Projects\assets\asset_patch\file_download_edm\control_automation_04_equi.txt"
    testParser file

let temp03 () = 
    let file = @"G:\work\Projects\assets\asset_patch\file_download_edm\ACO01_funcloc_file_download.txt"
    testParser file
