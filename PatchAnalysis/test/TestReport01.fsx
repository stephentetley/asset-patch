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
open PatchAnaylsis.Utilities.ChangeFileReport


/// Copy this file to the ouput directory so Markdown can find it easily
let pathToCss () = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\..\..\libs\markdown-css-master\github.css")

let outputChangeReport1 (destDir: string) (sourcePath: string) : Result<Unit, string> = 
    let cssDest = Path.Combine(destDir, "github.css")
    if not <| File.Exists(cssDest) then
        File.Copy(sourceFileName = pathToCss (), destFileName =  cssDest)
    else ()
    changeFileReport "github.css" destDir sourcePath


let reportChangeFiles (srcDir: string) (destDir: string): Unit = 
    let sources = 
        System.IO.Directory.GetFiles( path = srcDir, searchPattern = "*.txt")
    Array.iter (outputChangeReport1 destDir >> ignore) sources

let reportChangeFiles01 () = 
    let src =  @"G:\work\Projects\assets\asset_patch\env_discharge_2019\patch_output"
    let dest = @"G:\work\Projects\assets\asset_patch\env_discharge_2019\patch_output\html"
    reportChangeFiles src dest

