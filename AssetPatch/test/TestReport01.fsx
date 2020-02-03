// Copyright (c) Stephen Tetley 2019

#r "netstandard"
#r "System.Text.Encoding.dll"
open System.IO

#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"

#I @"C:\Users\stephen\.nuget\packages\giraffe\4.0.1\lib\netstandard2.0"
#r "Giraffe"

open FSharp.Core

#load "..\..\AssetPatch\src\AssetPatch\Base\Addendum.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\Common.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\AssocList.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\ChangeFile.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\Acronyms.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\Parser.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\Printer.fs"
#load "..\..\AssetPatch\src\AssetPatch\Analysis\AbsChangeFile.fs"
#load "..\..\AssetPatch\src\AssetPatch\Analysis\Utilities\ChangeFileReport.fs"
open AssetPatch.Anaylsis.Utilities.ChangeFileReport


/// Copy this file to the ouput directory so Markdown can find it easily
let pathToCss () = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\..\..\..\libs\markdown-css-master\github.css")

let outputChangeReport1 (destDir: string) (sourcePath: string) : Result<Unit, string> = 
    changeFileReport (pathToCss ()) destDir sourcePath


let reportChangeFiles (srcDir: string) (destDir: string): Unit = 
    let sources = 
        System.IO.Directory.GetFiles( path = srcDir, searchPattern = "*.txt")
    Array.iter (outputChangeReport1 destDir >> ignore) sources

let reportChangeFiles01 () = 
    let src =  @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\workings\"
    let dest = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\workings\html"
    reportChangeFiles src dest

