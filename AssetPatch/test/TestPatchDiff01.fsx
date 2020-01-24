// Copyright (c) Stephen Tetley 2019

#r "netstandard"
#r "System.Text.Encoding.dll"
open System.IO

#I @"C:\Users\stephen\.nuget\packages\giraffe\4.0.1\lib\netstandard2.0"
#r "Giraffe"

#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"


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
#load "..\..\AssetPatch\src\AssetPatch\Analysis\PatchDiff.fs"
open AssetPatch.Analysis.PatchDiff





let reportDiffs01 () = 
    let src1=  @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\workings\preprod+outstation_retire+before_download.txt"
    let src2 = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\workings\preprod+outstation_retire+after_download.txt"
    let dest = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\workings\html\preprod+outstation-retire-changes.html"
    reportRowDiffs "EQUI" src1 src2 dest
         