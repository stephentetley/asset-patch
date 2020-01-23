// Copyright (c) Stephen Tetley 2019

#r "netstandard"
#r "System.Text.Encoding.dll"
open System.IO

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
open AssetPatch.Base.ChangeFile
open AssetPatch.Base.Parser
open AssetPatch.Analysis.PatchDiff


let readFiles (sourcePath1: string) (sourcePath2: string) : Result<ChangeFile * ChangeFile, string> = 
    match readChangeFile sourcePath1 with
    | Error msg1 -> Error msg1
    | Ok ans1 -> 
        match readChangeFile sourcePath2 with
        | Error msg2 -> Error msg2
        | Ok ans2 -> Ok (ans1, ans2)


let reportDiffs01 () = 
    let src1=  @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\workings\preprod+outstation_retire+before_download.txt"
    let src2 = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\workings\preprod+outstation_retire+after_download.txt"
    match readFiles src1 src2 with
    | Error msg -> Error msg
    | Ok (a,b) -> diffChangeFiles "EQUI" a b


