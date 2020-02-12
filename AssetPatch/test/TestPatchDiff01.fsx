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


#load "..\..\AssetPatch\src\AssetPatch\Base\Addendum.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\Common.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\AssocList.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\Aiw\ChangeFile.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\Aiw\Acronyms.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\Aiw\ChangeFileParser.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\Aiw\ChangeFilePrinter.fs"
#load "..\..\AssetPatch\src\AssetPatch\Analysis\AbsChangeFile.fs"
#load "..\..\AssetPatch\src\AssetPatch\Analysis\PatchDiff.fs"
open AssetPatch.Analysis.PatchDiff



let reportDiffs01 () = 
    let src1=  @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\preprod\workings\preprod+outstation_retire+01_before_download+attempt05.txt"
    let src2 = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\preprod\workings\preprod+outstation_retire+02_after_download+attempt05.txt"
    let dest = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\preprod\workings\html\preprod+outstation-retire-changes+attempt05.html"
    reportRowDiffs "EQUI" src1 src2 dest
         