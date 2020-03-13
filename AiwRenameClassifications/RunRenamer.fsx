#r "netstandard"
#r "System.Xml.Linq"
#r "System.Text.Encoding.dll"
open System

#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"

// Use FSharp.Data for CSV reading
// Must reference "System.Xml.Linq"
#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.3.3\lib\netstandard2.0"
#r @"FSharp.Data.dll"
#r @"FSharp.Data.DesignTime.dll"
open FSharp.Data

#load "..\AssetPatch\src\AssetPatch\Base\Addendum.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Common.fs"
#load "..\AssetPatch\src\AssetPatch\Base\AssocList.fs"
#load "..\AssetPatch\src\AssetPatch\Base\CsvFile.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Aiw\ChangeFile.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Aiw\Acronyms.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Aiw\ChangeFileParser.fs"
#load "src\AssetPatch\AiwRenameClassifications\Renamer.fs"
open AssetPatch.AiwRenameClassifications.Renamer

let demo01() = 
    let stagingFile =  @"G:\work\Projects\assets\asset_patch\outstation-worklists-march2020\file_download_CAA_floc_download_for_staging_ids.txt"
    let src = 
        [ @"G:\work\Projects\assets\asset_patch\outstation-worklists-march2020\patch_output\01_create_flocs\outstation_patch_level2_step2_classes.txt"
        ; @"G:\work\Projects\assets\asset_patch\outstation-worklists-march2020\patch_output\01_create_flocs\outstation_patch_level2_step3_characteristics.txt"
        ]
    renamePatches stagingFile src