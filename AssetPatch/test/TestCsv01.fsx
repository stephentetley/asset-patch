// Copyright (c) Stephen Tetley 2020

#r "netstandard"
#r "System.Xml.Linq.dll"
#r "System.Text.Encoding.dll"
open System.IO

// Use FSharp.Data for CSV reading
#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.3.3\lib\netstandard2.0"
#r @"FSharp.Data.dll"



#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"


open FSharp.Core


#load "..\..\AssetPatch\src\AssetPatch\Base\Addendum.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\Common.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\AssocList.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\ChangeFile.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\Acronyms.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\AbsChangeFile.fs"
#load "..\..\AssetPatch\src\AssetPatch\Base\Parser.fs"
#load "..\..\AssetPatch\src\AssetPatch\Lib\WriteCsv.fs"
#load "..\..\AssetPatch\src\AssetPatch\Analysis\Utilities\CsvExport.fs"
open AssetPatch.Base.Parser
open AssetPatch.Analysis.Utilities.CsvExport



let outputCsv1 (destDir: string) (sourcePath: string) : Result<Unit, string> = 
    match readChangeFile sourcePath with
    | Error msg -> Error msg
    | Ok ans -> 
        let filename = Path.GetFileName sourcePath
        let outfile = Path.Combine(destDir, filename) |> fun x -> Path.ChangeExtension(x, "csv")
        exportCsv ans outfile |> Ok


let outputAsCsv (sourcePath: string) : Result<Unit, string> = 
    match readChangeFile sourcePath with
    | Error msg -> Error msg
    | Ok ans -> 
        let filename = Path.GetFileName sourcePath
        let outfile = Path.ChangeExtension(path = sourcePath, extension = "csv")
        exportCsv ans outfile |> Ok


let exportCsvFiles (srcDir: string) (destDir: string): Unit = 
    let sources = 
        System.IO.Directory.GetFiles( path = srcDir, searchPattern = "*.txt")
    Array.iter (outputCsv1 destDir >> ignore) sources

let exportCsv01 () = 
    let src =  @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\workings\"
    let dest = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\workings\csv"
    exportCsvFiles src dest



let exportCsv02 () = 
    let src =  @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\preprod\workings\sample_SURVEY_DATE_on_NETW_file_download.txt"
    outputAsCsv src

