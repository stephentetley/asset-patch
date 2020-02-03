// Copyright (c) Stephen Tetley 2020

#r "netstandard"
#r "System.Xml.Linq.dll"
#r "System.Text.Encoding.dll"
open System.IO
open System.Text.RegularExpressions


// Use FSharp.Data for CSV reading
#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.3.3\lib\netstandard2.0"
#r @"FSharp.Data.dll"



#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"


open FSharp.Core

#load "..\..\AssetPatch\src\AssetPatch\Base\CsvFile.fs"
#load "..\..\AssetPatch\src\AssetPatch\Analysis\Utilities\ReadErrorLog.fs"
open AssetPatch.Base.CsvFile
open AssetPatch.Anaylsis.Utilities.ReadErrorLog

let sampleLog = @"G:\work\Projects\assets\asset_patch\error_logs\preprod-errors-stopping-retire.txt"
let sampleOut = @"G:\work\Projects\assets\asset_patch\error_logs\COLLECT_preprod-errors-stopping-retire.csv"

let sampleLine = "Equipment 000000000101016676:Assignment of Obj. Type NETW specific Classification is mandatory for 000000000101016676"


let getEquiIdAndType: Matcher<option<int64 * string>> = 
    pipeV2 (matchValue "0000\d+") 
           (alt (matchValue "NETW") (matchValue "PODE")) 
           (fun x y -> (int64 x,y)) |> optional
    

let demo01() = 
    File.ReadAllLines(sampleLog) |> choose getEquiIdAndType

let demo02() = 
    let ans1 = Regex.Match(input= "Hello 123", pattern = "\d+")
    if ans1.Success then
        Ok(ans1.Value)
    else
        Error("no match")

let demo03() = 
    let makeRow (i: int64, s: string) = [| i.ToString(); s|]
    match File.ReadAllLines(sampleLog) |> choose getEquiIdAndType with
    | Error msg -> printfn "FATAL:\n%s" msg
    | Ok vals -> 
        let csv : CsvFileWithHeaders = 
            { Headers = [| "EquiId"; "ClassType"|] 
              Rows = List.map makeRow vals
            }
        writeCsvFile csvDefaults csv sampleOut
