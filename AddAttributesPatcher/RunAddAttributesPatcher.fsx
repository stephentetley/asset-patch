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
#load "..\AssetPatch\src\AssetPatch\Base\FuncLocPath.fs"
#load "..\AssetPatch\src\AssetPatch\Base\ValuaValue.fs"
#load "..\AssetPatch\src\AssetPatch\Base\CsvFile.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Aiw\ChangeFile.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Aiw\Acronyms.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Aiw\ChangeFileParser.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Aiw\ChangeFilePrinter.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Uxl\FileTypes.fs"
#load "..\AssetPatch\src\AssetPatch\Lib\Common.fs"
#load "..\AssetPatch\src\AssetPatch\Lib\OSGB36.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Base\Hierarchy.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Base\Template.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Base\GenerateMonad.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Aiw\Base.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Aiw\FileTypes.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Aiw\Emitter.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Aiw\Generate.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Aiw\GenerateAttributes.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Uxl\Base.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Uxl\Emitter.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Uxl\Generate.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Uxl\GenerateAttributes.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Catalogue\Equi.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Catalogue\AssetCondition.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Catalogue\Lstn.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Catalogue\Smon.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Catalogue\Netw.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Catalogue\Pode.fs"
open AssetPatch.TemplatePatcher.Base.Template
open AssetPatch.TemplatePatcher.Catalogue.Equi
open AssetPatch.TemplatePatcher.Catalogue.Netw
open AssetPatch.TemplatePatcher.Catalogue.Pode
open AssetPatch.TemplatePatcher.Aiw.GenerateAttributes
open AssetPatch.TemplatePatcher.Uxl.GenerateAttributes

// This patcher is a bit more general than the other ones
    
let addNETWTL : EquiClass = 
    netwtl [ 
        uniclass_code ()
        uniclass_desc ()
    ]

let addPODEUP : EquiClass = 
    podeup [ 
        uniclass_code ()
        uniclass_desc ()
    ]


type InputTable = 
    CsvProvider<Sample = @"G:\work\Projects\assets\asset_patch\error_logs\preprod-errors-stopping-retire_FUNCLOC_added.csv",
                 HasHeaders = true >

type InputRow = InputTable.Row


let inputRows () : InputRow list  = (new InputTable()).Rows |> Seq.toList


let aiwAttrs01() = 
    let opts : AiwOptions = 
        { UserName = "TETLEYS"
          FilePrefix = "preprod_add_PODE_attributes"
          OutputDirectory = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\preprod\patch_output"
        }
    let podes = 
        inputRows () 
            |> List.filter (fun (row: InputRow) -> row.ClassType = "PODE")
            |> List.map (fun row -> row.EquiId.ToString() )
    
    // TODO - add a strategy that allow podes and netws in same iteration
    genAiwEquiAttibutes opts podes [addPODEUP] |> ignore

    let netws = 
        inputRows () 
            |> List.filter (fun (row: InputRow) -> row.ClassType = "NETW")
            |> List.map (fun row -> row.EquiId.ToString() )

    let optsNETW = { opts with FilePrefix = "preprod_add_NETW_attributes" }
    genAiwEquiAttibutes optsNETW netws [addNETWTL]

let uxlAttrs01() = 
    let opts : UxlOptions = 
        { ProcessRequester = "ASSET DATA"
          ChangeRequestDescription = sprintf "S3953 MMIM Retires %s" (DateTime.Now.ToShortDateString())
          
          FilePrefix = "preprod_add_PODE_attributes"
          OutputDirectory = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\preprod\patch_output\csv"
        }
    let podes = 
        inputRows () 
            |> List.filter (fun (row: InputRow) -> row.ClassType = "PODE")
            |> List.map (fun row -> row.EquiId.ToString() )
    
    // TODO - add a strategy that allow podes and netws in same iteration
    genUxlEquiAttibutes opts podes [addPODEUP] |> ignore

    let netws = 
        inputRows () 
            |> List.filter (fun (row: InputRow) -> row.ClassType = "NETW")
            |> List.map (fun row -> row.EquiId.ToString() )

    let optsNETW = { opts with FilePrefix = "preprod_add_NETW_attributes" }
    genUxlEquiAttibutes optsNETW netws [addNETWTL]
