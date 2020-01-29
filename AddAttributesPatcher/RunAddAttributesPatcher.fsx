#r "netstandard"
#r "System.Xml.Linq"
#r "System.Text.Encoding.dll"

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
#load "..\AssetPatch\src\AssetPatch\Base\ChangeFile.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Acronyms.fs"
#load "..\AssetPatch\src\AssetPatch\Base\AbsChangeFile.fs"
#load "..\AssetPatch\src\AssetPatch\Base\FuncLocPath.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Parser.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Printer.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\CommonTypes.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\PatchTypes.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\EquiIndexing.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\TemplateHierarchy.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Template.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\CompilerMonad.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\PatchWriter.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\EmitPhase1.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\EmitPhase2.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\EmitNewAttributes.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\PatchCompiler.fs"
#load "..\AssetPatch\src\AssetPatch\Lib\Common.fs"
#load "..\AssetPatch\src\AssetPatch\Lib\OSGB36.fs"
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\Base.fs"
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\AssetCondition.fs"
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\Lstn.fs"
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\Smon.fs"
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\Netw.fs"
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\Pode.fs"
#load "src\AssetPatch\AddAttributesPatcher\AddAttributesPatcher.fs"
open AssetPatch.TemplatePatcher.Template
open AssetPatch.TemplateCatalogue.Base
open AssetPatch.TemplateCatalogue.Netw
open AssetPatch.TemplateCatalogue.Pode
open AssetPatch.AddAttributesPatcher

// This patcher is a bit more general than the other ones
    
let addNETWTL : Class = 
    netwtl [ 
        uniclass_code ()
        uniclass_desc ()
    ]

let addPODEUP : Class = 
    podeup [ 
        uniclass_code ()
        uniclass_desc ()
    ]

type InputTable = 
    CsvProvider<Sample = @"G:\work\Projects\assets\asset_patch\error_logs\preprod-errors-stopping-retire_FUNCLOC_added.csv",
                 HasHeaders = true >

type InputRow = InputTable.Row


let inputRows () : InputRow list  = (new InputTable()).Rows |> Seq.toList


let demo01() = 
    let opts = 
        { UserName = "TETLEYS"
          FilePrefix = "preprod_add_PODE_attributes"
          OutputDirectory = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\patch_output"
        }
    let podes = 
        inputRows () 
            |> List.filter (fun (row: InputRow) -> row.ClassType = "PODE")
            |> List.map (fun row -> row.EquiId.ToString() )
    
    // TODO - add a strategy that allow podes and netws in same iteration
    generateEquipmentAttibutes opts podes [addPODEUP] |> ignore

    let netws = 
        inputRows () 
            |> List.filter (fun (row: InputRow) -> row.ClassType = "NETW")
            |> List.map (fun row -> row.EquiId.ToString() )

    let optsNETW = { opts with FilePrefix = "preprod_add_NETW_attributes" }
    generateEquipmentAttibutes optsNETW netws [addNETWTL]
