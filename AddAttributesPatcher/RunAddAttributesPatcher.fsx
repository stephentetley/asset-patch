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
#load "..\AssetPatch\src\AssetPatch\Base\FuncLocPath.fs"
#load "..\AssetPatch\src\AssetPatch\Base\ValuaValue.fs"
#load "..\AssetPatch\src\AssetPatch\Base\CsvFile.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Aiw\ChangeFile.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Aiw\Acronyms.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Aiw\ChangeFileParser.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Aiw\ChangeFilePrinter.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Uxl\FileTypes.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Base\TemplateHierarchy.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Base\Template.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Base\CompilerMonad.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Aiw\Base.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Aiw\PatchTypes.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Aiw\PatchWriter.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Aiw\EmitPhase1.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Aiw\EmitPhase2.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Aiw\EmitNewAttributes.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Aiw\PatchCompiler.fs"
#load "..\AssetPatch\src\AssetPatch\Lib\Common.fs"
#load "..\AssetPatch\src\AssetPatch\Lib\OSGB36.fs"
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\Root.fs"
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\AssetCondition.fs"
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\Lstn.fs"
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\Smon.fs"
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\Netw.fs"
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\Pode.fs"
#load "src\AssetPatch\AddAttributesPatcher\AiwAddAttributesPatcher.fs"
open AssetPatch.TemplatePatcher.Base.Template
open AssetPatch.TemplateCatalogue.Root
open AssetPatch.TemplateCatalogue.Netw
open AssetPatch.TemplateCatalogue.Pode
open AssetPatch.AddAttributesPatcher.AiwAddAttributesPatcher

// This patcher is a bit more general than the other ones
    
let addNETWTL : Classification = 
    netwtl [ 
        uniclass_code ()
        uniclass_desc ()
    ]

let addPODEUP : Classification = 
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
    generateEquipmentAttibutes opts podes [addPODEUP] |> ignore

    let netws = 
        inputRows () 
            |> List.filter (fun (row: InputRow) -> row.ClassType = "NETW")
            |> List.map (fun row -> row.EquiId.ToString() )

    let optsNETW = { opts with FilePrefix = "preprod_add_NETW_attributes" }
    generateEquipmentAttibutes optsNETW netws [addNETWTL]
