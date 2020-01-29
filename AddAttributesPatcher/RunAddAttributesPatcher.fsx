#r "netstandard"
#r "System.Text.Encoding.dll"

#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"


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
#load "src\AssetPatch\AddAttributesPatcher\AddAttributesPatcher.fs"
open AssetPatch.TemplatePatcher.Template
open AssetPatch.TemplateCatalogue.Base
open AssetPatch.TemplateCatalogue.Netw
open AssetPatch.AddAttributesPatcher

// This patcher is a bit more general than the other ones
    
let addNETWTL : Class = 
    netwtl [ 
        uniclass_code ()
        uniclass_desc ()
    ]

let demo01() = 
    let input = [ ("FLA01-CAA-NET-TEL-SYS01", "101208938") ] |> List.map (fun (x,y) -> makeEquiRow x y)
    let opts = 
        { UserName = "TETLEYS"
          FilePrefix = "SAMPLE_add_netwtl_attributes"
          OutputDirectory = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\patch_output"
        }
    generateEquipmentAttibutes opts input [addNETWTL]



