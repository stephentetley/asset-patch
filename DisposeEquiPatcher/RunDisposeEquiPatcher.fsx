#r "netstandard"
#r "System.Xml.Linq.dll"
#r "System.Text.Encoding.dll"

// Use FSharp.Data for CSV reading and writing
#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.3.3\lib\netstandard2.0"
#r @"FSharp.Data.dll"


#I @"C:\Users\stephen\.nuget\packages\ExcelProvider\1.0.1\lib\netstandard2.0"
#r "ExcelProvider.Runtime.dll"

#I @"C:\Users\stephen\.nuget\packages\ExcelProvider\1.0.1\typeproviders\fsharp41\netstandard2.0"
#r "ExcelDataReader.DataSet.dll"
#r "ExcelDataReader.dll"
#r "ExcelProvider.DesignTime.dll"


#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"


#load "..\AssetPatch\src\AssetPatch\Base\Addendum.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Common.fs"
#load "..\AssetPatch\src\AssetPatch\Base\AssocList.fs"
#load "..\AssetPatch\src\AssetPatch\Base\ChangeFile.fs"
#load "..\AssetPatch\src\AssetPatch\Base\CsvFile.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Acronyms.fs"
#load "..\AssetPatch\src\AssetPatch\Base\FuncLocPath.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Parser.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Printer.fs"
#load "..\AssetPatch\src\AssetPatch\Lib\Common.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Base\CommonTypes.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Base\TemplateHierarchy.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Base\Template.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Base\CompilerMonad.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Uxl\Base.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Uxl\PatchTypes.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Uxl\PatchWriter.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Uxl\Emitter.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Uxl\PatchCompiler.fs"
#load "src\AssetPatch\DisposeEquiPatcher\InputData.fs"
#load "src\AssetPatch\DisposeEquiPatcher\AiwDisposeEquiPatcher.fs"
#load "src\AssetPatch\DisposeEquiPatcher\UxlDisposeEquiPatcher.fs"
open AssetPatch.DisposeEquiPatcher.AiwDisposeEquiPatcher
open AssetPatch.DisposeEquiPatcher.UxlDisposeEquiPatcher


// TODO - derive filename from input file?
let aiwRetireMm3x () = 
    let opts = 
        { UserName          = "TETLEYS"
          WorkListPath      = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\preprod\MM3X_preprod_2019_retire_worklist1.xlsx"
          OutputDirectory   = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\preprod\patch_output\"
        }
    runAiwDisposeEquiPatcher opts

let uxlRetireMm3x () = 
    let opts = 
        { ProcessRequester  = "ASSET DATA"
          ChangeRequestDescription = "S3975 Retire MM3X 04/02/20"
          WorkListPath      = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\preprod\MM3X_preprod_2019_retire_worklist1.xlsx"
          OutputDirectory   = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\preprod\patch_output\"          
        }
    runUxlDisposeEquiPatcher opts


