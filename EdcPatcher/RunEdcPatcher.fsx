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
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Uxl\Base.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Uxl\Emitter.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Uxl\PatchCompiler.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Catalogue\Root.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Catalogue\AssetCondition.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Catalogue\Lstn.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Catalogue\Smon.fs"
#load "src\AssetPatch\EdcPatcher\InputData.fs"
#load "src\AssetPatch\EdcPatcher\EdcTemplate.fs"
#load "src\AssetPatch\EdcPatcher\AiwPatcher.fs"
#load "src\AssetPatch\EdcPatcher\UxlPatcher.fs"
open AssetPatch.EdcPatcher.AiwPatcher
open AssetPatch.EdcPatcher.UxlPatcher


let aiwOptions : AiwOptions = 
    { UserName          = "TETLEYS"
      WorkListPath      = @"G:\work\Projects\assets\asset_patch\env_discharge_2019\qa\EA_discharge_2019_sample1.xlsx" 
      OutputDirectory   = @"G:\work\Projects\assets\asset_patch\env_discharge_2019\qa\patch_output"
    }

let aiwEdcPatches01 () = 
    runAiwEdcPatcherPhase1 aiwOptions 


// Generate ClassEqui, ValuaEqui and Eqmltxt files for Equipment 
// once it has been activated and downloaded...
let aiwEdcPatches02 () = 
    let equiFile = @"G:\work\Projects\assets\asset_patch\env_discharge_2019\qa\env_discharge_worklist1_mocked_download.txt"
    runAiwEdcPatcherPhase2 aiwOptions equiFile


let uxlOptions : UxlOptions = 
    { ProcessRequester  = "ASSET DATA"
      ChangeRequestDescription = "T0875 Event Duration Monitoring"
      WorkListPath      = @"G:\work\Projects\assets\asset_patch\env_discharge_2019\qa\EA_discharge_2019_sample1.xlsx" 
      OutputDirectory   = @"G:\work\Projects\assets\asset_patch\env_discharge_2019\qa\patch_output\csv"
    }


let uxlEdcPatches01 () = 
    runUxlEdcPatcher uxlOptions