#r "netstandard"
#r "System.Text.Encoding.dll"


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
#load "..\AssetPatch\src\AssetPatch\Base\Acronyms.fs"
#load "..\AssetPatch\src\AssetPatch\Base\AbsChangeFile.fs"
#load "..\AssetPatch\src\AssetPatch\Base\FuncLocPath.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Parser.fs"
#load "..\AssetPatch\src\AssetPatch\Base\Printer.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Base\CommonTypes.fs"
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
#load "src\AssetPatch\EdcPatcher\InputData.fs"
#load "src\AssetPatch\EdcPatcher\EdcTemplate.fs"
#load "src\AssetPatch\EdcPatcher\EdcPatcher.fs"
open AssetPatch.EdcPatcher



let options : EdcPatcherOptions = 
    { UserName          = "TETLEYS"
      OutputDirectory   = @"G:\work\Projects\assets\asset_patch\env_discharge_2019\patch_output"
      WorkListPath      = @"G:\work\Projects\assets\asset_patch\env_discharge_2019\EA_discharge_2019_worklist1.xlsx" 
    }

let main01 () = 
    runEdcPatcherPhase1 options 


// Generate ClassEqui, ValuaEqui and Eqmltxt files for Equipment 
// once it has been activated and downloaded...
let main02 () = 
    let equiFile = @"G:\work\Projects\assets\asset_patch\env_discharge_worklist1_mocked_download.txt"
    runEdcPatcherPhase2 options equiFile

