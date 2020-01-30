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
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\Ctos.fs"
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\Netw.fs"
#load "src\AssetPatch\OutstationPatcher\InputData.fs"
#load "src\AssetPatch\OutstationPatcher\OutstationTemplate.fs"
#load "src\AssetPatch\OutstationPatcher\OutstationPatcher.fs"
open AssetPatch.OutstationPatcher


let options : OsPatcherOptions = 
    {   UserName = "TETLEYS"
        WorkListPath =    @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\preprod\MMIM_preprod_upgrade_2019_worklist_part2.xlsx" 
        OutputDirectory = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\preprod\patch_output"        
    }

let main01 () = 
    runOutstationPatcherPhase1 options 


// Generate ClassEqui, ValuaEqui and Eqmltxt files for Equipment 
// once it has been activated and downloaded...
// Note - parsing the equi file is currently far from robust.
let main02 () = 
    let equiFile = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\preprod\workings\equi_file_download_FOR_INDEXING_jan30_initial02.txt"
    runOutstationPatcherPhase2 options equiFile  



