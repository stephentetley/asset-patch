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


#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"

#I @"C:\Users\stephen\.nuget\packages\markdowndoc\1.0.1-alpha-20191014\lib\netstandard2.0"
#r "MarkdownDoc.dll"


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
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\EmitCommon.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\EmitEquipment.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\EmitFuncLoc.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Emitter.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\PatchCompiler.fs"
#load "..\AssetPatch\src\AssetPatch\Lib\Common.fs"
#load "..\AssetPatch\src\AssetPatch\Lib\OSGB36.fs"
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\Base.fs"
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\AssetCondition.fs"
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\Ctos.fs"
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\Netw.fs"
#load "OutstationPatcher\InputData.fs"
#load "OutstationPatcher\OutstationTemplate.fs"
#load "OutstationPatcher\OutstationPatcher.fs"
open OutstationPatcher.OutstationPatcher


let options : OsPatcherOptions = 
    {   UserName = "TETLEYS"
        OutputDirectory = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\patch_output"
        WorkListPath =    @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\MMIM_upgrade_2019_worklist1.xlsx" 
    }

let main01 () = 
    runOutstationPatcherPhase1 options 


// Generate ClassEqui, ValuaEqui and Eqmltxt files for Equipment 
// once it has been activated and downloaded...
// Note - parsing the equi file is currently far from robust.
let main02 () = 
    let equiFile = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\mmim_upgrade_worklist1_mocked_download.txt"
    runOutstationPatcherPhase2 options equiFile  



