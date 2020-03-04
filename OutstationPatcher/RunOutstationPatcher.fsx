#r "netstandard"
#r "System.Xml.Linq.dll"
#r "System.Text.Encoding.dll"
open System

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
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Base\Hierarchy.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Base\Template.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Base\GenerateMonad.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Aiw\Base.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Aiw\FileTypes.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Aiw\Emitter.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Aiw\Generate.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Uxl\Base.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Uxl\Emitter.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Uxl\Generate.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Catalogue\Floc.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Catalogue\Equi.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Catalogue\AssetCondition.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Catalogue\Ctos.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Catalogue\Netw.fs"
#load "src\AssetPatch\OutstationPatcher\InputData.fs"
#load "src\AssetPatch\OutstationPatcher\OutstationTemplate.fs"
#load "src\AssetPatch\OutstationPatcher\AiwPatcher.fs"
#load "src\AssetPatch\OutstationPatcher\UxlPatcher.fs"
open AssetPatch.OutstationPatcher.AiwPatcher
open AssetPatch.OutstationPatcher.UxlPatcher

let aiwOptions : AiwOptions = 
    {   UserName = "TETLEYS"
        WorkListPath =    @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\qa\QA_MMIM_upgrade_2019_worklist1_20_outstations.xlsx" 
        OutputDirectory = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\qa\patch_output"
    }

let aiwOutstationUpgrades01 () = 
    runAiwOutstationPatcherCreateFlocPhase aiwOptions 

    
let aiwOutstationFlocPatches01 () = 
    runAiwOutstationPatcherCreateFlocPhase aiwOptions 
    
let aiwOutstationEquiPatchesPhase01 () = 
    runAiwOutstationPatcherCreateEquiPhase aiwOptions


// Generate ClassEqui, ValuaEqui and Eqmltxt files for Equipment 
// once it has been activated and downloaded...
// Note - parsing the equi file is currently far from robust.
let aiwOutstationEquiPatchesPhase02 () = 
    let equiFile = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\preprod\workings\equi_file_download_FOR_INDEXING_jan30_the_rest.txt"
    runAiwOutstationPatcherAnnotateEquiPhase aiwOptions equiFile


let uxlOptions : UxlOptions = 
    { ProcessRequester  = "ASSET DATA"
      ChangeRequestDescription = sprintf "S3953 MMIM Upgrades %s" (DateTime.Now.ToShortDateString())
      WorkListPath      = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\qa\QA_MMIM_upgrade_2019_worklist1_20_outstations.xlsx" 
      OutputDirectory   = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\qa\patch_output\csv" 
    }

let uxlOutstationUpgrades01 () = 
    runUxlOutstationPatcher uxlOptions

