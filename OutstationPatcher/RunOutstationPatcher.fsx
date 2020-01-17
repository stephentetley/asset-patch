#r "netstandard"
#r "System.Text.Encoding.dll"
#r "System.Xml.Linq"
#r "System.Xml.ReaderWriter"
#r "System.Xml.XDocument"
#r "System.IO.FileSystem.Primitives"
open System.IO

#I @"C:\Users\stephen\.nuget\packages\ExcelProvider\1.0.1\lib\netstandard2.0"
#r "ExcelProvider.Runtime.dll"

#I @"C:\Users\stephen\.nuget\packages\ExcelProvider\1.0.1\typeproviders\fsharp41\netstandard2.0"
#r "ExcelDataReader.DataSet.dll"
#r "ExcelDataReader.dll"
#r "ExcelProvider.DesignTime.dll"

#I @"C:\Users\stephen\.nuget\packages\system.io.packaging\4.5.0\lib\netstandard1.3"
#r "System.IO.Packaging"
#I @"C:\Users\stephen\.nuget\packages\DocumentFormat.OpenXml\2.9.1\lib\netstandard1.3"
#r "DocumentFormat.OpenXml"


#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"


open FSharp.Core

#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190721\lib\netstandard2.0"
#r "SLFormat.dll"

#I @"C:\Users\stephen\.nuget\packages\markdowndoc\1.0.1-alpha-20191014\lib\netstandard2.0"
#r "MarkdownDoc.dll"

#I @"C:\Users\stephen\.nuget\packages\sheetdoc\1.0.0-alpha-20191121a\lib\netstandard2.0"
#r "SheetDoc.dll"


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

let outputDirectory (child : string) : string = 
    match child with 
    | null | "" -> Path.Combine(__SOURCE_DIRECTORY__, @"..\output")
    | _ -> Path.Combine(__SOURCE_DIRECTORY__, @"..\output", child)


let options : OsPatcherOptions = 
    {   UserName = "TETLEYS"
        OutputDirectory = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\patch_output"
        WorkListPath =    @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\MMIM_upgrade_2019_worklist1.xlsx" 
    }

let main01 () = 
    runOutstationPatcherPhase1 options 


// Generate ClassEqui and ValuaEqui files for Equipment 
// once it has been activated and downloaded...
let main02 () = 
    let equiFile = @"G:\work\Projects\assets\asset_patch\__oustation_equi_download_with_dollar_numbers.txt"
    runOutstationPatcherPhase2 options equiFile  



