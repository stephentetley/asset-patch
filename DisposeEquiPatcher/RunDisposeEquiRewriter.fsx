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
#load "..\AssetPatch\src\AssetPatch\RewritePatcher\Base\UpdateTypes.fs"
#load "..\AssetPatch\src\AssetPatch\RewritePatcher\Base\Rewrite.fs"
#load "..\AssetPatch\src\AssetPatch\RewritePatcher\Base\RewriteMonad.fs"
#load "..\AssetPatch\src\AssetPatch\RewritePatcher\Base\Uxl.fs"
#load "..\AssetPatch\src\AssetPatch\RewritePatcher\Catalogue\EquiRoot.fs"
#load "src\AssetPatch\DisposeEqui\InputData.fs"
#load "src\AssetPatch\DisposeEqui\UxlDisposeEquiRewriter.fs"
open AssetPatch.RewritePatcher.Base.Rewrite
open AssetPatch.RewritePatcher.Base.RewriteMonad
open AssetPatch.RewritePatcher.Base.Uxl
open AssetPatch.RewritePatcher.Catalogue.EquiRoot
open AssetPatch.DisposeEqui.InputData
open AssetPatch.DisposeEqui.UxlDisposeEquiRewriter





    
let outputDirectory   = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\qa\patch_output\csv"

let test01 () = 
    let worklist = 
        readWorkList @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\qa\QA_MM3X_retire_2019_worklist2.xlsx"
            |> List.map WorkRow
    let changeRequestName = sprintf "S3953 MMIM Retires %s" (DateTime.Now.ToShortDateString())
    
    runRewriter (defaultEnv changeRequestName ())
        <| rewriter { 
                let! changes = rewriteEquiAll (equiDispose ()) worklist
                do! writeChangeRequestAndEquipmentPatches changes outputDirectory "outstation_rewrite"
                return ()
            }

