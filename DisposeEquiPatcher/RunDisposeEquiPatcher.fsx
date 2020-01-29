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
#load "..\AssetPatch\src\AssetPatch\Lib\Common.fs"
#load "src\AssetPatch\DisposeEquiPatcher\InputData.fs"
#load "src\AssetPatch\DisposeEquiPatcher\DisposeEquiPatcher.fs"
open AssetPatch.DisposeEquiPatcher


// TODO - derive filename from input file?
let retireMm3x01 () = 
    let opts = 
        { UserName          = "TETLEYS"
          WorkListPath      = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\preprod\MM3X_preprod_2019_retire_worklist1.xlsx"
          OutputDirectory   = @"G:\work\Projects\assets\asset_patch\mmim_upgrade_2019\preprod\patch_output\"
          }
    runDisposeEquiPatcher opts


