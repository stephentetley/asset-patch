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

#load "..\AssetPatch\src\AssetPatch\Lib\Common.fs"
#load "DisposeEquiPatcher\InputData.fs"
#load "DisposeEquiPatcher\DisposeEquiPatcher.fs"
open DisposeEquiPatcher.DisposeEquiPatcher


