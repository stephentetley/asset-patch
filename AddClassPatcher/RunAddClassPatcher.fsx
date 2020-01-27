#r "netstandard"
#r "System.Text.Encoding.dll"

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
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\EmitCommon.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\EmitEquipment.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\EmitFuncLoc.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\Emitter.fs"
#load "..\AssetPatch\src\AssetPatch\TemplatePatcher\PatchCompiler.fs"
#load "..\AssetPatch\src\AssetPatch\Lib\Common.fs"
#load "..\AssetPatch\src\AssetPatch\Lib\OSGB36.fs"
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\Base.fs"
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\AssetCondition.fs"
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\Lstn.fs"
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\Smon.fs"
#load "..\AssetPatch\src\AssetPatch\TemplateCatalogue\Netw.fs"
#load "src\AssetPatch\AddClassPatcher\AddClassPatcher.fs"
open AssetPatch.TemplatePatcher.Template
open AssetPatch.TemplateCatalogue.Base
open AssetPatch.TemplateCatalogue.Netw
open AssetPatch.AddClassPatcher


// let addPODEUP (equiNumber: string) : Class = 
    
let addNETWTL (equiNumber: string) : Class = 
    netwtl [ 
        uniclass_code ()
        uniclass_desc ()
    ]
