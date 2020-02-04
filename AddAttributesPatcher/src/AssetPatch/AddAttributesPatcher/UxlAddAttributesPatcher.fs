// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.AddAttributesPatcher


module UxlAddAttributesPatcher =

    open AssetPatch.Base.FuncLocPath
    open AssetPatch.TemplatePatcher.Base.CompilerMonad
    open AssetPatch.TemplatePatcher.Base.Template
    open AssetPatch.TemplatePatcher.Uxl.Base
    // open AssetPatch.TemplatePatcher.Uxl.EmitNewAttributes
    open AssetPatch.TemplatePatcher.Uxl.PatchCompiler

    type UxlOptions = 
        { ProcessRequester: string
          ChangeRequestDescription: string
          FilePrefix: string
          OutputDirectory : string
        }

    