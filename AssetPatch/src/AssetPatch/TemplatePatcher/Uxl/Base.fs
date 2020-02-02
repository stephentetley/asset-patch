// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Uxl


module Base =
    

    open AssetPatch.TemplatePatcher.Base.CompilerMonad

    type UxlCompilerMonad<'a> = CompilerMonad<'a, unit>


