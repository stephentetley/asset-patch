// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Uxl


module Base =
    
    open AssetPatch.Base.Common
    open AssetPatch.TemplatePatcher.Base.CompilerMonad

    type UxlCompilerMonad<'a> = CompilerMonad<'a, unit>

    let runUxlCompiler (options : CompilerOptions) 
                        (action : UxlCompilerMonad<'a> ) : Result<'a, ErrMsg> = 
        runCompiler options () action

