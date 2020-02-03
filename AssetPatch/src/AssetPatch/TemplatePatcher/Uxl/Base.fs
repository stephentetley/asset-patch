// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Uxl


module Base =
    
    open AssetPatch.Base.Common
    open AssetPatch.TemplatePatcher.Base.CompilerMonad

    type UxlEnv = 
        { ProcessRequestor: string }


    type UxlCompilerMonad<'a> = CompilerMonad<'a, UxlEnv>

    let getProcessRequestor () : UxlCompilerMonad<string> = 
        asksUserEnv (fun env -> env.ProcessRequestor)

