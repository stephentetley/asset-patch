// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Uxl


module Base =
    
    open AssetPatch.Base.Common
    open AssetPatch.TemplatePatcher.Base
    open AssetPatch.TemplatePatcher.Base.CompilerMonad

    type UxlEnv = 
        { ProcessRequester: string
          ChangeRequestDescription: string 
          ChangeRequestType: string }

    let defaultUxlEnv (changeRequestDescription: string ) = 
        { ProcessRequester = "ASSET DATA"
          ChangeRequestDescription = changeRequestDescription
          ChangeRequestType = "AIWEAM0P" 
        }

    type UxlCompilerMonad<'a> = CompilerMonad<'a, UxlEnv>

    


    let getProcessRequester () : UxlCompilerMonad<string> = 
        asksUserEnv (fun env -> env.ProcessRequester)

    let getChangeRequestDescription () : UxlCompilerMonad<string> = 
        asksUserEnv (fun env -> env.ChangeRequestDescription)

    let getChangeRequestType () : UxlCompilerMonad<string> = 
        asksUserEnv (fun env -> env.ChangeRequestType)

    let newEquimentId () : UxlCompilerMonad<string> = 
        freshName <| fun i -> sprintf "$%i" (i + 2000)


    type ChangeRequestProperties = 
        { Description: string
          ChangeType: string
          Requester: string
        }


    type UxlGenerate<'a> = GenerateMonad.GenerateMonad<'a, UxlEnv>
    
    
    let askChangeRequestProperties() : UxlGenerate<ChangeRequestProperties> = 
        GenerateMonad.asksUserEnv <| fun env -> 
                        { Description = env.ChangeRequestDescription
                          ChangeType = env.ChangeRequestType
                          Requester = env.ProcessRequester
                        }
