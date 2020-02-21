// Copyright (c) Stephen Tetley 2020
// License: BSD 3 Clause

namespace AssetPatch.TemplatePatcher.Uxl


module Base =
    

    open AssetPatch.TemplatePatcher.Base.GenerateMonad

    type UxlEnv = 
        { ProcessRequester: string
          ChangeRequestDescription: string 
          ChangeRequestType: string }

    let defaultUxlEnv (changeRequestDescription: string ) = 
        { ProcessRequester = "ASSET DATA"
          ChangeRequestDescription = changeRequestDescription
          ChangeRequestType = "AIWEAM0P" 
        }



    type ChangeRequestProperties = 
        { Description: string
          ChangeType: string
          Requester: string
        }


    type UxlGenerate<'a> = GenerateMonad<'a, UxlEnv>
    
    
    let askChangeRequestProperties() : UxlGenerate<ChangeRequestProperties> = 
        asksUserEnv <| fun env -> 
                        { Description = env.ChangeRequestDescription
                          ChangeType = env.ChangeRequestType
                          Requester = env.ProcessRequester
                        }
