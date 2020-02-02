// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause


namespace AssetPatch.OutstationPatcher

module UxlPatcher =

    type UxlOptions = 
        { ProcessRequester : string 
          OutputDirectory : string
          WorkListPath : string
        }