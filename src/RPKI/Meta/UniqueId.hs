module RPKI.Meta.UniqueId where

import RPKI.AppTypes
import RPKI.Meta.Version

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> " " <> 
    -- The content is to be updated by the 'src-hash' script 
    -- that calculates hash of the source tree and configuration/build files     
    "srcHash#60efffebc8e87e2d0d8846a86f8d0d7729c5f8283026888a0482c8ccba272c8c#srcHash"        
