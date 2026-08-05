{-# LANGUAGE OverloadedStrings #-}

module RPKI.Meta.UniqueId where

import RPKI.AppTypes
import RPKI.Version

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> " " <> 
    -- The content is to be updated by the 'src-hash' script 
    -- that calculates hash of the source tree and configuration/build files     
    "srcHash#5d9015ce3976c4b3d1ced43f61c02934dfb9f052ef9d63ab2a71cfcf352fa3af#srcHash"
    
