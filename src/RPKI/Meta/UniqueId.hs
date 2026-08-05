{-# LANGUAGE OverloadedStrings #-}

module RPKI.Meta.UniqueId where

import RPKI.AppTypes
import RPKI.Version

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> " " <> 
    -- The content is to be updated by the 'src-hash' script 
    -- that calculates hash of the source tree and configuration/build files     
    "srcHash#733f1bc1d2f1730c43a4a7e937dbe9d9450f7469afdc82f8b3fe0ca8dd3bf646#srcHash"
    
