{-# LANGUAGE OverloadedStrings    #-}
module RPKI.UniqueId where

import RPKI.AppTypes
import RPKI.Version

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> " " <> 
    -- The content is to be updated by the 'src-hash' script 
    -- that calculates hash of the source tree and configuration/build files     
    "srcHash#edf4cc16d6f4e0d525a0d92797738a3ffe16b7f5ee03334f9f6abe4f34ab0d2a#srcHash"
