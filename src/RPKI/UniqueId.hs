{-# LANGUAGE OverloadedStrings    #-}
module RPKI.UniqueId where

import RPKI.AppTypes
import RPKI.Version

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> " " <> 
    -- The content is to be updated by the 'src-hash' script 
    -- that calculates hash of the source tree and configuration/build files     
    "srcHash#c2120bb2d91d9e1ebbf0023d166cc6167dc40d32a7f3200693b31e799d1cac5d#srcHash"