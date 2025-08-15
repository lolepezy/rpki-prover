{-# LANGUAGE OverloadedStrings    #-}
module RPKI.UniqueId where

import RPKI.AppTypes
import RPKI.Version

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> " " <> 
    -- The content is to be updated by the 'src-hash' script 
    -- that calculates hash of the source tree and configuration/build files     
    "srcHash#4fdc67a82fb5d20f840e379f01c826016faa937fe095bfd6e1129aada12dbde2#srcHash"