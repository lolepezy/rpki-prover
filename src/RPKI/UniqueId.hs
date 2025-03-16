{-# LANGUAGE OverloadedStrings    #-}
module RPKI.UniqueId where

import RPKI.AppTypes
import RPKI.Version

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> " " <> 
    -- The content is to be updated by the 'src-hash' script 
    -- that calculates hash of the source tree and configuration/build files     
    "srcHash#90466c8061e3ba36ba1f54a57b23f2107b06d5a53d13d961bfa77b1dd8b1a51c#srcHash"
