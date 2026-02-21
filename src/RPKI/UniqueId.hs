{-# LANGUAGE OverloadedStrings #-}

module RPKI.UniqueId where

import RPKI.AppTypes
import RPKI.Version

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> " " <> 
    -- The content is to be updated by the 'src-hash' script 
    -- that calculates hash of the source tree and configuration/build files     
<<<<<<< HEAD
    "srcHash#9b83137f4b6a8d97c1e4991e8e3b7e047f5cd0c88feef14ca6bfe2cb21ebe635#srcHash"
=======
    "srcHash#172f3fca97369e42da55e21ae06f138370d9a671d58dbf159c128c0760236f4f#srcHash"
    
>>>>>>> 85581e81 (Use aki for referring to CAs)
