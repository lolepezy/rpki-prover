{-# LANGUAGE OverloadedStrings #-}

module RPKI.Meta.UniqueId where

import RPKI.AppTypes
import RPKI.Meta.Version

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> " " <> 
    -- The content is to be updated by the 'src-hash' script 
    -- that calculates hash of the source tree and configuration/build files     
    "srcHash#3ea8ddc7966a8136aa3213468a844882c8e327eb9ce818aab5a7e8dae8c22006#srcHash"        
