{-# LANGUAGE OverloadedStrings #-}

module RPKI.UniqueId where

import RPKI.AppTypes
import RPKI.Version

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> " " <> 
    -- The content is to be updated by the 'src-hash' script 
    -- that calculates hash of the source tree and configuration/build files     
    "srcHash#a62c32c2d7f796222bc9f3811358a45a0fb1b4f5ba6e85c8f6c1a7e7c624595c#srcHash"
    
