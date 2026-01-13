{-# LANGUAGE OverloadedStrings #-}

module RPKI.UniqueId where

import RPKI.AppTypes
import RPKI.Version

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> " " <> 
    -- The content is to be updated by the 'src-hash' script 
    -- that calculates hash of the source tree and configuration/build files     
    "srcHash#1110438f63f0dc92b4d49d10cde94e74d749fe830c256fad6fb2da37572ea843#srcHash"
