{-# LANGUAGE OverloadedStrings #-}

module RPKI.UniqueId where

import RPKI.AppTypes
import RPKI.Version

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> " " <> 
    -- The content is to be updated by the 'src-hash' script 
    -- that calculates hash of the source tree and configuration/build files     
    "srcHash#64b39d184d90958a8b9dd0f69710079bcd793c61f48d42652cc5f17961a58fdb#srcHash"
    