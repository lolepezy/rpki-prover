{-# LANGUAGE OverloadedStrings #-}

module RPKI.Meta.UniqueId where

import RPKI.AppTypes
import RPKI.Meta.Version

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> " " <>
    -- The content is to be updated by the 'src-hash' script
    -- that calculates hash of the source tree and configuration/build files
    "srcHash#3f24286144d810a28cea2137aef34eb20ef6817ef79774460602fafe11174421#srcHash"
