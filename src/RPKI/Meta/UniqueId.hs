{-# LANGUAGE OverloadedStrings #-}

module RPKI.Meta.UniqueId where

import RPKI.AppTypes
import RPKI.Meta.Version

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> " " <>
    -- The content is to be updated by the 'src-hash' script
    -- that calculates hash of the source tree and configuration/build files
    "srcHash#45329d17cbe712bba8d2af036fe38ecda6c614931a794ea8eb66f3c331855957#srcHash"
