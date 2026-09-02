{-# LANGUAGE OverloadedStrings #-}

module RPKI.Meta.UniqueId where

import RPKI.AppTypes
import RPKI.Meta.Version

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> " " <>
    -- The content is to be updated by the 'src-hash' script
    -- that calculates hash of the source tree and configuration/build files
    "srcHash#01bd63a30ec9602e949973d856fbe34fea0cb12241384cb62e28e96913af91e8#srcHash"
