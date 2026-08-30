{-# LANGUAGE OverloadedStrings #-}

module RPKI.Meta.UniqueId where

import RPKI.AppTypes
import RPKI.Meta.Version

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> " " <>
    -- The content is to be updated by the 'src-hash' script
    -- that calculates hash of the source tree and configuration/build files
    "srcHash#dce8e487a00c393f1d2cc458421677c899f1ebd564b748fcead7d9476372ce0a#srcHash"
