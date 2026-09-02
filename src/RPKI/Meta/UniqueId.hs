{-# LANGUAGE OverloadedStrings #-}

module RPKI.Meta.UniqueId where

import RPKI.AppTypes
import RPKI.Meta.Version

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> " " <>
    -- The content is to be updated by the 'src-hash' script
    -- that calculates hash of the source tree and configuration/build files
    "srcHash#63735e5bd7da3afda860b6989ab020943bb02bbb330a817198121a27aa0ae2e3#srcHash"
