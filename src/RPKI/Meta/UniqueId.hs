module RPKI.Meta.UniqueId where

import qualified Data.Text as Text

import RPKI.AppTypes
import RPKI.Meta.BuildInfo
import RPKI.Meta.Version

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> " " <> Text.pack $(sourceTreeHash)
-- Keeps the hash out of the interface, so that only this module recompiles
-- when it changes, not everything that imports it.
{-# NOINLINE thisExecutableVersion #-}
