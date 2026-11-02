module RPKI.Meta.GitVersionInfo where

import RPKI.Meta.BuildInfo

gitInfo :: GitInfo
gitInfo = $(gitInfoOfTheTree)
-- Keeps the values out of the interface, so that only this module recompiles
-- when they change, not everything that imports it.
{-# NOINLINE gitInfo #-}

gitBranch :: String
gitBranch = "rsync-merge"

gitHash :: String
gitHash = "24fc758714e8cd49138acd176c5f8a5a56eedf43"

gitCommitDate :: String
gitCommitDate = "2026-11-01 23:06:56 +0100"

gitCommitCount :: Int
gitCommitCount = 3116

gitDirty :: Bool
gitDirty = gitInfo.dirty
