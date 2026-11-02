module RPKI.Meta.GitVersionInfo where

import RPKI.Meta.BuildInfo

gitInfo :: GitInfo
gitInfo = $(gitInfoOfTheTree)
-- Keeps the values out of the interface, so that only this module recompiles
-- when they change, not everything that imports it.
{-# NOINLINE gitInfo #-}

gitBranch :: String
gitBranch = gitInfo.branch

gitHash :: String
gitHash = gitInfo.commitHash

gitCommitDate :: String
gitCommitDate = gitInfo.commitDate

gitCommitCount :: Int
gitCommitCount = gitInfo.commitCount

gitDirty :: Bool
gitDirty = gitInfo.dirty
