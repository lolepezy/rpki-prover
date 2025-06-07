{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
module RPKI.Version where

import Data.Text (Text)
import Data.Version
import Data.String.Interpolate.IsString

import qualified Paths_rpki_prover as Autogen
import GitHash

rpkiProverVersion :: Text
rpkiProverVersion = [i|rpki-prover-#{showVersion Autogen.version}|]

getGitInfo :: Text
getGitInfo = [i|#{giBranch gi}@#{giHash gi}, at #{giCommitDate gi}, #{giCommitCount gi} commits in HEAD, #{dirty}|]
  where
    dirty | giDirty gi = "(uncommitted files present)"
          | otherwise  = ""
    gi = $$tGitInfoCwd
