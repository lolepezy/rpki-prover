module RPKI.Version (
    rpkiProverVersionNumber, 
    rpkiProverVersion, 
    makeGitInfo
) 
where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Version
import Data.String.Interpolate.IsString

import qualified Paths_rpki_prover as Autogen
import qualified RPKI.Meta.GitVersionInfo as Git

showVersion_ :: String
showVersion_ = showVersion Autogen.version

rpkiProverVersionNumber :: Text
rpkiProverVersionNumber = Text.pack showVersion_

rpkiProverVersion :: Text
rpkiProverVersion = [i|rpki-prover-#{showVersion_}|]

makeGitInfo :: Text
makeGitInfo = [i|#{Git.gitBranch}@#{Git.gitHash}, at #{Git.gitCommitDate}, #{Git.gitCommitCount} commits in HEAD#{dirty}|]
  where
    dirty | Git.gitDirty = ", (uncommitted files present)"
          | otherwise    = "" :: String
