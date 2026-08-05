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
import GitHash

showVersion_ :: String
showVersion_ = showVersion Autogen.version

rpkiProverVersionNumber :: Text
rpkiProverVersionNumber = Text.pack showVersion_

rpkiProverVersion :: Text
rpkiProverVersion = [i|rpki-prover-#{showVersion_}|]

makeGitInfo :: Text
makeGitInfo =
    case $$tGitInfoCwdTry of
        Left  _  -> Text.pack "git info unavailable"
        Right gi -> [i|#{giBranch gi}@#{giHash gi}, at #{giCommitDate gi}, #{giCommitCount gi} commits in HEAD#{dirty gi}|]
  where
    dirty gi | giDirty gi = ", (uncommitted files present)"
             | otherwise  = ""
