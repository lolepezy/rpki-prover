module RPKI.Version where

import Data.Text (Text)
import Data.Version

import RPKI.Util (convert)

import qualified Paths_rpki_prover as Autogen

rpkiProverVersion :: Text
rpkiProverVersion = convert $ "rpki-prover-" <> showVersion Autogen.version

-- The content between tags is to be updated by the 'src-hash' script 
-- that calculates hash of the source tree. 
srcHash :: Text
srcHash = convert "srcHash#504c69f82654a62fb9795664ce6410d68686ef2568ec1b4ae4ba184df888ac09#srcHash"

uniqueVersion :: Text
uniqueVersion = rpkiProverVersion <> srcHash
