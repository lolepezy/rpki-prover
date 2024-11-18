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
srcHash = convert "srcHash#e8f60d9e4522cd6cf909172ceb985c8575e4fe9168fb70aec506fe751e9ad774#srcHash"

uniqueVersion :: Text
uniqueVersion = rpkiProverVersion <> convert " " <> srcHash
