module RPKI.Version where

import Data.Text (Text)
import Data.Version

import RPKI.Util (convert)

import qualified Paths_rpki_prover as Autogen

rpkiProverVersion :: Text
rpkiProverVersion = convert $ "rpki-prover-" <> showVersion Autogen.version
