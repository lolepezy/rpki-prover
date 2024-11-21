module RPKI.Version where

import Data.Text (Text)
import Data.Version

import RPKI.AppTypes
import RPKI.Util (convert)

import qualified Paths_rpki_prover as Autogen

rpkiProverVersion :: Text
rpkiProverVersion = convert $ "rpki-prover-" <> showVersion Autogen.version

-- The content between tags is to be updated by the 'src-hash' script 
-- that calculates hash of the source tree and configuration/build files 
srcHash :: Text
srcHash = convert "srcHash#6c3b6c1f41697bf8e0e3b7c0ae6e4e258cd14d62752aaca559f1a9b27f79a77d#srcHash"

makeExecutableVersion :: ExecutableVersion
makeExecutableVersion = ExecutableVersion $ rpkiProverVersion <> convert " " <> srcHash
