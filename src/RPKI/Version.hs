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
srcHash = convert "srcHash#bc12b0ec0ca0ea2393f32313feccb55ddda2b4a76358646d0ae8a1206631f137#srcHash"

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> convert " " <> srcHash
