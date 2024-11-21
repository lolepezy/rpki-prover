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
srcHash = convert "srcHash#b5dc998ce47134f0e0c96a891aabce0379b868d41cedebec309f5645f833f4f9#srcHash"

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> convert " " <> srcHash
