module RPKI.UniqueId where

import Data.Text (Text)

import RPKI.AppTypes
import RPKI.Version
import RPKI.Util (convert)

-- The content between tags is to be updated by the 'src-hash' script 
-- that calculates hash of the source tree and configuration/build files 
srcHash :: Text
srcHash = convert "srcHash#7f1554f8413965101207ae9563bc7a29bfa89daec2fad993ec5c92320985ab40#srcHash"

thisExecutableVersion :: ExecutableVersion
thisExecutableVersion = ExecutableVersion $ rpkiProverVersion <> convert " " <> srcHash
