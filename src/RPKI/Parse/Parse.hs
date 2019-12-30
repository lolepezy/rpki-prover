module RPKI.Parse.Parse (
    module RPKI.Parse.Internal.Common,
    module RPKI.Parse.Internal.Cert,
    module RPKI.Parse.Internal.CRL,
    module RPKI.Parse.Internal.MFT,
    module RPKI.Parse.Internal.ROA,
    module RPKI.Parse.Internal.SignedObject,
    readObject,
    supportedExtension
)
where

import RPKI.Parse.Internal.Common
import RPKI.Parse.Internal.Cert
import RPKI.Parse.Internal.CRL
import RPKI.Parse.Internal.MFT
import RPKI.Parse.Internal.ROA
import RPKI.Parse.Internal.SignedObject

import RPKI.Domain

import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.Text as T

supportedExtension :: String -> Bool
supportedExtension filename = 
    let ext = L.drop (L.length filename - 4) filename
        in elem ext [".cer", ".mft", ".crl", ".roa", ".gbr"] 

-- | Parse object from a bytesting containing ASN1 representaton
-- | Decide which parser to use based on the object's filename
readObject :: String -> B.ByteString -> ParseResult RpkiObject
readObject name content = do    
  let ext = L.drop (L.length name - 3) name
  let u = URI $ T.pack name
  case ext of
    "cer" -> parse_ u parseResourceCertificate CerRO content            
    "mft" -> parse_ u parseMft MftRO content
    "roa" -> parse_ u parseRoa RoaRO content            
    "crl" -> parse_ u parseCrl CrlRO content            
    _     -> Left $ fmtErr $ "Unknown object type: " <> show name
    where
        parse_ u parse constructor bs = do
            f <- parse bs
            pure $ constructor (f u)

