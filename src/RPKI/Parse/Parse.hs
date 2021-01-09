module RPKI.Parse.Parse (
    module RPKI.Parse.Internal.Common,
    module RPKI.Parse.Internal.Cert,
    module RPKI.Parse.Internal.CRL,
    module RPKI.Parse.Internal.MFT,
    module RPKI.Parse.Internal.ROA,
    module RPKI.Parse.Internal.SignedObject,
    module RPKI.Parse.Internal.GBR,
    readObject,
    supportedExtension
)
where

import qualified Data.ByteString                  as BS
import           Data.Char                        (toLower)
import qualified Data.List                        as List
import qualified Data.Text                        as Text

import           RPKI.Parse.Internal.Cert
import           RPKI.Parse.Internal.Common
import           RPKI.Parse.Internal.CRL
import           RPKI.Parse.Internal.MFT
import           RPKI.Parse.Internal.ROA
import           RPKI.Parse.Internal.GBR
import           RPKI.Parse.Internal.SignedObject

import           RPKI.Domain

-- | 
supportedExtension :: String -> Bool
supportedExtension filename = 
    let ext = map toLower $ List.drop (List.length filename - 4) filename
        in elem ext [".cer", ".mft", ".crl", ".roa", ".gbr"] 

-- | Parse object from a bytesting containing ASN1 representaton
-- | Decide which parser to use based on the object's filename
readObject :: RpkiURL -> BS.ByteString -> ParseResult RpkiObject
readObject objectURL content = do    
    let URI u = getURL objectURL
    let ext = map toLower $ Text.unpack $ Text.drop (Text.length u - 4) u
    case ext of
        ".cer" -> parse_ objectURL parseResourceCertificate CerRO content            
        ".mft" -> parse_ objectURL parseMft MftRO content
        ".roa" -> parse_ objectURL parseRoa RoaRO content                    
        ".crl" -> parse_ objectURL parseCrl CrlRO content            
        ".gbr" -> parse_ objectURL parseGbr GbrRO content            
        _      -> Left $ fmtErr $ "Unknown object type: " <> show u
        where
            parse_ u parse constructor bs = do
                f <- parse bs
                pure $ constructor (f u)