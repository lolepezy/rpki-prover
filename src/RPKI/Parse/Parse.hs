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

readObject :: String -> B.ByteString -> ParseResult RpkiObject
readObject name content = do    
  let ext = L.drop (L.length name - 3) name
  let uri = URI $ T.pack name
  case ext of
        "cer" -> do
            f <- parseResourceCertificate content
            let (meta, o) = f uri
            pure $ RpkiObject meta $ CerRO o

        "mft" -> do
            f <- parseMft content             
            let (meta, o) = f uri
            pure $ RpkiObject meta $ MftRO o

        "roa" -> do
            f <- parseRoa content
            let (meta, o) = f uri
            pure $ RpkiObject meta $ RoaRO o

        "crl" -> do
            f <- parseCrl content
            let (meta, o) = f uri
            pure $ RpkiCrl meta o

        _     -> Left $ fmtErr $ "Unknown object type: " <> show name

