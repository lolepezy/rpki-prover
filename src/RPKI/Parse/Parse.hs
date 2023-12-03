{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module RPKI.Parse.Parse (
    module RPKI.Parse.Internal.Common,
    module RPKI.Parse.Internal.Cert,
    module RPKI.Parse.Internal.CRL,
    module RPKI.Parse.Internal.MFT,
    module RPKI.Parse.Internal.ROA,
    module RPKI.Parse.Internal.SignedObject,
    module RPKI.Parse.Internal.GBR,
    module RPKI.Parse.Internal.RSC,
    readObject,
    supportedExtension,
    isSupportedExtension
)
where

import qualified Data.ByteString                  as BS
import           Data.Char                        (toLower)
import qualified Data.List                        as List
import qualified Data.Text                        as Text
import           Data.String                      (IsString)

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Parse.Internal.Cert
import           RPKI.Parse.Internal.Common
import           RPKI.Parse.Internal.CRL
import           RPKI.Parse.Internal.MFT
import           RPKI.Parse.Internal.ROA
import           RPKI.Parse.Internal.GBR
import           RPKI.Parse.Internal.RSC
import           RPKI.Parse.Internal.Aspa
import           RPKI.Parse.Internal.SignedObject

import           RPKI.Util (fmtGen)

-- | 
supportedExtension :: String -> Bool
supportedExtension filename = 
    case map toLower $ List.drop (List.length filename - 4) filename of 
        dot : ext -> dot == '.' && isSupportedExtension ext
        _         -> False

isSupportedExtension :: (Eq a, IsString a) => a -> Bool
isSupportedExtension s = s `elem` 
    ["cer", "mft", "crl", "roa", "gbr", "sig", "asa"]

-- | Parse object from a bytesting containing ASN1 representaton
-- | Decide which parser to use based on the object's filename
readObject :: RpkiURL -> BS.ByteString -> PureValidatorT RpkiObject
readObject objectURL content = do 
    let URI u = getURL objectURL
    let ext = map toLower $ Text.unpack $ Text.drop (Text.length u - 4) u
    case ext of
        ".cer" -> do 
            (rc, certType, rfc, ski, aki, hash) <- parseResourceCertificate content
            case certType of 
                CACert -> do 
                    let certificate = TypedCert $ ResourceCertificate $ mkPolyRFC rfc rc
                    pure $ CerRO $ CaCerObject {..}
                BGPCert -> do 
                    let certificate = TypedCert rc
                    pure $ BgpRO $ BgpCerObject {..}
                EECert -> 
                    pureError $ parseErr "Cannot have EE certificate as a separate object."

        ".mft" -> parse_ parseMft MftRO content
        ".roa" -> parse_ parseRoa RoaRO content                    
        ".crl" -> parse_ parseCrl CrlRO content            
        ".gbr" -> parse_ parseGbr GbrRO content            
        ".sig" -> parse_ parseRsc RscRO content            
        ".asa" -> parse_ parseAspa AspaRO content            
        _      -> pureError $ parseErr $ "Unknown object type: " <> fmtGen u
        where
            parse_ parse constructor bs = 
                constructor <$> parse bs
                