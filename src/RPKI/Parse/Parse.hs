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
    readObjectOfType,
    supportedExtension,
    isSupportedExtension,
    rpkiObjectType,
    urlObjectType,
    textObjectType
)
where

import qualified Data.ByteString                  as BS
import           Data.Char                        (toLower)
import qualified Data.List                        as List
import qualified Data.Text                        as Text
import           Data.String                      (IsString)
import           Data.Maybe

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


rpkiObjectType :: (Eq s, IsString s) => s -> Maybe RpkiObjectType
rpkiObjectType s = 
    case s of 
        "cer" -> Just CER
        "mft" -> Just MFT 
        "crl" -> Just CRL 
        "roa" -> Just ROA 
        "gbr" -> Just GBR
        "sig" -> Just RSC 
        "asa" -> Just ASPA
        _      -> Nothing

-- | 
supportedExtension :: String -> Bool
supportedExtension filename = 
    case map toLower $ List.drop (List.length filename - 4) filename of 
        dot : ext -> dot == '.' && isSupportedExtension ext
        _         -> False

isSupportedExtension :: (Eq a, IsString a) => a -> Bool
isSupportedExtension = isJust . rpkiObjectType

urlObjectType :: RpkiURL -> Maybe RpkiObjectType
urlObjectType (getURL -> URI u) = textObjectType u

textObjectType :: Text.Text -> Maybe RpkiObjectType
textObjectType t = rpkiObjectType $ Text.takeEnd 3 t

-- | Parse object from a bytesting containing ASN1 representatond
-- | Decide which parser to use based on the object's filename
readObject :: RpkiURL -> BS.ByteString -> PureValidatorT RpkiObject
readObject objectURL content =     
    case urlObjectType objectURL of 
        Just type_ -> readObjectOfType type_ content
        Nothing    -> pureError $ parseErr $ "Could not figure out object type from URL: " <> fmtGen objectURL


readObjectOfType :: RpkiObjectType -> BS.ByteString -> PureValidatorT RpkiObject        
readObjectOfType objectType content = 
    case objectType of 
        CER -> do 
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

        MFT  -> parse_ parseMft MftRO content
        ROA  -> parse_ parseRoa RoaRO content
        CRL  -> parse_ parseCrl CrlRO content            
        GBR  -> parse_ parseGbr GbrRO content            
        RSC  -> parse_ parseRsc RscRO content            
        ASPA -> parse_ parseAspa AspaRO content     
        t    -> pureError $ parseErr $ "Parsing of type " <> fmtGen t <> " is not supported"
  where
    parse_ parse constructor bs = 
        constructor <$> parse bs        
