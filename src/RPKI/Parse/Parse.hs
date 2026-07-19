{-# LANGUAGE OverloadedStrings #-}

module RPKI.Parse.Parse (
    module RPKI.Parse.Internal.Common,
    module RPKI.Parse.Internal.Cert,
    module RPKI.Parse.Internal.CRL,
    module RPKI.Parse.Internal.MFT,
    module RPKI.Parse.Internal.ROA,
    module RPKI.Parse.Internal.SignedObject,
    module RPKI.Parse.Internal.GBR,
    module RPKI.Parse.Internal.RSC,
    module RPKI.Parse.Internal.Erik,
    readObject,
    readObjectOfType,
    supportedExtension,
    supportedExtensionByErik,
    isSupportedExtension,    
    rpkiObjectType,
    urlObjectType,
    textObjectType,
    nameObjectType,
    isOfType
)
where

import qualified Data.ByteString                  as BS
import           Data.Char                        (toLower)
import qualified Data.Text                        as Text
import           Data.String                      (IsString)
import qualified Data.List.Split                  as Split
import           Data.Maybe

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Parse.Internal.Cert
import           RPKI.Parse.Internal.Common
import           RPKI.Parse.Internal.CRL
import           RPKI.Parse.Internal.MFT
import           RPKI.Parse.Internal.ROA
import           RPKI.Parse.Internal.SPL
import           RPKI.Parse.Internal.GBR
import           RPKI.Parse.Internal.RSC
import           RPKI.Parse.Internal.Erik
import           RPKI.Parse.Internal.Aspa
import           RPKI.Parse.Internal.SignedObject

import           RPKI.Util (fmtGen)


rpkiObjectType :: (Eq s, IsString s) => s -> Maybe RpkiObjectType
rpkiObjectType = \case
    "cer" -> Just CER
    "mft" -> Just MFT 
    "crl" -> Just CRL 
    "roa" -> Just ROA 
    "gbr" -> Just GBR
    "sig" -> Just RSC 
    "asa" -> Just ASPA
    "spl" -> Just SPL
    _      -> Nothing

-- | 
supportedExtension :: String -> Bool
supportedExtension filename = 
    supportedExtensionGen filename isSupportedExtension

supportedExtensionByErik :: String -> Bool
supportedExtensionByErik filename = 
    supportedExtensionGen filename $ \ext -> 
        case rpkiObjectType ext of     
            Nothing   -> False
            -- For now Erik relays don't retransmit GBR objects
            Just GBR  -> False    
            Just _    -> True

supportedExtensionGen :: String -> (String -> Bool) -> Bool
supportedExtensionGen filename f =
    case map toLower $ take 4 $ reverse filename of 
        [c3, c2, c1, dot] -> dot == '.' && f [c1, c2, c3]
        _                 -> False


isSupportedExtension :: (Eq a, IsString a) => a -> Bool
isSupportedExtension = isJust . rpkiObjectType
        
urlObjectType :: RpkiURL -> Maybe RpkiObjectType
urlObjectType (getURL -> URI u) = textObjectType u

textObjectType :: Text.Text -> Maybe RpkiObjectType
textObjectType t = deriveObjectType $ Text.split (== '.') t        

nameObjectType :: String -> Maybe RpkiObjectType
nameObjectType s = deriveObjectType $ Split.splitOn "." s

deriveObjectType :: (Eq s, IsString s) => [s] -> Maybe RpkiObjectType
deriveObjectType = \case 
    _ : x@(_ : _) -> rpkiObjectType $ last x
    _             -> Nothing

isOfType :: RpkiObjectType -> RpkiObjectType -> Bool
isOfType t1 t2 = t1 == t2 || t1 == BGPSec && t2 == CER

-- | Parse object from a bytesting containing ASN1 representaton
-- | Decide which parser to use based on the object's filename
readObject :: RpkiURL -> BS.ByteString -> PureValidatorT RpkiObject
readObject objectURL bs =     
    case urlObjectType objectURL of 
        Just type_ -> readObjectOfType type_ bs
        Nothing    -> pureError $ parseErr $ "Could not figure out object type from URL: " <> fmtGen objectURL


readObjectOfType :: RpkiObjectType -> BS.ByteString -> PureValidatorT RpkiObject        
readObjectOfType objectType bs = 
    case objectType of 
        CER -> do 
            (rc, certType, ski, aki, hash) <- parseResourceCertificate bs
            case certType of 
                CACert -> do 
                    let certificate = TypedCert $ ResourceCertificate rc
                    pure $ CerRO $ CaCerObject {..}
                BGPCert -> do 
                    let certificate = TypedCert rc
                    pure $ BgpRO $ BgpCerObject {..}
                EECert -> 
                    pureError $ parseErr "Cannot have EE certificate as a separate object."
        MFT  -> MftRO <$> parseMft bs
        ROA  -> RoaRO <$> parseRoa bs
        SPL  -> SplRO <$> parseSpl bs
        CRL  -> CrlRO <$> parseCrl bs            
        GBR  -> GbrRO <$> parseGbr bs            
        RSC  -> RscRO <$> parseRsc bs
        ASPA -> AspaRO <$> parseAspa bs     
        t    -> pureError $ parseErr $ "Parsing of type " <> fmtGen t <> " is not supported"  
