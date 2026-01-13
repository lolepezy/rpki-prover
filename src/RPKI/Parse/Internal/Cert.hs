{-# LANGUAGE OverloadedStrings #-}

module RPKI.Parse.Internal.Cert where

import Control.Monad

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Base64     as B64

import           Data.Foldable
import           Data.String.Interpolate.IsString

import           Data.ASN1.BitArray
import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Types

import           Data.X509

import           RPKI.AppMonad
import           RPKI.Reporting
import           RPKI.Domain
import           RPKI.Resources.Resources
import qualified RPKI.Util                  as U

import           RPKI.Parse.Internal.Common


{- |
  Parse RPKI certificate object with the IP and ASN resource extensions.
-}
parseResourceCertificate :: BS.ByteString 
                        -> PureValidatorT (RawResourceCertificate, CertType, SKI, Maybe AKI, Hash)
parseResourceCertificate bs = do
    cert <- mapParseErr $ decodeSignedObject bs      
    let z = unifyCert cert
    (rc, ski_, aki_) <- toResourceCert z
    certType <- getCertificateType $ getExtsSign z
    pure (rc, certType, ski_, aki_, U.sha256s bs)


toResourceCert :: CertificateWithSignature 
                -> PureValidatorT (RawResourceCertificate, SKI, Maybe AKI)
toResourceCert cert = do  
    let exts = getExtsSign cert
    case extVal exts id_subjectKeyId of 
        Just s -> do
            rc <- parseResources cert    
            ki <- parseKI s            
            fmap (rc, SKI ki, ) $!
                case extVal exts id_authorityKeyId of
                    Nothing -> pure Nothing
                    Just a  -> Just . AKI <$> parseKI a
        Nothing -> 
            pureError $ parseErr "No SKI extension"


parseResources :: CertificateWithSignature -> PureValidatorT RawResourceCertificate
parseResources x509cert = do    
    let ext' = extVal $ getExtsSign x509cert        
    ips'  <- maybe (pure emptyIpResources) (parseR parseIpExt) $ ext' id_pe_ipAddrBlocks
    asns' <- maybe (pure emptyAsResources) (parseR parseAsnExt) $ ext' id_pe_autonomousSysIds
    pure $ RawResourceCertificate x509cert $ allResources ips' asns'
  where             
    parseR f bs = 
        case decodeASN1' BER bs of 
            Left e     -> pureError $ parseErr $ "Couldn't parse IP address extension: " <> U.fmtGen e
            Right asns -> f asns                  

-- | https://tools.ietf.org/html/rfc5280#page-16
--
getSubjectPublicKeyInfo :: Certificate -> SPKI
getSubjectPublicKeyInfo cert = SPKI $ EncodedBase64 $ B64.encodeBase64' $ 
  encodeASN1' DER $ (toASN1 $ certPubKey cert) []

getCertificateType :: [ExtensionRaw] -> PureValidatorT CertType
getCertificateType extensions =
    withCriticalExtension extensions id_ce_keyUsage $ \bs parsed ->             
        case parsed of 
            -- Bits position are from
            -- https://datatracker.ietf.org/doc/html/rfc5280#section-4.2.1.3
            -- 
            -- Which bits needs to set and where
            -- https://datatracker.ietf.org/doc/html/rfc6487#section-4.8.4
            [BitString ba@(BitArray 7 _)] -> do 
                unless (bitArrayGetBit ba 5) $ vPureError $ BrokenKeyUsage "keyCertSign bit is not set"
                unless (bitArrayGetBit ba 6) $ vPureError $ BrokenKeyUsage "cRLSign bit is not set"
                for_ [0..4] $ \bit -> 
                    when (bitArrayGetBit ba bit) $ vPureError $ BrokenKeyUsage 
                        [i|Bit #{bit} is set, only keyCertSign and cRLSign must be set.|]

                pure CACert            

            -- There must be only the `digitalSignature` bit 
            [BitString ba@(BitArray 1 _)] ->                     
                let badExtKU = vPureError $ CertBrokenExtension id_ce_extKeyUsage bs
                in case extVal extensions id_ce_extKeyUsage of
                    Nothing -> do                             
                        unless (bitArrayGetBit ba 0) $ vPureError $ BrokenKeyUsage "digitalSignature bit is not set"
                        pure EECert
                    Just bs1 
                        | BS.null bs1 -> badExtKU
                        | otherwise ->
                            case decodeASN1 DER (LBS.fromStrict bs1) of
                                Left _  -> badExtKU
                                Right [Start Sequence, OID oid, End Sequence]
                                    | oid == id_kp_bgpsecRouter -> pure BGPCert
                                    | otherwise -> badExtKU                                            
                                _ -> badExtKU
                
            _ -> vPureError $ UnknownCriticalCertificateExtension id_ce_keyUsage bs


withCriticalExtension :: [ExtensionRaw]
                    -> OID
                    -> (BS.ByteString -> [ASN1] -> PureValidatorT r)
                    -> PureValidatorT r
withCriticalExtension extensions oid f = do 
    case extVal extensions oid of
        Nothing -> vPureError $ MissingCriticalExtension oid
        Just bs 
            | BS.null bs -> vPureError $ MissingCriticalExtension oid
            | otherwise -> do 
                case decodeASN1 DER (LBS.fromStrict bs) of
                    Left _  -> vPureError $ CertBrokenExtension oid bs        
                    Right z -> f bs z            
