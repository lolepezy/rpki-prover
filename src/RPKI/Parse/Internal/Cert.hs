{-# LANGUAGE OverloadedStrings #-}

module RPKI.Parse.Internal.Cert where

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base64     as B64

import           Data.Bifunctor

import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Types

import           Data.X509

import           RPKI.Domain
import           RPKI.Resources.Resources   as R
import qualified RPKI.Util                  as U

import           RPKI.Parse.Internal.Common


{- |
  Parse RPKI certificate object with the IP and ASN resource extensions.
-}
parseResourceCertificate :: BS.ByteString ->
                            ParseResult CerObject
parseResourceCertificate bs = do
    cert <- mapParseErr $ decodeSignedObject bs      
    (rc, ski_, aki_) <- toResourceCert $ unifyCert cert
    pure $! newCert aki_ ski_ (U.sha256s bs) rc


toResourceCert :: CertificateWithSignature -> ParseResult (ResourceCertificate, SKI, Maybe AKI)
toResourceCert cert = do  
    let exts = getExtsSign cert
    case extVal exts id_subjectKeyId of 
        Just s -> do
            rc <- parseResources cert    
            ki <- parseKI s
            aki' <- case extVal exts id_authorityKeyId of
                            Nothing -> pure Nothing
                            Just a  -> Just . AKI <$> parseKI a                      
            pure (rc, SKI ki, aki')
        Nothing -> Left $ fmtErr "No SKI extension"


parseResources :: CertificateWithSignature -> ParseResult ResourceCertificate
parseResources x509cert = do    
    let ext' = extVal $ getExtsSign x509cert
    case (ext' id_pe_ipAddrBlocks,
          ext' id_pe_ipAddrBlocks_v2,
          ext' id_pe_autonomousSysIds,
          ext' id_pe_autonomousSysIds_v2) 
      of
        (Just _, Just _, _, _) -> broken "Both IP extensions"
        (_, _, Just _, Just _) -> broken "Both ASN extensions"
        (Just _, _, _, Just _) -> broken "There is both IP V1 and ASN V2 extensions"
        (_, Just _, Just _, _) -> broken "There is both IP V2 and ASN V1 extensions"
        (ips, Nothing, asns, Nothing) -> strictCert <$> cert' x509cert ips asns
        (Nothing, ips, Nothing, asns) -> reconsideredCert <$> cert' x509cert ips asns
  where 
    broken = Left . fmtErr    
    cert' x509c ips asns = do 
        ips'  <- maybe (pure emptyIpResources) (parseR parseIpExt) ips
        asns' <- maybe (pure emptyAsResources) (parseR parseAsnExt) asns
        pure $ ResourceCert x509c $ allResources ips' asns'

    parseR :: ([ASN1] -> ParseResult a) -> BS.ByteString -> ParseResult a
    parseR f bs = f =<< first fmt (decodeASN1' BER bs)
      where 
        fmt err = fmtErr $ "Couldn't parse IP address extension: " <> show err

-- | https://tools.ietf.org/html/rfc5280#page-16
--
subjectPublicKeyInfo :: Certificate -> EncodedBase64
subjectPublicKeyInfo cert = EncodedBase64 $ B64.encodeBase64' $ 
  encodeASN1' DER $ (toASN1 $ certPubKey cert) []

getX509Cert :: SignedExact c -> c
getX509Cert = signedObject . getSigned