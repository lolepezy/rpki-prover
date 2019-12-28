{-# LANGUAGE RecordWildCards #-}
module RPKI.Validation.Objects where

import           Control.Monad
import           Control.Monad.Reader

import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           Data.Maybe

import           Data.Data                (Typeable)
import           Data.Has

import           GHC.Generics

import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Types
import           Data.X509

import           Data.Hourglass           (DateTime)
import           Data.X509.Validation     hiding (InvalidSignature)
import           System.Hourglass         (dateCurrent)

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Parse.Parse
import           RPKI.TAL
import           RPKI.Util                (convert)
import           RPKI.Validation.Crypto


-- | Current time that is to be passed into the environment of validating functions
newtype Now = Now DateTime
  deriving (Show, Eq, Ord)

newtype Validated a = Validated a
  deriving (Show, Eq, Typeable, Generic)


now :: IO Now 
now = Now <$> dateCurrent

validateResourceCertExtensions :: ResourceCertificate -> PureValidator conf ResourceCertificate
validateResourceCertExtensions rc@(ResourceCertificate cert) = 
  validateHasCriticalExtensions $ cwsX509certificate $ withRFC cert certX509     
  where
    validateHasCriticalExtensions x509cert = do
      let exts = getExts x509cert
      case extVal exts id_ce_certificatePolicies of
        Nothing -> pureError CertNoPolicyExtension
        Just bs 
          | B.null bs -> pureError CertNoPolicyExtension
          | otherwise ->
            case decodeASN1 DER (BL.fromStrict bs) of
                Right [
                    Start Sequence,
                    Start Sequence,
                    OID oid,
                    End Sequence,
                    End Sequence
                  ] | oid == id_cp_ipAddr_asNumber -> pure rc
                _ -> pureError $ CertWrongPolicyExtension bs
                                
-- | 
validateTACert :: TAL -> URI -> RpkiObject -> PureValidator conf CerObject
validateTACert tal u (CerRO cert@(_, rc@(ResourceCertificate taCert))) = do
    let spki = subjectPublicKeyInfo $ cwsX509certificate $ withRFC taCert certX509
    pureErrorIfNot (publicKeyInfo tal == spki) $ SPKIMismatch (publicKeyInfo tal) spki    
    pureErrorIfNot (isNothing $ aki $ getMeta cert) $ TACertAKIIsNotEmpty u
    -- It's self-signed, so use itself as a parent to check the signature
    signatureCheck $ validateCertSignature rc rc
    validateResourceCertExtensions rc
    pure cert

validateTACert _ _ _ = pureError UnknownObjectAsTACert


{- |
    In general, resource certifcate validation is:

    - check the signature (with the parent)
    - check if it's revoked
    - check all the needed extensions and expiration times
    - check the resource set (needs the parent as well)
    - check it's not revoked (needs CRL)
 -}
validateResourceCert :: ResourceCertificate -> 
                        ResourceCertificate -> 
                        Validated CrlObject ->
                        PureValidator conf (Validated ResourceCertificate)
validateResourceCert cert parentCert vcrl = do
  signatureCheck $ validateCertSignature cert parentCert
  when (isRevoked cert vcrl) $ 
    pureError RevokedResourceCertificate
  validateResourceCertExtensions cert
  validateResourceSet cert parentCert
  pure $ Validated cert
  where 
    -- TODO Implement it
    validateResourceSet cert parentCert = pure ()
    


-- | Validate CRL object with the parent certificate
validateCrl :: Has Now conf => 
              CrlObject -> ResourceCertificate -> PureValidator conf (Validated CrlObject)
validateCrl x@(_, s@SignCRL {..}) parentCert = do
  signatureCheck $ validateCRLSignature s parentCert
  void $ validateNexUpdate (crlNextUpdate crl)
  pure $ Validated x


-- TODO Validate other stuff, validate resource certificate, etc.
validateMft :: Has Now conf => 
              MftObject -> ResourceCertificate -> Validated CrlObject -> 
              PureValidator conf (Validated MftObject)
validateMft mft@(_, cms) parentCert crl = do
  validateCms cms parentCert crl $ \m -> do
      void $ validateNexUpdate $ Just $ nextTime $ getCMSContent m
      pure m
  pure $ Validated mft

validateRoa :: Has Now conf => 
              RoaObject -> ResourceCertificate -> Validated CrlObject -> 
              PureValidator conf (Validated RoaObject)
validateRoa roa@(_, cms) parentCert crl = do
  validateCms cms parentCert crl pure
  pure $ Validated roa

validateGbr :: Has Now conf => 
              GbrObject -> ResourceCertificate -> Validated CrlObject -> 
              PureValidator conf (Validated GbrObject)
validateGbr gbr@(_, cms) parentCert crl = do
  validateCms cms parentCert crl pure
  pure $ Validated gbr

validateCms :: Has Now conf => 
               CMS a -> 
               ResourceCertificate -> 
               Validated CrlObject -> 
               (CMS a -> PureValidator conf (CMS a)) ->
               PureValidator conf (Validated (CMS a))
validateCms cms parentCert crl extraValidation = do
  signatureCheck $ validateCMSSignature cms
  let eeCert = getEEResourceCert $ unCMS cms  
  validateResourceCert eeCert parentCert crl
  when (isRevoked eeCert crl) $ 
    pureError RevokedEECertificate
  extraValidation cms
  pure $ Validated cms


-- | Validate the nextUpdateTime for objects that have it (MFT, CRLs)
validateNexUpdate :: Has Now conf => 
                    Maybe DateTime -> PureValidator conf DateTime
validateNexUpdate nextUpdateTime = do 
    Now now' <- asks getter
    case nextUpdateTime of
      Nothing -> pureError NextUpdateTimeNotSet
      Just nextUpdate 
        | nextUpdate < now' -> pureError $ NextUpdateTimeIsBeforeNow nextUpdate
        | otherwise         -> pure nextUpdate


-- | Check if CMS is on the revocation list
isRevoked :: ResourceCertificate -> Validated CrlObject -> Bool
isRevoked (ResourceCertificate cert) (Validated (_, SignCRL {..})) = 
  not $ null $ filter
    (\(RevokedCertificate {..}) -> Serial revokedSerialNumber == serial) $ 
      crlRevokedCertificates crl
  where
    serial = Serial (certSerial $ cwsX509certificate $ withRFC cert certX509)
    

signatureCheck :: SignatureVerification -> PureValidator conf ()
signatureCheck sv = case sv of
    SignatureFailed e -> pureError $ InvalidSignature $ convert $ show e
    SignaturePass     -> pure ()        
