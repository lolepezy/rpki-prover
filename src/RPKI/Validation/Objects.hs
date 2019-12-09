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
import           RPKI.SignTypes
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

validateResourceCertExtensions :: CerObject -> PureValidator conf CerObject
validateResourceCertExtensions cert@(ResourceCert rc) = 
  validateHasCriticalExtensions $ cwsX509certificate $ withRFC rc certX509     
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
                  ] | oid == id_cp_ipAddr_asNumber -> pure cert
                _ -> pureError $ CertWrongPolicyExtension bs
                                
-- | 
validateTACert :: TAL -> URI -> RpkiObject -> PureValidator conf (WithMeta CerObject)
validateTACert tal u ro@(RpkiObject (WithMeta m@(RpkiMeta {..}) (CerRO cert@(ResourceCert taCert)))) = do
    let spki = subjectPublicKeyInfo $ cwsX509certificate $ withRFC taCert certX509
    pureErrorIfNot (publicKeyInfo tal == spki) $ SPKIMismatch (publicKeyInfo tal) spki    
    pureErrorIfNot (isNothing aki) $ TACertAKIIsNotEmpty u
    -- It's self-signed, so use itself as a parent to check the signature
    signatureCheck $ validateSignature ro cert
    WithMeta m <$> validateResourceCertExtensions cert

validateTACert _ _ _ = pureError UnknownObjectAsTACert


{- |
    In general, resource certifcate validation is:

    - check the signature (with the parent)
    - check if it's revoked
    - check all the needed extensions and expiration times
    - check the resource set (needs the parent as well)
    - check it's not revoked (needs CRL)
 -}
validateResourceCert :: WithMeta CerObject -> 
                        WithMeta CerObject -> 
                        Validated CrlObject ->
                        PureValidator conf (WithMeta CerObject)
validateResourceCert wcer@(WithMeta RpkiMeta {..} cert) (WithMeta _ parentCert) vcrl = do
  signatureCheck $ validateCertSignature cert parentCert
  when (isRevoked wcer vcrl) $ 
    pureError $ RevokedResourceCertificate locations
  validateResourceCertExtensions cert
  validateResourceSet cert parentCert
  pure wcer
  where 
    -- TODO Implement it
    validateResourceSet cert parentCert = pure ()
    


-- | Validate CRL object with the parent certificate
validateCrl :: Has Now conf => 
              CrlObject -> CerObject -> PureValidator conf (Validated CrlObject)
validateCrl crlObject@(CrlMeta {..}, SignCRL {..}) parentCert = do
  signatureCheck $ validateCRLSignature crlObject parentCert
  void $ validateNexUpdate (crlNextUpdate crl)
  pure $ Validated crlObject  


-- TODO Validate other stuff, validate resource certificate, etc.
validateMft :: Has Now conf => 
               WithMeta MftObject -> 
               CerObject -> 
               Validated CrlObject -> PureValidator conf (WithMeta MftObject)
validateMft wmft@(WithMeta RpkiMeta {..} mft) parentCert crl = do
  signatureCheck $ validateCMSSignature mft
  signatureCheck $ validateCMS'EECertSignature mft parentCert
  void $ validateNexUpdate $ Just $ nextTime $ getCMSContent mft
  when (isRevoked wmft crl) $ 
    pureError $ RevokedEECertificate locations
  pure wmft  


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
isRevoked :: WithMeta a -> Validated CrlObject -> Bool
isRevoked (WithMeta RpkiMeta {..} _) (Validated (_, SignCRL {..})) = 
  null $ filter
    (\(RevokedCertificate {..}) -> Serial revokedSerialNumber == serial) $ 
      crlRevokedCertificates crl
    

signatureCheck :: SignatureVerification -> PureValidator conf ()
signatureCheck sv = case sv of
    SignatureFailed e -> pureError $ InvalidSignature $ convert $ show e
    SignaturePass     -> pure ()        
