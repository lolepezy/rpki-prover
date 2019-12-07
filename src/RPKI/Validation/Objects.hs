{-# LANGUAGE RecordWildCards #-}
module RPKI.Validation.Objects where

import           Control.Monad
import           Control.Monad.Reader

import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           Data.Maybe

import           Data.Has

import           Data.X509
import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Types

import           Data.Hourglass           (DateTime)
import           Data.X509.Validation     hiding (InvalidSignature)

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

{- 
    TODO Make is more consistent.

    In general, resource certifcate validation is:

    - check the signature (with the parent)
    - check all the needed extensions and expiration times
    - check the resource set (needs the parent as well)
    - check it's not revoked (needs CRL)
 -}
validateResourceCertItself :: CerObject -> PureValidator conf CerObject
validateResourceCertItself cert@(ResourceCert rc) = 
  validateHasCriticalExtensions $ getX509Cert $ withRFC rc certX509     
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
    let spki = subjectPublicKeyInfo $ getX509Cert $ withRFC taCert certX509
    pureErrorIfNot (publicKeyInfo tal == spki) $ SPKIMismatch (publicKeyInfo tal) spki    
    pureErrorIfNot (isNothing aki) $ TACertAKIIsNotEmpty u
    -- It's self-signed, so use itself as a parent to check the signature
    case validateSignature ro cert of
      SignatureFailed f -> pureError $ InvalidSignature (convert $ show f)
      SignaturePass     -> WithMeta m <$> validateResourceCertItself cert

validateTACert _ _ _ = pureError UnknownObjectAsTACert


-- | Validate CRL object with the parent certificate
validateCrl :: Has Now conf => 
              CrlObject -> CerObject -> PureValidator conf CrlObject
validateCrl crlObject@(CrlMeta {..}, SignCRL {..}) parentCert = do
  case validateCRLSignature crlObject parentCert of
    SignatureFailed f -> pureError $ InvalidSignature (convert $ show f)
    SignaturePass     -> validateNexUpdate
  where
    validateNexUpdate = do 
      Now now <- asks getter
      case crlNextUpdate crl of
        Nothing -> pureError CRLNextUpdateTimeNotSet
        Just nextUpdateTime 
          | nextUpdateTime < now -> pureError $ CRLNextUpdateTimeIsBeforeNow nextUpdateTime
          | otherwise            -> pure crlObject


validateMft :: Has Now conf => 
              WithMeta MftObject -> CerObject -> PureValidator conf (WithMeta MftObject)
validateMft wm@(WithMeta m mft) parentCert = do
  pure wm