{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}

module RPKI.Validation.ObjectValidation where

import           Control.Monad
import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Types
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as LBS
import           Data.Data                          (Typeable)
import           Data.Hourglass                     (DateTime)
import           Data.X509
import           Data.X509.Validation               hiding (InvalidSignature)
import           GHC.Generics

import           System.Hourglass                   (dateCurrent)

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Parse.Parse
import           RPKI.Resources.Types
import           RPKI.TAL
import           RPKI.Util                          (convert)
import           RPKI.Validation.Crypto
import           RPKI.Validation.ResourceValidation

-- | Current time that is to be passed into the environment of validating functions
newtype Now = Now DateTime
  deriving (Show, Eq, Ord)

newtype Validated a = Validated a
  deriving (Show, Eq, Typeable, Generic)

thisMoment :: IO Now
thisMoment = Now <$> dateCurrent

validateResourceCertExtensions :: WithResourceCertificate c => c -> PureValidator conf c
validateResourceCertExtensions cert =
  validateHasCriticalExtensions $ cwsX509certificate $ getCertWithSignature cert
  where
    validateHasCriticalExtensions x509cert = do
      let exts = getExts x509cert
      case extVal exts id_ce_certificatePolicies of
        Nothing -> vPureError CertNoPolicyExtension
        Just bs
          | BS.null bs -> vPureError CertNoPolicyExtension
          | otherwise ->
            case decodeASN1 DER (LBS.fromStrict bs) of
              -- TODO Make it more generic
              Right
                [ Start Sequence,
                  Start Sequence,
                  OID oid,
                  End Sequence,
                  End Sequence
                  ] | oid == id_cp_ipAddr_asNumber -> pure cert
              Right
                [ Start Sequence,
                  Start Sequence,
                  OID oid,
                  Start Sequence,
                  Start Sequence,
                  OID oidCps,
                  ASN1String _,
                  End Sequence,
                  End Sequence,
                  End Sequence,
                  End Sequence
                  ] | oid == id_cp_ipAddr_asNumber && oidCps == id_cps_qualifier -> pure cert
              _ -> vPureError $ CertWrongPolicyExtension bs

-- |
validateTACert :: TAL -> URI -> RpkiObject -> PureValidator conf CerObject
validateTACert tal u (CerRO taCert) = do
  let spki = subjectPublicKeyInfo $ cwsX509certificate $ getCertWithSignature taCert
  pureErrorIfNot (publicKeyInfo tal == spki) $ SPKIMismatch (publicKeyInfo tal) spki
  case getAKI taCert of
    Nothing -> continue
    Just (AKI ki)
      | SKI ki == getSKI taCert -> continue
      | otherwise -> vPureError $ TACertAKIIsNotEmpty u
  where
    continue = do
      -- It's self-signed, so use itself as a parent to check the signature
      signatureCheck $ validateCertSignature taCert taCert
      validateResourceCertExtensions taCert >> pure taCert
      
validateTACert _ _ _ = vPureError UnknownObjectAsTACert

-- |
--    In general, resource certifcate validation is:
--
--    - check the signature (with the parent)
--    - check if it's revoked
--    - check all the needed extensions
--    - check expiration times
--    - check the resource set (needs the parent as well)
--    - check it's not revoked (needs CRL)
validateResourceCert ::
  ( WithResourceCertificate c,
    WithResourceCertificate parent,
    WithSKI parent,
    WithAKI c
  ) =>
  Now ->
  c ->
  parent ->
  Validated CrlObject ->
  PureValidator conf (Validated c)
validateResourceCert (Now now) cert parentCert vcrl = do
  let (before, after) = certValidity $ cwsX509certificate $ getCertWithSignature cert
  signatureCheck $ validateCertSignature cert parentCert
  when (isRevoked cert vcrl) $ vPureError RevokedResourceCertificate
  when (now < before) $ vPureError CertificateIsInTheFuture
  when (now > after) $ vPureError CertificateIsExpired
  when (not (correctSkiAki cert parentCert))
    $ vPureError
    $ AKIIsNotEqualsToParentSKI (getAKI cert) (getSKI parentCert)
  void $ validateResourceCertExtensions cert
  pure $ Validated cert
  where
    correctSkiAki c (getSKI -> SKI s) =
      maybe False (\(AKI a) -> a == s) $ getAKI c

validateResources ::
  ( WithResourceCertificate c,
    WithResourceCertificate parent
  ) =>
  Maybe (VerifiedRS PrefixesAndAsns) ->
  c ->
  parent ->
  PureValidator conf (VerifiedRS PrefixesAndAsns)
validateResources
  verifiedResources
  (getRC -> ResourceCertificate cert)
  (getRC -> ResourceCertificate parentCert) =
    validateChildParentResources
      validationRFC
      (withRFC cert resources)
      (withRFC parentCert resources)
      verifiedResources
    where
      validationRFC = forRFC cert (const Strict_) (const Reconsidered_)

-- | Validate CRL object with the parent certificate
validateCrl ::
  WithResourceCertificate c =>
  Now ->
  CrlObject ->
  c ->
  PureValidator conf (Validated CrlObject)
validateCrl now crlObject parentCert = do
  signatureCheck $ validateCRLSignature crlObject parentCert
  void $ validateNexUpdate now (crlNextUpdate crl)
  pure $ Validated crlObject
  where
    SignCRL {..} = extract crlObject

validateMft ::
  (WithResourceCertificate c, WithSKI c) =>
  Now ->
  MftObject ->
  c ->
  Validated CrlObject ->
  PureValidator conf (Validated MftObject)
validateMft now mft parentCert crl = do
  void $ validateCms now (extract mft) parentCert crl $ \m -> do
    void $ validateNexUpdate now $ Just $ nextTime $ getCMSContent m
    pure m
  pure $ Validated mft

validateRoa ::
  (WithResourceCertificate c, WithSKI c) =>
  Now ->
  RoaObject ->
  c ->
  Validated CrlObject ->
  PureValidator conf (Validated RoaObject)
validateRoa now roa parentCert crl = do
  void $ validateCms now (extract roa) parentCert crl pure
  pure $ Validated roa

validateGbr ::
  (WithResourceCertificate c, WithSKI c) =>
  Now ->
  GbrObject ->
  c ->
  Validated CrlObject ->
  PureValidator conf (Validated GbrObject)
validateGbr now gbr parentCert crl = do
  void $ validateCms now (extract gbr) parentCert crl pure
  -- TODO Implement it
  pure $ Validated gbr

validateCms ::
  (WithResourceCertificate c, WithSKI c) =>
  Now ->
  CMS a ->
  c ->
  Validated CrlObject ->
  (CMS a -> PureValidator conf (CMS a)) ->
  PureValidator conf (Validated (CMS a))
validateCms now cms parentCert crl extraValidation = do
  signatureCheck $ validateCMSSignature cms
  let eeCert = getEEResourceCert $ unCMS cms
  void $ validateResourceCert now eeCert parentCert crl
  Validated <$> extraValidation cms

-- | Validate the nextUpdateTime for objects that have it (MFT, CRLs)
validateNexUpdate :: Now -> Maybe DateTime -> PureValidator conf DateTime
validateNexUpdate (Now now) nextUpdateTime = do
  case nextUpdateTime of
    Nothing -> vPureError NextUpdateTimeNotSet
    Just nextUpdate
      | nextUpdate < now -> vPureError $ NextUpdateTimeIsBeforeNow nextUpdate
      | otherwise -> pure nextUpdate

-- | Check if CMS is on the revocation list
isRevoked :: WithResourceCertificate c => c -> Validated CrlObject -> Bool
isRevoked cert (Validated crlObject) =
  not $ null
    $ filter
      (\(RevokedCertificate {..}) -> Serial revokedSerialNumber == serial)
    $ crlRevokedCertificates crl
  where
    SignCRL {..} = extract crlObject
    serial = getSerial cert

signatureCheck :: SignatureVerification -> PureValidator conf ()
signatureCheck sv = case sv of
  SignatureFailed e -> vPureError $ InvalidSignature $ convert $ show e
  SignaturePass     -> pure ()

validateSizeOfBS :: BS.ByteString -> PureValidator c BS.ByteString
validateSizeOfBS bs = validateSizeM (toInteger $ BS.length bs) >> pure bs

validateSizeM :: Integer -> PureValidator c Integer
validateSizeM s = vFromEither $ validateSize s

validateSize :: Integer -> Either ValidationError Integer
validateSize s =
  case () of
    _
      | s < 10 -> Left $ ObjectIsTooSmall s
      | s > 50_000_000 -> Left $ ObjectIsTooBig s
      | otherwise -> pure s
