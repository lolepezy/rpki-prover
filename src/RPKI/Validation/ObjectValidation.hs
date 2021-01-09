{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module RPKI.Validation.ObjectValidation where
    
import           Control.Monad

import           Control.Lens
import           Data.Generics.Labels
import           Data.Generics.Product.Typed

import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Types
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.Text                          as Text

import qualified Data.Set                           as Set

import           Data.X509
import           Data.X509.Validation               hiding (InvalidSignature)
import           GHC.Generics

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Parse.Parse
import           RPKI.Resources.Types
import           RPKI.Resources.IntervalSet
import           RPKI.TAL
import           RPKI.Time
import           RPKI.Util                          (convert)
import           RPKI.Validation.Crypto
import           RPKI.Validation.ResourceValidation
import RPKI.Resources.Resources
import Data.Maybe (fromJust)



newtype Validated a = Validated a
    deriving stock (Show, Eq, Generic)

validateResourceCertExtensions :: WithResourceCertificate c => 
                                    c -> PureValidatorT c
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
validateTACert :: TAL -> RpkiURL -> RpkiObject -> PureValidatorT CerObject
validateTACert tal u (CerRO taCert) = do
  let spki = subjectPublicKeyInfo $ cwsX509certificate $ getCertWithSignature taCert
  pureErrorIfNot (publicKeyInfo tal == spki) $ SPKIMismatch (publicKeyInfo tal) spki
  case getAKI taCert of
    Nothing -> continue
    Just (AKI ki)
      | SKI ki == getSKI taCert -> continue
      | otherwise -> vPureError $ TACertAKIIsNotEmpty (getURL u)
  where
    continue = do
      -- It's self-signed, so use itself as a parent to check the signature
      signatureCheck $ validateCertSignature taCert taCert
      validateResourceCertExtensions taCert >> pure taCert
      
validateTACert _ _ _ = vPureError UnknownObjectAsTACert


-- | In general, resource certifcate validation is:
--
--    - check the signature (with the parent)
--    - check if it's not revoked
--    - check all the needed extensions
--    - check expiration times
--    - check the resource set (needs the parent as well)
--    - check it's not revoked (needs CRL)
-- 
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
  PureValidatorT (Validated c)
validateResourceCert (Now now) cert parentCert vcrl = do
    let (before, after) = certValidity $ cwsX509certificate $ getCertWithSignature cert
    signatureCheck $ validateCertSignature cert parentCert
    when (isRevoked cert vcrl)  $ vPureError RevokedResourceCertificate
    when (now < Instant before) $ vPureError CertificateIsInTheFuture
    when (now > Instant after)  $ vPureError CertificateIsExpired
    unless (correctSkiAki cert parentCert)
        $ vPureError $ AKIIsNotEqualsToParentSKI (getAKI cert) (getSKI parentCert)
    void $ validateResourceCertExtensions cert
    pure $ Validated cert
    where
        correctSkiAki c (getSKI -> SKI s) =
            maybe False (\(AKI a) -> a == s) $ getAKI c


validateResources :: (WithResourceCertificate c, WithResourceCertificate parent) =>
                    Maybe (VerifiedRS PrefixesAndAsns) ->
                    c ->
                    parent ->
                    PureValidatorT (VerifiedRS PrefixesAndAsns)
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
    PureValidatorT (Validated CrlObject)
validateCrl now crlObject parentCert = do
    signatureCheck $ validateCRLSignature crlObject parentCert
    void $ validateThisUpdate now thisUpdateTime
    void $ validateNextUpdate now nextUpdateTime    
    pure $ Validated crlObject
    where
        SignCRL {..} = signCrl crlObject

validateMft ::
  (WithResourceCertificate c, WithSKI c) =>
  Now ->
  MftObject ->
  c ->
  Validated CrlObject ->
  Maybe (VerifiedRS PrefixesAndAsns) ->
  PureValidatorT (Validated MftObject)
validateMft now mft parentCert crl verifiedResources = do
    void $ validateCms now (cmsPayload mft) parentCert crl verifiedResources $ \mftCMS -> do
        let cmsContent = getCMSContent mftCMS
        void $ validateThisUpdate now $ thisTime cmsContent
        void $ validateNextUpdate now $ Just $ nextTime cmsContent        
    pure $ Validated mft

validateRoa ::
    (WithResourceCertificate c, WithSKI c) =>
    Now ->
    RoaObject ->
    c ->
    Validated CrlObject ->
    Maybe (VerifiedRS PrefixesAndAsns) ->
    PureValidatorT (Validated RoaObject)
validateRoa now roa parentCert crl verifiedResources = do
    void $
        validateCms now (cmsPayload roa) parentCert crl verifiedResources $ \roaCMS -> do
            let checkerV4 = validatedPrefixInRS @Ipv4Prefix verifiedResources
            let checkerV6 = validatedPrefixInRS @Ipv6Prefix verifiedResources
            forM_ (getCMSContent roaCMS) $ \vrp@(Vrp _ prefix maxLength) ->
                case prefix of
                    Ipv4P p -> do 
                        checkerV4 p (RoaPrefixIsOutsideOfResourceSet prefix)
                        when (ipv4PrefixLen p > maxLength) $ 
                            vPureError $ RoaPrefixLenghtsIsBiggerThanMaxLength vrp
                    Ipv6P p -> do 
                        checkerV6 p (RoaPrefixIsOutsideOfResourceSet prefix)
                        when (ipv6PrefixLen p > maxLength) $ 
                            vPureError $ RoaPrefixLenghtsIsBiggerThanMaxLength vrp
    pure $ Validated roa
  where
    validatedPrefixInRS ::
        forall a .
        (Interval a, HasType (IntervalSet a) PrefixesAndAsns) =>
        Maybe (VerifiedRS PrefixesAndAsns) ->         
        (a -> (PrefixesAndAsns -> ValidationError) -> PureValidatorT ())
    validatedPrefixInRS verifiedResources =
        case verifiedResources of
            Nothing -> \_ _ -> pure ()
            Just (VerifiedRS vrs) ->
                let rs = vrs ^. typed
                 in \i errorReport ->
                        unless (isInside i rs) $
                            vPureError $ errorReport vrs


validateGbr ::
    (WithResourceCertificate c, WithSKI c) =>
    Now ->
    GbrObject ->
    c ->
    Validated CrlObject ->
    Maybe (VerifiedRS PrefixesAndAsns) ->
    PureValidatorT (Validated GbrObject)
validateGbr now gbr parentCert crl verifiedResources = do
    void $
        validateCms now (cmsPayload gbr) parentCert crl verifiedResources $ \gbrCms -> do
            let Gbr vcardBS = getCMSContent gbrCms
            case parseVCard $ toNormalBS vcardBS of
                Left e -> vPureError $ InvalidVCardFormatInGbr e
                Right _ -> pure ()
    pure $ Validated gbr

validateCms ::
    (WithResourceCertificate c, WithSKI c) =>
    Now 
    -> CMS a 
    -> c 
    -> Validated CrlObject 
    -> Maybe (VerifiedRS PrefixesAndAsns) 
    -> (CMS a -> PureValidatorT ()) 
    -> PureValidatorT ()
validateCms now cms parentCert crl verifiedResources extraValidation = do
  -- Signature algorithm in the EE certificate has to be 
  -- exactly the same as in the signed attributes
  let eeCert = getEEResourceCert $ unCMS cms  
  let certWSign = getCertWithSignature eeCert
  let SignatureAlgorithmIdentifier eeCertSigAlg = certWSign ^. #cwsSignatureAlgorithm
  let attributeSigAlg                           = certWSign ^. #cwsX509certificate . #certSignatureAlg  

  -- That can be a problem:
  -- http://sobornost.net/~job/arin-manifest-issue-2020.08.12.txt
  -- Correct behaviour is to request exact match here.
  unless (eeCertSigAlg == attributeSigAlg) $ 
    vPureError $ CMSSignatureAlgorithmMismatch 
                    (Text.pack $ show eeCertSigAlg) (Text.pack $ show attributeSigAlg)
    
  signatureCheck $ validateCMSSignature cms  
  void $ validateResourceCert now eeCert parentCert crl
  void $ validateResources verifiedResources eeCert parentCert   
  extraValidation cms

-- | Validate the nextUpdateTime for objects that have it (MFT, CRLs)
validateNextUpdate :: Now -> Maybe Instant -> PureValidatorT Instant
validateNextUpdate (Now now) nextUpdateTime =
  case nextUpdateTime of
    Nothing -> vPureError NextUpdateTimeNotSet
    Just nextUpdate
      | nextUpdate < now -> vPureError $ NextUpdateTimeIsInThePast nextUpdate now
      | otherwise        -> pure nextUpdate

-- | Validate the thisUpdateTeim for objects that have it (MFT, CRLs)
validateThisUpdate :: Now -> Instant -> PureValidatorT Instant
validateThisUpdate (Now now) thisUpdateTime
      | thisUpdateTime >= now = vPureError $ ThisUpdateTimeIsInTheFuture thisUpdateTime now
      | otherwise             = pure thisUpdateTime

-- | Check if CMS is on the revocation list
isRevoked :: WithResourceCertificate c => c -> Validated CrlObject -> Bool
isRevoked cert (Validated crlObject) =
  Set.member serial revokenSerials
  where
    SignCRL {..} = signCrl crlObject
    serial = getSerial cert

signatureCheck :: SignatureVerification -> PureValidatorT ()
signatureCheck sv = case sv of
  SignatureFailed e -> vPureError $ InvalidSignature $ convert $ show e
  SignaturePass     -> pure ()

validateSizeOfBS :: BS.ByteString -> PureValidatorT BS.ByteString
validateSizeOfBS bs = validateSizeM (toInteger $ BS.length bs) >> pure bs

validateSizeM :: Integer -> PureValidatorT Integer
validateSizeM s = vFromEither $ validateSize s

validateSize :: Integer -> Either ValidationError Integer
validateSize s =
  case () of
    _
      | s < 10         -> Left $ ObjectIsTooSmall s
      | s > 10_000_000 -> Left $ ObjectIsTooBig s
      | otherwise -> pure s
