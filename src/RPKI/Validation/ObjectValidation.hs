{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module RPKI.Validation.ObjectValidation where
    
import           Control.Monad

import           Control.Lens
import           Data.Generics.Product.Typed

import qualified Data.ByteString                    as BS
import qualified Data.Text                          as Text
import           Data.Foldable (for_)

import qualified Data.Set                           as Set

import           Data.Proxy

import           Data.X509
import           Data.X509.Validation               hiding (InvalidSignature)
import           Data.ASN1.Types
import           GHC.Generics

import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Parse.Parse
import           RPKI.Resources.Types
import           RPKI.Resources.IntervalSet as IS
import           RPKI.TAL
import           RPKI.Time
import           RPKI.Util                          (convert)
import           RPKI.Validation.Crypto
import           RPKI.Validation.ResourceValidation
import           RPKI.Resources.Resources


newtype Validated a = Validated a
    deriving stock (Show, Eq, Generic)


class ExtensionValidator (t :: CertType) where
    validateResourceCertExtensions_ :: Proxy t -> [ExtensionRaw] -> PureValidatorT ()

instance ExtensionValidator 'CACert where
    validateResourceCertExtensions_ _ extensions = do             
        validateCaBasicConstraint extensions
        validatePolicyExtension extensions                        

instance ExtensionValidator 'BGPCert where
    -- https://datatracker.ietf.org/doc/html/rfc8209#section-3.1.3
    validateResourceCertExtensions_ _ = noBasicContraint

instance ExtensionValidator 'EECert where
    validateResourceCertExtensions_ _ = noBasicContraint

validateResourceCertExtensions :: forall c (t :: CertType) .
    (WithRawResourceCertificate c, OfCertType c t, ExtensionValidator t) => 
    c -> PureValidatorT c
validateResourceCertExtensions cert = do     
    let extensions = getExts $ cwsX509certificate $ getCertWithSignature cert

    validateResourceCertExtensions_ (Proxy :: Proxy t) extensions
    validateNoUnknownCriticalExtensions extensions            
    
    pure cert


-- https://datatracker.ietf.org/doc/html/rfc6487#section-4.8            
validateCaBasicConstraint :: [ExtensionRaw] -> PureValidatorT ()
validateCaBasicConstraint extensions = 
    withCriticalExtension extensions id_ce_basicConstraints $ \bs parsed ->
        case parsed of 
            [Start Sequence, Boolean _, End Sequence] -> pure ()                
            _ -> vPureError $ CertBrokenExtension id_ce_basicConstraints bs

noBasicContraint :: [ExtensionRaw] -> PureValidatorT ()
noBasicContraint extensions = 
    for_ (extVal extensions id_ce_basicConstraints) $ \bs -> 
        vPureError $ UnknownCriticalCertificateExtension id_ce_basicConstraints bs  

validatePolicyExtension :: [ExtensionRaw] -> PureValidatorT ()
validatePolicyExtension extensions = 
    withCriticalExtension extensions id_ce_certificatePolicies $ \bs parsed ->
        case parsed of 
            [ Start Sequence
                , Start Sequence
                , OID oid
                , End Sequence
                , End Sequence
                ] | oid == id_cp_ipAddr_asNumber -> pure ()        
            [ Start Sequence
                , Start Sequence
                , OID oid
                , Start Sequence
                , Start Sequence
                , OID oidCps
                , ASN1String _
                , End Sequence
                , End Sequence
                , End Sequence
                , End Sequence
                ] | oid == id_cp_ipAddr_asNumber && oidCps == id_cps_qualifier -> pure ()   

            _ -> vPureError $ CertBrokenExtension id_ce_certificatePolicies bs                
                
validateNoUnknownCriticalExtensions :: [ExtensionRaw] -> PureValidatorT ()
validateNoUnknownCriticalExtensions extensions =
    for_ extensions $ \ExtensionRaw {..} -> do 
        when (extRawCritical && extRawOID `notElem` allowedCriticalOIDs) $ 
            vPureError $ UnknownCriticalCertificateExtension extRawOID extRawContent
        

-- | Validated specifically the TA's self-signed certificate
-- 
validateTACert :: TAL -> RpkiURL -> RpkiObject -> PureValidatorT CaCerObject
validateTACert tal u (CerRO taCert) = do
    let spki = subjectPublicKeyInfo $ cwsX509certificate $ getCertWithSignature taCert
    let talSPKI = SPKI $ publicKeyInfo tal
    unless (talSPKI == spki) $ vPureError $ SPKIMismatch talSPKI spki
    validateTaCertAKI taCert u
    signatureCheck $ validateCertSignature taCert taCert
    validateResourceCertExtensions @CaCerObject @'CACert taCert

validateTACert _ _ _ = vPureError UnknownObjectAsTACert

validateTaCertAKI ::
    (WithAKI taCert, WithSKI taCert) =>
    taCert ->
    RpkiURL ->
    PureValidatorT ()
validateTaCertAKI taCert u =
    case getAKI taCert of
        Nothing -> pure ()
        Just (AKI ki)
            | SKI ki == getSKI taCert -> pure ()
            | otherwise -> vPureError $ TACertAKIIsNotEmpty (getURL u)

-- | Compare new certificate and the previous one
-- If validaity period of the new certificate is somehow earlier than 
-- the one of the previoius certificate, emit a warning and use
-- the previous certificate.
--
validateTACertWithPreviousCert :: CaCerObject -> CaCerObject -> PureValidatorT CaCerObject
validateTACertWithPreviousCert cert previousCert = do
    let validities = bimap Instant Instant . certValidity . cwsX509certificate . getCertWithSignature
    let (before, after) = validities cert
    let (prevBefore, prevAfter) = validities previousCert
    if before < prevBefore || after < prevAfter
        then do
            void $ vPureWarning $ TACertOlderThanPrevious{..}
            pure previousCert
        else pure cert

-- | In general, resource certifcate validation is:
--
--    - check the signature (with the parent)
--    - check all the needed extensions
--    - check expiration times
--    - check the resource set (needs the parent as well)
--    - check it's not revoked (needs CRL)
-- 
validateResourceCert :: forall child parent (childCertType :: CertType) .
    ( WithRawResourceCertificate child
    , WithRawResourceCertificate parent
    , WithSKI parent
    , WithAKI child
    , WithSerial child
    , WithValidityPeriod child
    , OfCertType parent 'CACert
    , OfCertType child childCertType
    , ExtensionValidator childCertType
    ) =>
    Now ->
    child ->    
    parent ->
    Validated CrlObject ->    
    PureValidatorT (Validated child)
validateResourceCert now cert parentCert vcrl = do
    
    signatureCheck $ validateCertSignature cert parentCert

    when (isRevoked cert vcrl) $ 
        vPureError RevokedResourceCertificate

    validateCertValidityPeriod cert now    

    unless (correctSkiAki cert parentCert) $
        vPureError $ AKIIsNotEqualsToParentSKI (getAKI cert) (getSKI parentCert)

    Validated <$> validateResourceCertExtensions @_ @childCertType cert    
  where
    correctSkiAki c (getSKI -> SKI s) =
        maybe False (\(AKI a) -> a == s) $ getAKI c


validateCertValidityPeriod :: WithValidityPeriod c => c -> Now -> PureValidatorT ()
validateCertValidityPeriod c (Now now) = do 
    let (before, after) = getValidityPeriod c
    when (now < before) $ 
        vPureError $ CertificateIsInTheFuture before after
    when (now > after) $ 
        vPureError $ CertificateIsExpired before after


validateResources ::
    (WithRawResourceCertificate c, 
     WithRawResourceCertificate parent,
     WithRFC c,
     OfCertType parent 'CACert) =>
    Maybe (VerifiedRS PrefixesAndAsns) ->
    c ->
    parent ->
    PureValidatorT (VerifiedRS PrefixesAndAsns)
validateResources verifiedResources cert parentCert =
    validateChildParentResources
        (getRFC cert)
        (getRawCert cert ^. typed)
        (getRawCert parentCert ^. typed)
        verifiedResources


validateBgpCert ::
    forall c parent.
    ( WithRawResourceCertificate c
    , WithRawResourceCertificate parent
    , WithSKI parent
    , WithAKI c
    , WithSKI c
    , WithValidityPeriod c
    , WithSerial c
    , OfCertType c 'BGPCert
    , OfCertType parent 'CACert
    ) =>
    Now ->
    c ->
    parent ->
    Validated CrlObject ->
    PureValidatorT BGPSecPayload
validateBgpCert now bgpCert parentCert validCrl = do
    -- Validate BGP certificate according to 
    -- https://www.rfc-editor.org/rfc/rfc8209.html#section-3.3    

    -- Validate resource set
    void $ validateResourceCert @_ @_ @ 'BGPCert now bgpCert parentCert validCrl

    let cwsX509 = cwsX509certificate $ getCertWithSignature bgpCert

    -- No SIA
    for_ (getSiaExt cwsX509) $ \sia -> 
        vError $ BGPCertSIAPresent sia

    -- No IP resources
    let AllResources ipv4 ipv6 asns = getRawCert bgpCert ^. #resources
    ipMustBeEmpty ipv4 BGPCertIPv4Present
    ipMustBeEmpty ipv6 BGPCertIPv6Present    
    
    -- Must be some ASNs
    bgpSecAsns <- case asns of 
                Inherit -> vError BGPCertBrokenASNs
                RS i
                    | IS.null i -> vError BGPCertBrokenASNs                
                    | otherwise -> pure $ unwrapAsns $ IS.toList i

    let bgpSecSki = getSKI bgpCert

    -- https://www.rfc-editor.org/rfc/rfc8208#section-3.1    
    let bgpSecSpki = subjectPublicKeyInfo cwsX509
    pure BGPSecPayload {..}
  where 
    ipMustBeEmpty ips errConstructor = 
        case ips of 
            Inherit -> vError errConstructor
            RS i    -> unless (IS.null i) $ vError errConstructor


-- | Validate CRL object with the parent certificate
validateCrl ::    
    WithRawResourceCertificate c =>
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
  (WithRawResourceCertificate c, WithSKI c, OfCertType c 'CACert) =>
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
    (WithRawResourceCertificate c, WithSKI c, OfCertType c 'CACert) =>
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
            for_ (getCMSContent roaCMS) $ \vrp@(Vrp _ prefix maxLength) ->
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
        forall a.
        (Interval a, HasType (IntervalSet a) PrefixesAndAsns) =>
        Maybe (VerifiedRS PrefixesAndAsns) ->
        (a -> (PrefixesAndAsns -> ValidationError) -> PureValidatorT ())
    validatedPrefixInRS = \case
            Nothing               -> \_ _ -> pure ()
            Just (VerifiedRS vrs) -> \i' errorReport ->
                unless (isInside i' (vrs ^. typed)) $
                    vPureError $ errorReport vrs

validateGbr ::
    (WithRawResourceCertificate c, WithSKI c, OfCertType c 'CACert) =>
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

validateRsc ::
    (WithRawResourceCertificate c, WithSKI c, OfCertType c 'CACert) =>
    Now ->
    RscObject ->
    c ->
    Validated CrlObject ->
    Maybe (VerifiedRS PrefixesAndAsns) ->
    PureValidatorT (Validated RscObject)
validateRsc now rsc parentCert crl verifiedResources = do
    void $
        validateCms now (cmsPayload rsc) parentCert crl verifiedResources $ \rscCms -> do
            let rsc' = getCMSContent rscCms
            let rc = getRawCert $ getEEResourceCert $ unCMS rscCms
            let eeCert = toPrefixesAndAsns $ resources rc 
            validateNested (rsc' ^. #rscResources) eeCert            
            
    pure $ Validated rsc

validateAspa ::
    (WithRawResourceCertificate parent, WithSKI parent, OfCertType parent 'CACert) =>
    Now ->
    AspaObject ->
    parent ->
    Validated CrlObject ->
    Maybe (VerifiedRS PrefixesAndAsns) ->
    PureValidatorT (Validated AspaObject)
validateAspa now aspa parentCert crl verifiedResources = do
    void $
        validateCms now (cmsPayload aspa) parentCert crl verifiedResources $ \aspaCms -> do
            let Aspa {..} = getCMSContent aspaCms            
            case filter (\(asn, _) -> asn == customerAsn) providerAsns of 
                []       -> pure ()
                _overlap -> vError $ AspaOverlappingCustomerProvider customerAsn (map fst providerAsns)
    pure $ Validated aspa


validateCms :: forall a c .
    (WithRawResourceCertificate c, 
    WithSKI c, 
    OfCertType c 'CACert) =>
    Now ->
    CMS a ->
    c ->
    Validated CrlObject ->
    Maybe (VerifiedRS PrefixesAndAsns) ->
    (CMS a -> PureValidatorT ()) ->
    PureValidatorT ()
validateCms now cms parentCert crl verifiedResources extraValidation = do
    -- Signature algorithm in the EE certificate has to be
    -- exactly the same as in the signed attributes
    let eeCert = getEEResourceCert $ unCMS cms
    let certWSign = getCertWithSignature eeCert
    let SignatureAlgorithmIdentifier eeCertSigAlg = certWSign ^. #cwsSignatureAlgorithm
    let attributeSigAlg = certWSign ^. #cwsX509certificate . #certSignatureAlg

    -- That can be a problem:
    -- http://sobornost.net/~job/arin-manifest-issue-2020.08.12.txt
    -- Correct behaviour is to request exact match here.
    unless (eeCertSigAlg == attributeSigAlg) $
        vPureError $
            CMSSignatureAlgorithmMismatch
                (Text.pack $ show eeCertSigAlg)
                (Text.pack $ show attributeSigAlg)

    signatureCheck $ validateCMSSignature cms
    void $ validateResourceCert @_ @_ @'EECert now eeCert parentCert crl
    void $ validateResources verifiedResources eeCert parentCert
    extraValidation cms

-- | Validate the nextUpdateTime for objects that have it (MFT, CRLs)
validateNextUpdate :: Now -> Maybe Instant -> PureValidatorT Instant
validateNextUpdate (Now now) nextUpdateTime =
    case nextUpdateTime of
        Nothing -> vPureError NextUpdateTimeNotSet
        Just nextUpdate
            | nextUpdate < now -> vPureError $ NextUpdateTimeIsInThePast nextUpdate now
            | otherwise -> pure nextUpdate

-- | Validate the thisUpdateTeim for objects that have it (MFT, CRLs)
validateThisUpdate :: Now -> Instant -> PureValidatorT Instant
validateThisUpdate (Now now) thisUpdateTime
    | thisUpdateTime >= now = vPureError $ ThisUpdateTimeIsInTheFuture thisUpdateTime now
    | otherwise = pure thisUpdateTime

-- | Check if CMS is on the revocation list
isRevoked :: WithSerial c => c -> Validated CrlObject -> Bool
isRevoked (getSerial -> serial) (Validated crlObject) = 
    Set.member serial revokenSerials
  where
    SignCRL{..} = signCrl crlObject

signatureCheck :: SignatureVerification -> PureValidatorT ()
signatureCheck sv = case sv of
    SignatureFailed e -> vPureError $ InvalidSignature $ convert $ show e
    SignaturePass -> pure ()

validateSizeM :: ValidationConfig -> Integer -> PureValidatorT Integer
validateSizeM vc s = vFromEither $ validateSize vc s

validateSizeOfBS :: ValidationConfig -> BS.ByteString -> Either ValidationError Integer
validateSizeOfBS vc bs = validateSize vc (toInteger $ BS.length bs)

validateSize :: ValidationConfig -> Integer -> Either ValidationError Integer
validateSize vc s =    
    case () of
        _
            | s < vc ^. #minObjectSize -> Left $ ObjectIsTooSmall s
            | s > vc ^. #maxObjectSize -> Left $ ObjectIsTooBig s
            | otherwise                -> pure s
