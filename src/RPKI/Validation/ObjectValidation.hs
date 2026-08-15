{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE OverloadedStrings    #-}

module RPKI.Validation.ObjectValidation where
    
import           Control.Monad

import           Control.Lens
import           Data.Generics.Product.Typed

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Short            as BSS
import qualified Data.Text                        as Text
import           Data.Foldable (for_)
import qualified Data.Set                         as Set
import qualified Data.Map.Strict                  as Map

import           Data.Proxy

import qualified Crypto.PubKey.RSA.Types          as RSA
import qualified Crypto.Hash.SHA1                 as SHA1

import           Data.X509
import           Data.X509.Validation               hiding (InvalidSignature)
import           Data.ASN1.Types
import           Data.ASN1.Encoding                 (encodeASN1')
import           Data.ASN1.BinaryEncoding           (DER(..))
import           Data.ASN1.BitArray               (BitArray(..))

import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Parse.Parse
import           RPKI.Resources.Types
import           RPKI.Resources.IntervalContainers as IS
import           RPKI.TAL
import           RPKI.Time
import           RPKI.Util                          (convert)
import qualified RPKI.Util                         as U
import           RPKI.Validation.Crypto
import           RPKI.Validation.ResourceValidation
import           RPKI.Resources.Resources
import           RPKI.Validation.Common        


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
        

-- | Validate specifically the TA's self-signed certificate.
validateTACert :: TAL -> RpkiURL -> ParsedRpkiObject -> PureValidatorT ValidatedCaCert
validateTACert tal u (CerRO taCert) = do
    let spki = getSubjectPublicKeyInfo $ cwsX509certificate $ getCertWithSignature taCert
    let talSPKI = SPKI $ publicKeyInfo tal
    unless (talSPKI == spki) $ vPureError $ SPKIMismatch talSPKI spki
    validateTaCertAKI taCert u
    signatureCheck $ validateCertSignature taCert taCert
    validateResourceCertExtensions @CaCerObject @'CACert taCert
    pure $ extractCert taCert

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

--
-- Use the tiebreaker logic proposed by 
-- https://datatracker.ietf.org/doc/draft-spaghetti-sidrops-rpki-ta-tiebreaker/
--
-- Emit a warning when deciding to use the cached certificate 
-- instead of the fetched one.
-- 
chooseTaCert :: ValidatedCaCert -> ValidatedCaCert -> PureValidatorT ValidatedCaCert
chooseTaCert cert cachedCert = do
    let ValidityPeriod notBefore notAfter = getValidityPeriod cert
    let ValidityPeriod cachedNotBefore cachedNotAfter = getValidityPeriod cachedCert
    let bothValidities = TACertValidities {..}

        {- 
            Check whether the retrieved object has a more recent
            notBefore than the locally cached copy of the retrieved TA.
            If the notBefore of the retrieved object is less recent,
            use the locally cached copy of the retrieved TA.        
        -}
    if | notBefore < cachedNotBefore -> do
            void $ vPureWarning $ TACertPreferCachedCopy bothValidities
            pure cachedCert

        {- 
            If the notBefore dates are equal, check whether the
            retrieved object has a shorter validity period than the
            locally cached copy of the retrieved TA.  If the validity
            period of the retrieved object is longer, use the locally
            cached copy of the retrieved TA.        
        -}
        | notBefore == cachedNotBefore && cachedNotAfter < notAfter -> do 
            void $ vPureWarning $ TACertPreferCachedCopy bothValidities
            pure cachedCert            

        | otherwise -> pure cert


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
    cert' <- self
    wrtParent
    pure $ Validated cert'  
  where
    self = do 
        when (isRevoked cert vcrl) $ 
            vPureError RevokedResourceCertificate        
        void $ validateObjectValidityPeriod cert now    
        validateResourceCertExtensions @_ @childCertType cert

    wrtParent = do 
        signatureCheck $ validateCertSignature cert parentCert
        unless (correctSkiAki cert parentCert) $
            vPureError $ AKIIsNotEqualsToParentSKI (getAKI cert) (getSKI parentCert)
      where
        correctSkiAki c (getSKI -> SKI s) =
            maybe False (\(AKI a) -> a == s) $ getAKI c


validateObjectValidityPeriod :: WithValidityPeriod c => c -> Now -> PureValidatorT ValidityPeriod
validateObjectValidityPeriod c (Now now) = do 
    let vp@ValidityPeriod { notBefore, notAfter } = getValidityPeriod c
    when (now < notBefore) $ 
        vPureError $ ObjectValidityIsInTheFuture notBefore notAfter
    when (now > notAfter) $ 
        vPureError $ ObjectIsExpired notBefore notAfter
    pure vp


validateResources ::
    (WithRawResourceCertificate child, 
     WithRawResourceCertificate parent,
     parent `OfCertType` 'CACert) =>
    ValidationRFC ->
    Maybe (VerifiedRS PrefixesAndAsns) ->        
    child ->
    parent ->
    PureValidatorT (VerifiedRS PrefixesAndAsns, Maybe (Overclaiming PrefixesAndAsns))
validateResources validationRFC verifiedResources childCert parentCert =
    validateChildParentResources
        validationRFC
        (getRawCert childCert ^. typed)
        (getRawCert parentCert ^. typed)
        verifiedResources


validateBgpCert ::
    forall bgpCert parent.
    ( WithRawResourceCertificate bgpCert
    , WithRawResourceCertificate parent
    , WithSKI parent
    , WithAKI bgpCert
    , WithSKI bgpCert
    , WithValidityPeriod bgpCert
    , WithSerial bgpCert
    , bgpCert `OfCertType` BGPCert
    , parent `OfCertType` CACert
    ) =>
    Now ->
    bgpCert ->
    parent ->
    Validated CrlObject ->
    PureValidatorT (Validated bgpCert, BGPSecPayload)
validateBgpCert now bgpCert parentCert validCrl = do
    -- Validate BGP certificate according to 
    -- https://www.rfc-editor.org/rfc/rfc8209.html#section-3.3    

    -- Validate resource set
    void $ validateResourceCert @_ @_ @BGPCert now bgpCert parentCert validCrl

    let cwsX509 = cwsX509certificate $ getCertWithSignature bgpCert

    -- No SIA
    for_ (getSiaExt cwsX509) $ \sia -> 
        vError $ BGPCertSIAPresent sia

    -- No IP resources
    let AllResources ipv4 ipv6 asns = getRawCert bgpCert ^. #resources
    resourceSetMustBeEmpty ipv4 BGPCertIPv4Present
    resourceSetMustBeEmpty ipv6 BGPCertIPv6Present    
    
    -- Must be some ASNs
    bgpSecAsns <- case asns of 
                Inherit -> vError BGPCertBrokenASNs
                RS i
                    | IS.null i -> vError BGPCertBrokenASNs                
                    | otherwise -> pure $ unwrapAsns $ IS.toList i

    let bgpSecSki = getSKI bgpCert

    -- https://www.rfc-editor.org/rfc/rfc8208#section-3.1    
    let bgpSecSpki = getSubjectPublicKeyInfo cwsX509
    pure (Validated bgpCert, BGPSecPayload {..})

    

-- | Validate CRL object with the parent certificate
validateCrl ::    
    (WithPubKey parent, WithSKI parent) =>
    Now ->
    CrlObject ->
    parent ->
    PureValidatorT (Validated CrlObject)
validateCrl now crlObject@CrlObject {..} parentCert = do
    let SignCRL {..} = signCrl
    signatureCheck $ validateCRLSignature crlObject parentCert
    when (toAKI (getSKI parentCert) /= aki) $ 
        vPureError $ CRL_AKI_DifferentFromCertSKI (getSKI parentCert) aki
    case nextUpdateTime of 
        Nothing   -> vPureError NextUpdateTimeNotSet
        Just next -> validateUpdateTimes now thisUpdateTime next
    pure $ Validated crlObject  
    

validateMft ::
  (WithRawResourceCertificate parent, 
   WithSKI parent, 
   parent `OfCertType` CACert) =>
  ValidationRFC ->
  Now ->
  MftObject ->
  parent ->
  Validated CrlObject ->
  Maybe (VerifiedRS PrefixesAndAsns) ->
  PureValidatorT (Validated MftObject)
validateMft validationRFC now mft parentCert crl verifiedResources = do
    void $ validateCms validationRFC now (cmsPayload mft) parentCert crl verifiedResources $ \mftCMS -> do
        let Manifest {..} = getCMSContent mftCMS
        validateUpdateTimes now thisTime nextTime

        let AllResources ipv4 ipv6 asns = getRawCert (getEEResourceCert $ unCMS mftCMS) ^. #resources
        verifyInherit ipv4
        verifyInherit ipv6
        verifyInherit asns        

    pure $ Validated mft
  where
    verifyInherit = \case 
        Inherit -> pure ()                        
        RS _    -> vPureError ResourceSetMustBeInherit


validateRoa ::
    (WithRawResourceCertificate parent, 
     WithSKI parent, 
     OfCertType parent CACert) =>
    ValidationRFC ->
    Now ->
    RoaObject ->
    parent ->
    Validated CrlObject ->
    Maybe (VerifiedRS PrefixesAndAsns) ->
    PureValidatorT (Validated RoaObject)
validateRoa validationRFC now roa parentCert crl verifiedResources = do
    void $
        validateCms validationRFC now (cmsPayload roa) parentCert crl verifiedResources $ \roaCMS -> do
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

validateSpl ::
    (WithRawResourceCertificate parent, 
     WithSKI parent, 
     OfCertType parent CACert) =>
    ValidationRFC ->
    Now ->
    SplObject ->
    parent ->
    Validated CrlObject ->
    Maybe (VerifiedRS PrefixesAndAsns) ->
    PureValidatorT (Validated SplObject)
validateSpl validationRFC now spl parentCert crl verifiedResources = do
    void $
        validateCms validationRFC now (cmsPayload spl) parentCert crl verifiedResources $ \splCMS -> do            
            let SplPayload asn _ = getCMSContent splCMS
            for_ verifiedResources $ \(VerifiedRS vrs) -> do 
                let asns = vrs ^. typed
                unless (isInside (AS asn) asns) $
                    vPureError $ SplAsnNotInResourceSet asn (IS.toList asns)

            let AllResources ipv4 ipv6 _ = getRawCert (getEEResourceCert $ unCMS splCMS) ^. #resources
            resourceSetMustBeEmpty ipv4 (SplNotIpResources (ipToList Ipv4P ipv4))
            resourceSetMustBeEmpty ipv6 (SplNotIpResources (ipToList Ipv6P ipv6))

    pure $ Validated spl    
  where
    ipToList f = \case 
        Inherit -> []
        RS s    -> map f $ IS.toList s

validateGbr ::
    (WithRawResourceCertificate c, WithSKI c, OfCertType c 'CACert) =>
    ValidationRFC ->
    Now ->
    GbrObject ->
    c ->
    Validated CrlObject ->
    Maybe (VerifiedRS PrefixesAndAsns) ->
    PureValidatorT (Validated GbrObject)
validateGbr validationRFC now gbr parentCert crl verifiedResources = do
    void $
        validateCms validationRFC now (cmsPayload gbr) parentCert crl verifiedResources $ \gbrCms -> do
            let Gbr vcardBS = getCMSContent gbrCms
            case parseVCard $ toNormalBS vcardBS of
                Left e                   -> vPureError $ InvalidVCardFormatInGbr e
                Right (_, Just warnings) -> vPureWarning $ InvalidVCardFormatInGbr warnings
                Right _ -> pure ()
    pure $ Validated gbr

validateRsc ::
    (WithRawResourceCertificate c, WithSKI c, OfCertType c 'CACert) =>
    ValidationRFC ->
    Now ->
    RscObject ->
    c ->
    Validated CrlObject ->
    Maybe (VerifiedRS PrefixesAndAsns) ->
    PureValidatorT (Validated RscObject)
validateRsc validationRFC now rsc parentCert crl verifiedResources = do
    void $
        validateCms validationRFC now (cmsPayload rsc) parentCert crl verifiedResources $ \rscCms -> do
            let rsc' = getCMSContent rscCms
            let rc = getRawCert $ getEEResourceCert $ unCMS rscCms
            let eeCert = toPrefixesAndAsns $ rc ^. #resources 
            validateNested (rsc' ^. #rscResources) eeCert            
            
    pure $ Validated rsc

validateAspa ::
    (WithRawResourceCertificate parent, WithSKI parent, parent `OfCertType` 'CACert) =>
    ValidationRFC ->
    Now ->
    AspaObject ->
    parent ->
    Validated CrlObject ->
    Maybe (VerifiedRS PrefixesAndAsns) ->
    PureValidatorT (Validated AspaObject)
validateAspa validationRFC now aspa parentCert crl verifiedResources = do
    void $
        validateCms validationRFC now (aspa ^. #cmsPayload) parentCert crl verifiedResources $ \aspaCms -> do

            -- https://www.ietf.org/archive/id/draft-ietf-sidrops-aspa-profile-12.html#name-aspa-validation
            let AllResources ipv4 ipv6 asns = getRawCert (getEEResourceCert $ unCMS aspaCms) ^. #resources
            resourceSetMustBeEmpty ipv4 AspaIPv4Present
            resourceSetMustBeEmpty ipv6 AspaIPv6Present

            asnSet <- case asns of 
                        Inherit -> vError AspaNoAsn
                        RS s    -> pure s

            let Aspa {..} = getCMSContent aspaCms         

            unless ((AS customer) `IS.isInside` asnSet) $ 
                vError $ AspaAsNotOnEECert customer (IS.toList asnSet)

            when (customer `Set.member` providers) $
                vError $ AspaOverlappingCustomerProvider customer $ Set.toList providers
    
    pure $ Validated aspa
    


validateCms :: forall cms parent .
    (WithRawResourceCertificate parent, 
    WithSKI parent, 
    OfCertType parent 'CACert) =>
    ValidationRFC ->
    Now ->
    CMS cms ->
    parent ->
    Validated CrlObject ->
    Maybe (VerifiedRS PrefixesAndAsns) ->
    (CMS cms -> PureValidatorT ()) ->
    PureValidatorT ()
validateCms validationRFC now cms parentCert crl verifiedResources extraValidation = do
    -- Signature algorithm in the EE certificate has to be
    -- exactly the same as in the signed attributes
    let eeCert = getEEResourceCert $ unCMS cms
    let certWSign = getCertWithSignature eeCert
    let SignatureAlgorithmIdentifier eeCertSigAlg = certWSign ^. #cwsSignatureAlgorithm
    let attributeSigAlg = certSignatureAlg $ certWSign ^. #cwsX509certificate

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
    void $ validateResources validationRFC verifiedResources eeCert parentCert
    extraValidation cms


validateUpdateTimes :: Now -> Instant -> Instant -> PureValidatorT ()
validateUpdateTimes (Now now) thisUpdateTime nextUpdateTime = do
    when (thisUpdateTime >= now) $ vPureError $ ThisUpdateTimeIsInTheFuture {..}
    when (nextUpdateTime < now)  $ vPureError $ NextUpdateTimeIsInThePast {..}
    when (nextUpdateTime <= thisUpdateTime) $ 
        vPureError $ NextUpdateTimeBeforeThisUpdateTime {..}


validateAIA :: forall child parent (childCertType :: CertType) .
    (WithRawResourceCertificate child
    , WithLocations parent
    , OfCertType parent 'CACert
    , OfCertType child childCertType) =>    
    child  ->
    parent ->
    PureValidatorT ()
validateAIA cert parentCert =    
    for_ (getSiaExt $ cwsX509certificate $ getCertWithSignature cert) $ \sia -> do 
        for_ (extractSiaValue sia id_pe_sia) $ \ext -> do             
            let locations = getLocations parentCert
            case extractURI ext of 
                Left e               -> vPureWarning $ BrokenUri (Text.pack $ show ext) e
                Right u@(URI siaUrl) -> do                     
                    unless ("rsync://" `Text.isPrefixOf` siaUrl) $ 
                        vPureWarning $ MFTBadAIA u                
                    unless (siaUrl `elem` locationsToList locations) $                         
                        vPureWarning $ AIANotSameAsParentLocation siaUrl locations


-- | Check if CMS is on the revocation list
isRevoked :: WithSerial c => c -> Validated CrlObject -> Bool
isRevoked (getSerial -> serial) (Validated crlObject) = 
    Set.member serial revokedSerials
  where
    SignCRL{..} = signCrl crlObject

signatureCheck :: SignatureVerification -> PureValidatorT ()
signatureCheck sv = case sv of
    SignatureFailed e -> vPureError $ InvalidSignature $ convert $ show e
    SignaturePass -> pure ()


-- ============================================================
-- V-variant validators operating on 'ValidatedRpkiObject' types
-- ============================================================

-- | Validate a 'ValidatedCaCert' child against a 'ValidatedCaCert' parent.
-- Extension checks were already performed during prevalidation, so only
-- signature, revocation, validity period, and AKI/SKI are re-checked here.
validateResourceCertV :: 
    Now -> ValidatedCaCert -> ValidatedCaCert -> Validated CrlObject
    -> PureValidatorT (Validated ValidatedCaCert)
validateResourceCertV now cert parentCert vcrl = do
    signatureCheck $ validateCertSignatureCA cert parentCert
    when (isRevoked cert vcrl) $ vPureError RevokedResourceCertificate
    void $ validateObjectValidityPeriod cert now
    unless (maybe False (\(AKI a) -> a == unSKI (getSKI parentCert)) (getAKI cert)) $
        vPureError $ AKIIsNotEqualsToParentSKI (getAKI cert) (getSKI parentCert)
    pure $ Validated cert

-- | Validate a 'ValidatedEECert' (embedded in a CMS object) against a
-- 'ValidatedCaCert' parent.
validateEECertV ::
    Now -> ValidatedEECert -> ValidatedCaCert -> Validated CrlObject
    -> PureValidatorT ()
validateEECertV now eeCert parentCert vcrl = do
    signatureCheck $ validateCertSignatureEE eeCert parentCert
    when (isRevoked eeCert vcrl) $ vPureError RevokedResourceCertificate
    void $ validateObjectValidityPeriod eeCert now
    unless (getAKI eeCert == Just (toAKI $ getSKI parentCert)) $
        vPureError $ AKIIsNotEqualsToParentSKI (getAKI eeCert) (getSKI parentCert)

-- | Validate resources for a 'ValidatedEECert' child against a
-- 'ValidatedCaCert' parent.
validateResourcesEEV ::
    ValidationRFC ->
    Maybe (VerifiedRS PrefixesAndAsns) ->
    ValidatedEECert -> ValidatedCaCert ->
    PureValidatorT (VerifiedRS PrefixesAndAsns, Maybe (Overclaiming PrefixesAndAsns))
validateResourcesEEV validationRFC verifiedResources childCert parentCert =
    validateChildParentResources validationRFC (childCert ^. #resources) (parentCert ^. #resources) verifiedResources

-- | Validate resources for a 'ValidatedCaCert' child against a
-- 'ValidatedCaCert' parent.
validateResourcesCAV ::
    ValidationRFC ->
    Maybe (VerifiedRS PrefixesAndAsns) ->
    ValidatedCaCert -> ValidatedCaCert ->
    PureValidatorT (VerifiedRS PrefixesAndAsns, Maybe (Overclaiming PrefixesAndAsns))
validateResourcesCAV validationRFC verifiedResources childCert parentCert =
    validateChildParentResources validationRFC (childCert ^. #resources) (parentCert ^. #resources) verifiedResources

-- | Common CMS validation for validated objects.
validateCmsV ::
    ValidationRFC -> Now
    -> ValidatedCMSObject a
    -> ValidatedCaCert
    -> Validated CrlObject
    -> Maybe (VerifiedRS PrefixesAndAsns)
    -> (ValidatedCMSObject a -> PureValidatorT ())
    -> PureValidatorT (Validated (ValidatedCMSObject a))
validateCmsV validationRFC now cms parentCert crl verifiedResources extraValidation = do
    signatureCheck $ validateCMSSignatureV cms
    validateEECertV now (eeCert cms) parentCert crl
    void $ validateResourcesEEV validationRFC verifiedResources (eeCert cms) parentCert
    extraValidation cms
    pure $ Validated cms

validateRoaV ::
    ValidationRFC -> Now
    -> ValidatedCMSObject [Vrp] -> ValidatedCaCert -> Validated CrlObject
    -> Maybe (VerifiedRS PrefixesAndAsns)
    -> PureValidatorT (Validated (ValidatedCMSObject [Vrp]))
validateRoaV validationRFC now roa parentCert crl verifiedResources =
    validateCmsV validationRFC now roa parentCert crl verifiedResources $ \cms ->
        for_ (content cms) $ \vrp@(Vrp _ prefix maxLength) -> do
            case (verifiedResources, prefix) of
                (Just (VerifiedRS rs), Ipv4P p) ->
                    unless (isInside p (rs ^. typed)) $
                        vPureError $ RoaPrefixIsOutsideOfResourceSet prefix rs
                (Just (VerifiedRS rs), Ipv6P p) ->
                    unless (isInside p (rs ^. typed)) $
                        vPureError $ RoaPrefixIsOutsideOfResourceSet prefix rs
                _ -> pure ()
            when (prefixLen prefix > maxLength) $
                vPureError $ RoaPrefixLenghtsIsBiggerThanMaxLength vrp

validateMftV ::
    ValidationRFC -> Now
    -> ValidatedCMSObject Manifest -> ValidatedCaCert -> Validated CrlObject
    -> Maybe (VerifiedRS PrefixesAndAsns)
    -> PureValidatorT (Validated (ValidatedCMSObject Manifest))
validateMftV validationRFC now mft parentCert crl verifiedResources =
    validateCmsV validationRFC now mft parentCert crl verifiedResources $ \_ -> pure ()

validateGbrV ::
    ValidationRFC -> Now
    -> ValidatedCMSObject Gbr -> ValidatedCaCert -> Validated CrlObject
    -> Maybe (VerifiedRS PrefixesAndAsns)
    -> PureValidatorT (Validated (ValidatedCMSObject Gbr))
validateGbrV validationRFC now gbr parentCert crl verifiedResources =
    validateCmsV validationRFC now gbr parentCert crl verifiedResources $ \cms -> do
        let Gbr vcardBS = content cms
        case parseVCard $ toNormalBS vcardBS of
            Left e  -> vPureError $ InvalidVCardFormatInGbr e
            Right _ -> pure ()

validateAspaV ::
    ValidationRFC -> Now
    -> ValidatedCMSObject Aspa -> ValidatedCaCert -> Validated CrlObject
    -> Maybe (VerifiedRS PrefixesAndAsns)
    -> PureValidatorT (Validated (ValidatedCMSObject Aspa))
validateAspaV validationRFC now aspa parentCert crl verifiedResources =
    validateCmsV validationRFC now aspa parentCert crl verifiedResources $ \cms -> do
        let Aspa {..} = content cms
        when (customer `Set.member` providers) $
            vPureError $ AspaOverlappingCustomerProvider customer (Set.toList providers)

validateSplV ::
    ValidationRFC -> Now
    -> ValidatedCMSObject SplPayload -> ValidatedCaCert -> Validated CrlObject
    -> Maybe (VerifiedRS PrefixesAndAsns)
    -> PureValidatorT (Validated (ValidatedCMSObject SplPayload))
validateSplV validationRFC now spl parentCert crl verifiedResources =
    validateCmsV validationRFC now spl parentCert crl verifiedResources $ \_ -> pure ()

validateRscV ::
    ValidationRFC -> Now
    -> ValidatedCMSObject Rsc -> ValidatedCaCert -> Validated CrlObject
    -> Maybe (VerifiedRS PrefixesAndAsns)
    -> PureValidatorT (Validated (ValidatedCMSObject Rsc))
validateRscV validationRFC now rsc parentCert crl verifiedResources =
    validateCmsV validationRFC now rsc parentCert crl verifiedResources $ \_ -> pure ()

validateBgpCertV ::
    Now -> ValidatedCert 'BGPCert -> ValidatedCert 'CACert -> Validated CrlObject
    -> PureValidatorT (Validated (ValidatedCert 'BGPCert), BGPSecPayload)
validateBgpCertV now bgpCert parentCert vcrl = do
    let ee = bgpToEE bgpCert
    signatureCheck $ validateCertSignatureEE ee parentCert
    when (isRevoked bgpCert vcrl) $ vPureError RevokedResourceCertificate
    void $ validateObjectValidityPeriod bgpCert now
    unless (getAKI bgpCert == Just (toAKI $ getSKI parentCert)) $
        vPureError $ AKIIsNotEqualsToParentSKI (getAKI bgpCert) (getSKI parentCert)
    let bgpSecSki  = getSKI bgpCert
    let bgpSecAsns = case bgpCert ^. #resources of
            AllResources _ _ (RS asSet) -> unwrapAsns $ IS.toList asSet
            _                           -> []
    let bgpSecSpki = SPKI $ U.encodeBase64 $ DecodedBase64 $
                        encodeASN1' DER ((toASN1 $ bgpCert ^. #pubKey) [])
    pure (Validated bgpCert, BGPSecPayload {..})
  where
    bgpToEE ValidatedCert { aki = mAki, .. } =
        ValidatedEECert
            { aki = maybe (AKI (unSKI ski)) id mAki, .. }
            

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

isWithinValidityPeriod :: WithValidityPeriod a => Now -> a -> Bool
isWithinValidityPeriod (Now now) a = 
    let ValidityPeriod {..} = getValidityPeriod a
    in notBefore <= now && now < notAfter


-- | Full structural validation including multiple-location check.
-- Use in contexts (e.g. TopDown) where the full 'Located' object is available.
prevalidate :: Located ParsedRpkiObject -> PureValidatorT ValidatedRpkiObject
prevalidate located@(Located _ rpkiObject) = do
    validateObjectLocations located
    prevalidateObject rpkiObject


-- | Self-contained structural validation without a location context.
-- Used at object-save time (when only one URL is known) and wherever
-- constructing a 'Located' wrapper is unnecessary overhead.
prevalidateObject :: ParsedRpkiObject -> PureValidatorT ValidatedRpkiObject
prevalidateObject rpkiObject = do
    case rpkiObject of
        CerRO ca    -> validateCaCertStructure ca
        CrlRO crl   -> validateCrlStructure crl
        MftRO mft   -> validateCmsStructure mft >> validateMftStructure mft
        RoaRO roa   -> validateCmsStructure roa
        GbrRO gbr   -> validateCmsStructure gbr
        AspaRO aspa -> validateCmsStructure aspa >> validateAspaContent aspa
        SplRO spl   -> validateCmsStructure spl
        BgpRO bgp   -> validateBgpCertStructure bgp
        RscRO rsc   -> validateCmsStructure rsc
    pure $ toValidatedRpkiObject rpkiObject


-- | Convert a fully-parsed 'ParsedRpkiObject' to its minimized post-prevalidation
-- representation.  Drops all fields that are constant-after-validation
-- (CMS version, digest-algorithm OIDs, duplicate content-type, SignerInfo SID)
-- and extracts the few surviving fields (signing-time, CMS signature,
-- raw signed-attributes bytes, cert public-key, extensions, etc.).
toValidatedRpkiObject :: ParsedRpkiObject -> ValidatedRpkiObject
toValidatedRpkiObject = \case
    CerRO ca    -> CerRO  $ extractCert ca
    CrlRO crl   -> CrlRO  crl
    MftRO mft   -> MftRO  $ extractCMSObject mft
    RoaRO roa   -> RoaRO  $ extractCMSObject roa
    GbrRO gbr   -> GbrRO  $ extractCMSObject gbr
    AspaRO aspa -> AspaRO $ extractCMSObject aspa
    SplRO spl   -> SplRO  $ extractCMSObject spl
    BgpRO bgp   -> BgpRO  $ extractCert bgp
    RscRO rsc   -> RscRO  $ extractCMSObject rsc


extractCert :: (WithHash c, WithSKI c, WithAKI c, WithRawResourceCertificate c) => c -> ValidatedCert t
extractCert c =
    let rc        = getRawCert c
        cws       = certX509 rc
        cert      = cwsX509certificate cws
        (nb, na)  = certValidity cert
    in ValidatedCert
        { resources  = rc ^. #resources
        , pubKey     = certPubKey cert
        , serial     = Serial $ certSerial cert
        , validity   = ValidityPeriod (newInstant nb) (newInstant na)
        , extensions = getExts cert
        , encoded    = cwsEncoded cws
        , signature  = cwsSignature cws
        , sigAlg     = cwsSignatureAlgorithm cws
        , hash       = getHash c        
        , ski        = getSKI c
        , aki        = getAKI c
        }    

extractEECert :: EECerObject -> ValidatedEECert
extractEECert EECerObject { ski, aki, certificate } =
    let rc        = getRawCert certificate
        cws       = certX509 rc
        cert      = cwsX509certificate cws
        (nb, na)  = certValidity cert
    in ValidatedEECert
        { ski
        , aki
        , resources  = rc ^. #resources
        , pubKey     = certPubKey cert
        , serial     = Serial $ certSerial cert
        , validity   = ValidityPeriod (newInstant nb) (newInstant na)
        , extensions = getExts cert
        , encoded    = cwsEncoded cws
        , signature  = cwsSignature cws
        , sigAlg     = cwsSignatureAlgorithm cws
        }

extractCMSObject :: CMSBasedObject a -> ValidatedCMSObject a
extractCMSObject CMSBasedObject { hash, cmsPayload } =
    let CMS SignedObject { soContent = SignedData { scEncapContentInfo, scCertificate, scSignerInfos } } = cmsPayload
        SignerInfos { signature = cmsSignature, signedAttrs } = scSignerInfos
        SignedAttributes attrs signedAttrsBS = signedAttrs

        -- It will be there, it's already validated         
        [signingTime] = [ newInstant dt | SigningTime dt _ <- attrs ]

        eeCert      = extractEECert scCertificate
        content     = cContent scEncapContentInfo
    in ValidatedCMSObject { hash, content, eeCert, signingTime, cmsSignature, signedAttrsBS }

-- | Validate self-contained structural properties of a CA certificate.
validateCaCertStructure :: CaCerObject -> PureValidatorT ()
validateCaCertStructure ca@CaCerObject { ski } = do
    let certWS = getCertWithSignature ca
    validateCertX509Structure certWS
    validateSKIMatchesPublicKey ski certWS


-- | Validate self-contained structural properties of a BGP security certificate.
validateBgpCertStructure :: BgpCerObject -> PureValidatorT ()
validateBgpCertStructure bgp@BgpCerObject { ski } = do
    let certWS = getCertWithSignature bgp
    validateCertX509Structure certWS
    let pubKey = certPubKey $ cwsX509certificate certWS
    case pubKey of
        PubKeyEC _ -> pure ()
        _          -> vError $ InvalidPublicKey "BGPsec certificate must use an EC public key"
    validateSKIMatchesPublicKey ski certWS


-- | Validate the CMS envelope structure common to all signed objects.
validateCmsStructure :: CMSBasedObject a -> PureValidatorT ()
validateCmsStructure cmsObject = do
    let cms@(CMS SignedObject { soContent = sd }) = cmsPayload cmsObject
    let SignedData { scVersion, scSignerInfos, scEncapContentInfo, scCertificate } = sd

    -- RFC 6488 §2.1.1: SignedData.version must be 3
    let CMSVersion v = scVersion
    unless (v == 3) $ vError $ InvalidCMSVersion v

    let SignerInfos { siVersion, siSid, signedAttrs } = scSignerInfos

    -- RFC 6488 §2.1.6.1: SignerInfo.version must be 3
    let CMSVersion sv = siVersion
    unless (sv == 3) $ vError $ InvalidSignerInfoVersion sv

    -- Validate each signed attribute
    let SignedAttributes attrs _ = signedAttrs

    -- https://datatracker.ietf.org/doc/html/rfc9589#name-updates-to-rfc-6488
    -- There must be exactly three signed attributes: contentType, signingTime, and messageDigest.
    when (Prelude.null [ () | ContentTypeAttr _ <- attrs ]) $     
        vError ContentTypeAttrMissing
    
    when (Prelude.null [ () | SigningTime _ _ <- attrs ]) $ 
        vError SigningTimeMissing

    when (Prelude.null [ () | MessageDigest _ <- attrs ]) $
        vError MessageDigestMissing 

    -- And no other signed attributes are allowed. 
    -- If any unknown attributes are present, it's an error.
    for_ attrs $ \case        
        MessageDigest _ -> pure ()
        SigningTime _ _ -> pure ()
        ContentTypeAttr ct ->
            unless (ct == eContentType scEncapContentInfo) $
                vError EECertContentTypeMismatch        
        BinarySigningTime _    -> vError BinarySigningTimePresent        
        UnknownAttribute oid _ -> vError $ UnexpectedSignedAttribute oid        

    -- SignerInfo SID must equal the EE certificate's SKI
    let ski@(SKI (KI skiBytes))  = getSKI scCertificate
    let SignerIdentifier siBytes = siSid
    unless (skiBytes == siBytes) $ vError EECertSKIMismatch

    -- Validate the embedded EE certificate
    validateCertX509Structure (getCertWithSignature scCertificate)
    validateSKIMatchesPublicKey ski (getCertWithSignature scCertificate)

    -- Signature algorithm in the EE certificate has to be
    -- exactly the same as in the signed attributes
    let eeCert = getEEResourceCert $ unCMS cms
    let certWSign = getCertWithSignature eeCert
    let SignatureAlgorithmIdentifier eeCertSigAlg = certWSign ^. #cwsSignatureAlgorithm
    let attributeSigAlg = certSignatureAlg $ certWSign ^. #cwsX509certificate

    -- That can be a problem:
    -- http://sobornost.net/~job/arin-manifest-issue-2020.08.12.txt
    -- Correct behaviour is to request exact match here.
    unless (eeCertSigAlg == attributeSigAlg) $
        vPureError $
            CMSSignatureAlgorithmMismatch
                (Text.pack $ show eeCertSigAlg)
                (Text.pack $ show attributeSigAlg)    


-- | Validate manifest-specific structural invariants.
validateMftStructure :: MftObject -> PureValidatorT ()
validateMftStructure mft = do
    let Manifest { thisTime, nextTime, mftEntries } = getCMSContent (cmsPayload mft)

    -- thisUpdate must be strictly before nextUpdate
    when (thisTime >= nextTime) $
        vError $ NextUpdateTimeBeforeThisUpdateTime nextTime thisTime

    -- Every filename must use the permitted charset
    for_ mftEntries $ \MftPair { fileName } -> validateMftFileName fileName

    -- No two entries may share the same filename
    let fileNames    = map fileName mftEntries
    let dupFileNames = duplicatesOf fileNames
    unless (Prelude.null dupFileNames) $
        vError $ DuplicateManifestFilenames dupFileNames

    -- No two entries may share the same hash
    let hashes    = [ h | MftPair _ h <- mftEntries ]
    let dupHashes = duplicatesOf hashes
    unless (Prelude.null dupHashes) $
        vError $ NonUniqueManifestEntries
            [ (h, [ n | MftPair n h' <- mftEntries, h' == h ])
            | h <- dupHashes ]


-- | Validate ASPA content invariants.
validateAspaContent :: AspaObject -> PureValidatorT ()
validateAspaContent aspa = do     
    -- https://www.ietf.org/archive/id/draft-ietf-sidrops-aspa-profile-12.html#name-aspa-validation
    let AllResources ipv4 ipv6 asns = getRawCert (getEEResourceCert $ unCMS (cmsPayload aspa)) ^. #resources
    resourceSetMustBeEmpty ipv4 AspaIPv4Present
    resourceSetMustBeEmpty ipv6 AspaIPv6Present

    asnSet <- case asns of 
                Inherit -> vError AspaNoAsn
                RS s    -> pure s

    let Aspa {..} = getCMSContent (cmsPayload aspa)

    when (Set.null providers) $
        vError AspaNoAsn    

    unless ((AS customer) `IS.isInside` asnSet) $ 
        vError $ AspaAsNotOnEECert customer (IS.toList asnSet)

    when (customer `Set.member` providers) $
        vError $ AspaOverlappingCustomerProvider customer $ Set.toList providers        


-- | Validate CRL structural invariants.
validateCrlStructure :: CrlObject -> PureValidatorT ()
validateCrlStructure CrlObject { signCrl = SignCRL { thisUpdateTime, nextUpdateTime } } =
    case nextUpdateTime of
        Nothing         -> vError NextUpdateTimeNotSet
        Just nextUpdate ->
            when (nextUpdate <= thisUpdateTime) $
                vError $ NextUpdateTimeBeforeThisUpdateTime nextUpdate thisUpdateTime


-- | Validate X.509 certificate properties checkable without a parent certificate.
validateCertX509Structure :: CertificateWithSignature -> PureValidatorT ()
validateCertX509Structure CertificateWithSignature { cwsX509certificate = cert } = do
    -- notBefore must be strictly before notAfter        
    let (nb, na) = certValidity cert
    when (newInstant nb >= newInstant na) $
        vError CertValidityPeriodInvalid

    -- Serial number must be positive and within 20 octets (RFC 5280 §4.1.2.2)
    case makeSerial $ certSerial cert of 
        Left e  -> vError SerialNumberOutOfBounds
        Right _ -> pure ()            

    -- Public key constraints
    case certPubKey cert of
        PubKeyRSA k -> do
            when (RSA.public_size k /= 256) $   -- 256 bytes = 2048 bits
                vError $ InvalidPublicKey $
                    "RSA modulus must be 2048 bits, got "
                    <> Text.pack (show (RSA.public_size k * 8))
            when (RSA.public_e k /= 65537) $
                vError $ InvalidPublicKey $
                    "RSA public exponent must be 65537, got "
                    <> Text.pack (show (RSA.public_e k))
        PubKeyEC _ -> pure ()   -- EC keys are valid (BGPsec)
        _          -> vError $ InvalidPublicKey "Unsupported public key type for RPKI"


-- | Verify that the declared SKI equals SHA-1 of the subjectPublicKey bit-string value.
-- RFC 6487 §4.8.2 / RFC 5280 §4.2.1.2: SKI = SHA-1(subjectPublicKey BIT STRING value).
validateSKIMatchesPublicKey :: SKI   -- ^ raw bytes of the SKI extension KI
                            -> CertificateWithSignature
                            -> PureValidatorT ()
validateSKIMatchesPublicKey (SKI (KI skiBytes)) certWS = do
    let pubKey = certPubKey $ cwsX509certificate certWS
    -- toASN1 produces the SubjectPublicKeyInfo flat ASN1 sequence.
    -- The subjectPublicKey BIT STRING value is the preimage for the SHA-1 SKI.
    case [ bs | BitString (BitArray _ bs) <- toASN1 pubKey [] ] of
        (keyBytes : _) ->
            unless (BSS.fromShort skiBytes == SHA1.hash keyBytes) $
                vError SKINotMatchingPublicKey
        _ ->
            vError $ InvalidKI "Cannot extract subjectPublicKey bit string from public key"


resourceSetMustBeEmpty :: RSet (IntervalSet a) -> ValidationError -> PureValidatorT ()
resourceSetMustBeEmpty ips errConstructor = 
    case ips of 
        Inherit -> vError errConstructor
        RS i    -> unless (IS.null i) $ vError errConstructor  

-- | Return deduplicated elements that appear more than once in the input list.
duplicatesOf :: Ord a => [a] -> [a]
duplicatesOf =
    Map.keys . Map.filter (> 1) . Map.fromListWith (+) . map (, 1 :: Int)    