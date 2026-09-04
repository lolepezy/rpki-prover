{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE OverloadedStrings    #-}

module RPKI.Validation.ObjectValidation where
    
import           Control.Monad

import           Control.Lens
import           Data.Generics.Product.Typed

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Short            as BSS
import qualified Data.Text                        as Text
import           Data.Foldable (for_)
import           Data.Maybe (isJust, isNothing)
import qualified Data.Set                         as Set
import qualified Data.Map.Strict                  as Map

import qualified Crypto.PubKey.RSA.Types          as RSA
import qualified Crypto.Hash.SHA1                 as SHA1
import qualified Crypto.Hash.SHA256               as SHA256

import           Data.X509
import           Data.X509.Validation               hiding (InvalidSignature)
import           Data.ASN1.Types
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


type WithIssuerKey p = (WithPubKey p, WithSKI p)
type CertIssuer p    = (WithIssuerKey p, OfCertType p 'CACert)
type CaParent p      = (CertIssuer p, WithResources p)

type CertCore c       = (WithAKI c, WithSerial c, WithValidityPeriod c)
type SignedCertCore c = (CertCore c, WithSignMaterial c)

extractExtensions :: WithRawResourceCertificate c => c -> [ExtensionRaw]
extractExtensions = getExts . cwsX509certificate . getCertWithSignature

-- Resource certificates must carry at least one of IP/AS resources,
-- and each present resource extension must be critical.
-- https://www.rfc-editor.org/rfc/rfc6487#section-4.8.10
-- https://www.rfc-editor.org/rfc/rfc6487#section-4.8.11
validateResourceExtensionsPresenceAndCriticality :: [ExtensionRaw] -> PureValidatorT ()
validateResourceExtensionsPresenceAndCriticality extensions = do
    let ipExt = extRawVal extensions id_pe_ipAddrBlocks
    let asExt = extRawVal extensions id_pe_autonomousSysIds

    when (isNothing ipExt && isNothing asExt) $
        vPureError MissingIPOrASResourcesExtension

    for_ [ipExt, asExt] $ \case
        Just ExtensionRaw { extRawOID, extRawCritical = False } ->
            vPureError $ CertificateExtensionMustBeCritical extRawOID
        _ -> pure ()

-- A helper for profile clauses that require an extension to be present and non-critical.
-- https://www.rfc-editor.org/rfc/rfc6487#section-4.8.6
-- https://www.rfc-editor.org/rfc/rfc6487#section-4.8.7
validateRequiredNonCriticalExtension :: [ExtensionRaw] -> OID -> PureValidatorT BS.ByteString
validateRequiredNonCriticalExtension extensions oid =
    case extRawVal extensions oid of
        Nothing -> vPureError $ MissingRequiredCertificateExtension oid
        Just ExtensionRaw { extRawCritical = True } ->
            vPureError $ CertificateExtensionMustBeNonCritical oid
        Just ExtensionRaw { extRawContent = bs }
            | BS.null bs -> vPureError $ CertBrokenExtension oid bs
            | otherwise  -> pure bs

-- AIA must contain an id-ad-caIssuers URI, and rsync is mandatory.
-- https://www.rfc-editor.org/rfc/rfc6487#section-4.8.7
validateAiaCaIssuersUri :: [ExtensionRaw] -> PureValidatorT URI
validateAiaCaIssuersUri extensions = do
    aia <- validateRequiredNonCriticalExtension extensions id_pe_aia
    case extractSiaValue aia id_ad_caIssuers >>= either (const Nothing) Just . extractURI of
        Nothing -> vPureError $ CertBrokenExtension id_pe_aia aia
        Just uri@(URI url)
            | "rsync://" `Text.isPrefixOf` url -> pure uri
            | otherwise                        -> vPureError $ UnknownUriType uri

-- CRLDP must be present and carry a URI with rsync access.
-- https://www.rfc-editor.org/rfc/rfc6487#section-4.8.6
validatecrlDPUri :: [ExtensionRaw] -> PureValidatorT URI
validatecrlDPUri extensions = do
    crlDP <- validateRequiredNonCriticalExtension extensions id_ce_CRLDistributionPoints
    case extractCrlDistributionPoint crlDP of
        Nothing -> vPureError $ CertBrokenExtension id_ce_CRLDistributionPoints crlDP
        Just uri@(URI url)
            | "rsync://" `Text.isPrefixOf` url -> pure uri
            | otherwise                         -> vPureError $ UnknownUriType uri

getAiaCaIssuersUriExt :: [ExtensionRaw] -> Maybe URI
getAiaCaIssuersUriExt exts =
    extVal exts id_pe_aia >>= (\aia -> extractSiaValue aia id_ad_caIssuers >>= either (const Nothing) Just . extractURI)

deriveCertUrisFromExtensions :: [ExtensionRaw] -> CertUris
deriveCertUrisFromExtensions extensions =
    CertUris {
        aiaCaIssuersUri = getAiaCaIssuersUriExt extensions,
        crlDPUri = getCrlDistributionPointExt extensions,
        repositoryUri = getRepositoryUriExt extensions,
        manifestUri = getManifestUriExt extensions,
        rrdpNotifyUri = getRrdpNotifyUriExt extensions
    }

validateDerivedCertUris :: CertUris -> PureValidatorT ()
validateDerivedCertUris CertUris {..} = do
    for_ aiaCaIssuersUri $ \uri ->
        unless (U.isRsyncURI uri) $ vPureError $ UnknownUriType uri

    for_ crlDPUri $ \uri ->
        unless (U.isRsyncURI uri) $ vPureError $ UnknownUriType uri

    for_ repositoryUri $ \uri ->
        unless (U.isRsyncURI uri) $ vPureError $ UnknownUriType uri

    for_ manifestUri $ \uri ->
        unless (U.isRsyncURI uri) $ vPureError $ UnknownUriType uri

    for_ rrdpNotifyUri $ \uri ->
        unless (U.isRrdpURI uri) $ vPureError $ UnknownUriType uri

-- Child certificates must point to issuer material via AIA and CRLDP.
-- https://www.rfc-editor.org/rfc/rfc6487#section-7.2
validateRequiredParentPointerExtensions :: [ExtensionRaw] -> PureValidatorT ()
validateRequiredParentPointerExtensions extensions = do
    void $ validateAiaCaIssuersUri extensions
    void $ validatecrlDPUri extensions

validateCaCertExtensions :: [ExtensionRaw] -> PureValidatorT ()
validateCaCertExtensions extensions = do
    validateCaBasicConstraint extensions
    validatePolicyExtension extensions
    validateResourceExtensionsPresenceAndCriticality extensions
    validateNoUnknownCriticalExtensions extensions

-- https://datatracker.ietf.org/doc/html/rfc6487#section-4.8.9
validateEeCertExtensions :: [ExtensionRaw] -> PureValidatorT ()
validateEeCertExtensions extensions = do
    noBasicContraint extensions
    validatePolicyExtension extensions
    validateResourceExtensionsPresenceAndCriticality extensions
    validateNoUnknownCriticalExtensions extensions

-- https://datatracker.ietf.org/doc/html/rfc8209#section-3.1.3
validateBgpCertExtensions :: [ExtensionRaw] -> PureValidatorT ()
validateBgpCertExtensions = validateEeCertExtensions


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
validateTACert :: TAL -> RpkiURL -> ParsedRpkiObject -> PureValidatorT WellStructuredCaCert
validateTACert tal u (CerRO taCert) = do
    let spki = getSubjectPublicKeyInfo taCert
    let talSPKI = SPKI $ publicKeyInfo tal
    unless (talSPKI == spki) $ vPureError $ SPKIMismatch talSPKI spki
    validateTaCertAKI taCert u
    signatureCheck $ validateSignMaterial taCert taCert
    validateCaCertExtensions $ extractExtensions taCert
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
chooseTaCert :: WellStructuredCaCert -> WellStructuredCaCert -> PureValidatorT WellStructuredCaCert
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
validateResourceCert :: forall child parent .
    ( SignedCertCore child
    , CertIssuer parent
    ) =>
    Now ->
    child ->    
    parent ->
    Validated CrlObject ->    
    PureValidatorT (Validated child)
validateResourceCert now cert parentCert vcrl = do
    void $ validateObjectValidityPeriod cert now

    signatureCheck $ validateSignMaterial cert parentCert
    when (isRevoked cert vcrl) $ 
        vPureError RevokedResourceCertificate                

    unless (correctSkiAki cert parentCert) $    
        vPureError $ AKIIsNotEqualsToParentSKI (getAKI cert) (getSKI parentCert)

    pure $ Validated cert
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
    ( WithResources child
    , CaParent parent
    ) =>
    ValidationRFC ->
    Maybe (VerifiedRS PrefixesAndAsns) ->        
    child ->
    parent ->
    PureValidatorT (VerifiedRS PrefixesAndAsns, Maybe (Overclaiming PrefixesAndAsns))
validateResources validationRFC verifiedResources childCert parentCert =
    validateChildParentResources
        validationRFC
        (getResources childCert)
        (getResources parentCert)
        verifiedResources


validateBgpCert ::
    forall bgpCert parent.
    ( SignedCertCore bgpCert
    , WithPubKey bgpCert
    , WithSKI bgpCert
    , WithResources bgpCert
    , bgpCert `OfCertType` BGPCert
    , CertIssuer parent
    ) =>
    Now ->
    bgpCert ->
    parent ->
    Validated CrlObject ->
    PureValidatorT (Validated bgpCert, BGPSecPayload)
validateBgpCert now bgpCert parentCert validCrl = do
    -- Validate BGP certificate according to 
    -- https://www.rfc-editor.org/rfc/rfc8209.html#section-3.3    
    void $ validateResourceCert now bgpCert parentCert validCrl

    -- Must be some ASNs
    bgpSecAsns <- validateBgpCertAsns $ getResources bgpCert

    let bgpSecSki = getSKI bgpCert

    -- https://www.rfc-editor.org/rfc/rfc8208#section-3.1    
    let bgpSecSpki = getSubjectPublicKeyInfo bgpCert
    pure (Validated bgpCert, BGPSecPayload {..})

    

-- | Validate CRL object with the parent certificate
validateCrl ::
    WithIssuerKey parent =>
    Now ->
    CrlObject ->
    parent ->
    PureValidatorT (Validated CrlObject)
validateCrl now crlObject@CrlObject{..} parentCert = do
    let SignCRL{..} = signCrl
    signatureCheck $ validateSignMaterial crlObject parentCert
    when (toAKI (getSKI parentCert) /= aki) $
        vPureError $ CRL_AKI_DifferentFromCertSKI (getSKI parentCert) aki    
    validateUpdateTimes now thisUpdateTime nextUpdateTime
    pure $ Validated crlObject


validateMft ::
    CaParent parent =>
    ValidationRFC ->
    Now ->
    WellStructuredMft ->
    parent ->
    Validated CrlObject ->
    Maybe (VerifiedRS PrefixesAndAsns) ->
    PureValidatorT (Validated WellStructuredMft)
validateMft validationRFC now mft parentCert crl verifiedResources = do        
    let Manifest{..} = mft.content
    validateUpdateTimes now thisTime nextTime        
    validateCms validationRFC now mft parentCert crl verifiedResources

    let AllResources ipv4 ipv6 asns = getResources mft
    verifyInherit ipv4
    verifyInherit ipv6
    verifyInherit asns

    pure $ Validated mft

  where
    verifyInherit = \case
        RS _    -> vPureError ResourceSetMustBeInherit
        Inherit -> pure ()    

validateRoa ::
    CaParent parent =>
    ValidationRFC ->
    Now ->
    WellStructuredRoa ->
    parent ->
    Validated CrlObject ->
    Maybe (VerifiedRS PrefixesAndAsns) ->
    PureValidatorT (Validated WellStructuredRoa)
validateRoa validationRFC now roa parentCert crl verifiedResources = do      
    validateCms validationRFC now roa parentCert crl verifiedResources    
    checkResources roa.content
    
    pure $ Validated roa
  where
    checkResources (VrpsPerAs asn v4s v6s) = do 
        let checkerV4 = validatedPrefixInRS @Ipv4Prefix verifiedResources
        let checkerV6 = validatedPrefixInRS @Ipv6Prefix verifiedResources
        
        for_ v4s $ \(Vrp4 prefix maxLength) -> do
            checkerV4 prefix (RoaPrefixIsOutsideOfResourceSet (Ipv4P prefix))
            when (ipv4PrefixLen prefix > maxLength) $
                vPureError $ RoaPrefixLenghtsIsBiggerThanMaxLength (Vrp asn (Ipv4P prefix) maxLength)
        for_ v6s $ \(Vrp6 prefix maxLength) -> do
            checkerV6 prefix (RoaPrefixIsOutsideOfResourceSet (Ipv6P prefix))
            when (ipv6PrefixLen prefix > maxLength) $
                vPureError $ RoaPrefixLenghtsIsBiggerThanMaxLength (Vrp asn (Ipv6P prefix) maxLength)

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
    CaParent parent =>
    ValidationRFC ->
    Now ->
    WellStructuredSpl ->
    parent ->
    Validated CrlObject ->
    Maybe (VerifiedRS PrefixesAndAsns) ->
    PureValidatorT (Validated WellStructuredSpl)
validateSpl validationRFC now spl parentCert crl verifiedResources = do
    validateCms validationRFC now spl parentCert crl verifiedResources

    let SplPayload asn _ = spl.content
    for_ verifiedResources $ \(VerifiedRS vrs) -> do
        let asns = vrs ^. typed
        unless (isInside (AS asn) asns) $
            vPureError $
                SplAsnNotInResourceSet asn (IS.toList asns)

    let AllResources ipv4 ipv6 _ = getResources spl
    resourceSetMustBeEmpty ipv4 (SplNotIpResources (ipToList Ipv4P ipv4))
    resourceSetMustBeEmpty ipv6 (SplNotIpResources (ipToList Ipv6P ipv6))
    pure $ Validated spl
  where
    ipToList f = \case
        Inherit -> []
        RS s -> map f $ IS.toList s


validateGbr ::
    CaParent parent =>
    ValidationRFC ->
    Now ->
    WellStructuredGbr ->
    parent ->
    Validated CrlObject ->
    Maybe (VerifiedRS PrefixesAndAsns) ->
    PureValidatorT (Validated WellStructuredGbr)
validateGbr validationRFC now gbr parentCert crl verifiedResources = do
    validateCms validationRFC now gbr parentCert crl verifiedResources
    let Gbr vcardBS = gbr.content
    case parseVCard $ toNormalBS vcardBS of
        Left e -> vPureError $ InvalidVCardFormatInGbr e
        Right (_, Just warnings) -> vPureWarning $ InvalidVCardFormatInGbr warnings
        Right _ -> pure ()
    pure $ Validated gbr


validateRsc ::
    CaParent parent =>
    ValidationRFC ->
    Now ->
    WellStructuredRsc ->
    parent ->
    Validated CrlObject ->
    Maybe (VerifiedRS PrefixesAndAsns) ->
    PureValidatorT (Validated WellStructuredRsc)
validateRsc validationRFC now rsc parentCert crl verifiedResources = do
    validateCms validationRFC now rsc parentCert crl verifiedResources
    let rsc' = rsc.content
    let eeCert = toPrefixesAndAsns $ getResources rsc
    validateNested (rsc' ^. #rscResources) eeCert
    pure $ Validated rsc

validateAspa ::
    CaParent parent =>
    ValidationRFC ->
    Now ->
    WellStructuredAspa ->
    parent ->
    Validated CrlObject ->
    Maybe (VerifiedRS PrefixesAndAsns) ->
    PureValidatorT (Validated WellStructuredAspa)
validateAspa validationRFC now aspa parentCert crl verifiedResources = do    
    validateCms validationRFC now aspa parentCert crl verifiedResources 

    validateAspaCore (getResources aspa) aspa.content
    
    pure $ Validated aspa
    

validateCms ::
    CaParent parent =>
    ValidationRFC ->
    Now ->
    WellStructuredCms payload ->
    parent ->
    Validated CrlObject ->
    Maybe (VerifiedRS PrefixesAndAsns) ->
    PureValidatorT ()
validateCms validationRFC now cms parentCert crl verifiedResources = do
    void $ validateResourceCert now cms.eeCert parentCert crl
    void $ validateResources validationRFC verifiedResources cms.eeCert parentCert


validateParsedCms ::
    forall payload parent.
    CaParent parent =>
    ValidationRFC ->
    Now ->
    CMS payload ->
    parent ->
    Validated CrlObject ->
    Maybe (VerifiedRS PrefixesAndAsns) ->
    PureValidatorT ()
validateParsedCms validationRFC now cms parentCert crl verifiedResources = do
    let eeCert = getEEResourceCert cms
    signatureCheck $ validateCMSSignature cms
    void $ validateResourceCert now eeCert parentCert crl
    void $ validateResources validationRFC verifiedResources eeCert parentCert


validateUpdateTimes :: Now -> Instant -> Instant -> PureValidatorT ()
validateUpdateTimes (Now now) thisUpdateTime nextUpdateTime = do
    when (thisUpdateTime >= now) $ vPureError $ ThisUpdateTimeIsInTheFuture {..}
    when (nextUpdateTime < now)  $ vPureError $ NextUpdateTimeIsInThePast {..}
    validateUpdateTimesOrder thisUpdateTime nextUpdateTime


validateAIA ::
    WellStructuredCaCert ->
    Located WellStructuredCaCert ->
    PureValidatorT ()
validateAIA cert parentCert = do
    let locations = getLocations parentCert
    -- AIA caIssuers must identify the immediate superior certificate location.
    -- https://www.rfc-editor.org/rfc/rfc6487#section-4.8.7
    URI aiaUrl <- case cert.certUris.aiaCaIssuersUri of
        Nothing -> vPureError $ MissingRequiredCertificateExtension id_pe_aia
        Just uri
            | U.isRsyncURI uri -> pure uri
            | otherwise        -> vPureError $ UnknownUriType uri
    let parentUrls = locationsToList locations
    unless (aiaUrl `elem` parentUrls) $
        vPureWarning $ AIANotSameAsParentLocation aiaUrl locations  

-- | Check if CMS is on the revocation list
isRevoked :: WithSerial c => c -> Validated CrlObject -> Bool
isRevoked (getSerial -> serial) (Validated CrlObject {..}) = 
    Set.member serial revokedSerials
  where
    SignCRL{..} = signCrl

signatureCheck :: SignatureVerification -> PureValidatorT ()
signatureCheck sv = case sv of
    SignatureFailed e -> vPureError $ InvalidSignature $ convert $ show e
    SignaturePass     -> pure ()

validateUpdateTimesOrder :: Instant -> Instant -> PureValidatorT ()
validateUpdateTimesOrder thisUpdateTime nextUpdateTime =
    when (nextUpdateTime <= thisUpdateTime) $
        vError $ NextUpdateTimeBeforeThisUpdateTime nextUpdateTime thisUpdateTime

validateAspaCore :: AllResources -> Aspa -> PureValidatorT ()
validateAspaCore resources Aspa { customer, providers } = do
    -- https://www.ietf.org/archive/id/draft-ietf-sidrops-aspa-profile-12.html#name-aspa-validation
    let AllResources ipv4 ipv6 asns = resources
    resourceSetMustBeEmpty ipv4 AspaIPv4Present
    resourceSetMustBeEmpty ipv6 AspaIPv6Present

    asnSet <- case asns of
                Inherit -> vError AspaNoAsn
                RS s    -> pure s

    when (Set.null providers) $
        vError AspaNoProviders

    unless ((AS customer) `IS.isInside` asnSet) $
        vError $ AspaAsNotOnEECert customer (IS.toList asnSet)

    when (customer `Set.member` providers) $
        vError $ AspaOverlappingCustomerProvider customer $ Set.toList providers
    
    when ((ASN 0) `Set.member` providers && Set.size providers > 1) $
        vError $ AspaAsZeoAndNonZero $ Set.toList providers


validateBgpCertAsns :: AllResources -> PureValidatorT [ASN]
validateBgpCertAsns (AllResources _ _ asns) =
    case asns of
        Inherit -> vError BGPCertBrokenASNs
        RS i
            | IS.null i -> vError BGPCertBrokenASNs
            | otherwise -> pure $ unwrapAsns $ IS.toList i


validateCmsEeSignatureConsistency :: CMS a -> PureValidatorT ()
validateCmsEeSignatureConsistency cms = do
    -- EE cert should sign the CMS
    signatureCheck $ validateCMSSignature cms

    -- Signature algorithm in the EE certificate has to be
    -- exactly the same as in the signed attributes
    let eeCert = getEEResourceCert cms
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


{- | Self-contained structural validation without a location context.
Used at object-save time, when only one URL is known and there's no 
information about relashionships of this object with the other ones.
-}
prevalidateObject :: ParsedRpkiObject -> PureValidatorT WellStructuredRpkiObject
prevalidateObject rpkiObject = do
    case rpkiObject of
        CerRO ca -> do
            validateCaCertStructure ca
            pure $ CerRO $ extractCert ca
        CrlRO crl -> do
            validateCrlStructure crl
            pure $ CrlRO crl
        MftRO mft -> do
            validateCmsStructure mft
            validateMftStructure mft
            pure $ MftRO $ extractCMSObject mft
        RoaRO roa -> do
            validateCmsStructure roa
            pure $! RoaRO $ extractCMSObject roa
        GbrRO gbr -> do
            validateCmsStructure gbr
            pure $! GbrRO $ extractCMSObject gbr
        AspaRO aspa -> do
            validateCmsStructure aspa
            validateAspaContent aspa
            pure $! AspaRO $ extractCMSObject aspa
        SplRO spl -> do
            validateCmsStructure spl
            pure $! SplRO $ extractCMSObject spl
        BgpRO bgp -> do
            validateBgpCertStructure bgp
            pure $! BgpRO $ extractCert bgp
        RscRO rsc -> do
            validateCmsStructure rsc
            pure $! RscRO $ extractCMSObject rsc


extractCert :: (WithHash c, WithSKI c, WithAKI c, WithRawResourceCertificate c) => c -> WellStructuredCert t
extractCert c =
    let rc        = getRawCert c
        cws       = certX509 rc
        cert      = cwsX509certificate cws
        certUris  = deriveCertUrisFromExtensions $ getExts cert
        (nb, na)  = certValidity cert
    in WellStructuredCert
        { resources  = rc ^. #resources
        , pubKey     = certPubKey cert
        , serial     = Serial $ certSerial cert
        , validity   = ValidityPeriod (newInstant nb) (newInstant na)
        , certUris
        , encoded    = cwsEncoded cws
        , signature  = cwsSignature cws
        , sigAlg     = cwsSignatureAlgorithm cws
        , hash       = getHash c        
        , ski        = getSKI c
        , aki        = getAKI c
        }    

extractEECert :: EECerObject -> WellStructuredEECert
extractEECert EECerObject { ski, aki, certificate } =
    let rc        = getRawCert certificate
        cws       = certX509 rc
        cert      = cwsX509certificate cws
        certUris' = deriveCertUrisFromExtensions $ getExts cert
        (nb, na)  = certValidity cert
    in WellStructuredEECert
        { ski
        , aki
        , resources  = rc ^. #resources
        , pubKey     = certPubKey cert
        , serial     = Serial $ certSerial cert
        , validity   = ValidityPeriod (newInstant nb) (newInstant na)
        , certUris   = certUris'
        , encoded    = cwsEncoded cws
        , signature  = cwsSignature cws
        , sigAlg     = cwsSignatureAlgorithm cws
        }

extractCMSObject :: CMSBasedObject a -> WellStructuredCms a
extractCMSObject CMSBasedObject { hash, cmsPayload } =
    let CMS SignedObject { soContent = SignedData { scEncapContentInfo, scCertificate, scSignerInfos } } = cmsPayload
        SignerInfos { signature = cmsSignature, signedAttrs } = scSignerInfos
        SignedAttributes attrs signedAttrsBS = signedAttrs

        -- It is guaranteed by validateCmsStructure; keep total pattern here
        -- to satisfy -Wincomplete-uni-patterns.
        signingTime =
            case [ newInstant dt | SigningTime dt _ <- attrs ] of
                [st] -> st
                _    -> error "Invariant violated: expected exactly one SigningTime attribute"

        eeCert      = extractEECert scCertificate
        content     = cContent scEncapContentInfo
    in WellStructuredCms { hash, content, eeCert, signingTime, cmsSignature, signedAttrsBS }

-- | Validate self-contained structural properties of a CA certificate.
validateCaCertStructure :: CaCerObject -> PureValidatorT ()
validateCaCertStructure ca@CaCerObject { ski } = do
    let certWS = getCertWithSignature ca
    let extensions = extractExtensions ca
    let certUris = deriveCertUrisFromExtensions extensions
    validateCertX509Structure certWS
    validateSKIMatchesPublicKey ski certWS
    validateRequiredParentPointerExtensions extensions
    validateCaCertExtensions extensions
    validateDerivedCertUris certUris


-- | Validate self-contained structural properties of a BGP security certificate.
validateBgpCertStructure :: BgpCerObject -> PureValidatorT ()
validateBgpCertStructure bgp@BgpCerObject { ski } = do
    let certWS = getCertWithSignature bgp
    let extensions = extractExtensions bgp
    let certUris = deriveCertUrisFromExtensions extensions
    validateCertX509Structure certWS
    validateBgpCertExtensions extensions
    validateRequiredParentPointerExtensions extensions
    validateDerivedCertUris certUris
    let pubKey = certPubKey $ cwsX509certificate certWS
    case pubKey of
        PubKeyEC _ -> pure ()
        _          -> vError $ InvalidPublicKey "BGPsec certificate must use an EC public key"

    validateSKIMatchesPublicKey ski certWS

    let cwsX509 = cwsX509certificate $ getCertWithSignature bgp

    -- BGPsec router certificates must omit SIA.
    -- https://www.rfc-editor.org/rfc/rfc8209#section-3.1.3.3
    for_ (getSiaExt cwsX509) $ vError . BGPCertSIAPresent

    -- BGPsec router certificates must omit IP resources.
    -- https://www.rfc-editor.org/rfc/rfc8209#section-3.1.3.4
    when (isJust $ extRawVal extensions id_pe_ipAddrBlocks) $
        vError BGPCertIPv4Present

    -- BGPsec router certificates must include AS resources and keep it critical.
    -- https://www.rfc-editor.org/rfc/rfc8209#section-3.1.3.5
    case extRawVal extensions id_pe_autonomousSysIds of
        Nothing -> vError BGPCertBrokenASNs
        Just ExtensionRaw { extRawCritical = False } ->
            vError $ CertificateExtensionMustBeCritical id_pe_autonomousSysIds
        _ -> pure ()

    -- No IP resources in parsed resource set either.
    let AllResources ipv4 ipv6 _ = getResources bgp
    resourceSetMustBeEmpty ipv4 BGPCertIPv4Present
    resourceSetMustBeEmpty ipv6 BGPCertIPv6Present    
    
    -- Must be some ASNs
    void $ validateBgpCertAsns $ getResources bgp


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

    case [ md | MessageDigest md <- attrs ] of
        [messageDigest] -> do
            -- messageDigest must equal digest(eContent) computed with the declared digest algorithm.
            -- https://www.rfc-editor.org/rfc/rfc6488#section-2.1.6.4.2
            -- https://www.rfc-editor.org/rfc/rfc6488#section-3
            -- https://www.rfc-editor.org/rfc/rfc5652#section-5.4
            let DigestAlgorithmIdentifiers digestOids = scDigestAlgorithms sd
            digestOid <- case digestOids of
                [oid] -> pure oid
                _     -> vError $ UnsupportedHashAlgorithm $ Text.pack $ show digestOids

            expectedDigest <-
                if digestOid == id_sha256
                    then pure $ BSS.toShort $ SHA256.hash $ eContentBytes scEncapContentInfo
                    else vError $ UnsupportedHashAlgorithm $ Text.pack $ show digestOid

            unless (messageDigest == expectedDigest) $
                vError CMSMessageDigestMismatch
        _ -> vError MessageDigestMissing

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
    let eeExtensions = extractExtensions scCertificate
    validateRequiredParentPointerExtensions eeExtensions
    validateEeCertExtensions eeExtensions
    validateDerivedCertUris $ deriveCertUrisFromExtensions eeExtensions

    validateCmsEeSignatureConsistency cms


-- | Validate manifest-specific structural invariants.
validateMftStructure :: MftObject -> PureValidatorT ()
validateMftStructure mft = do
    let Manifest { thisTime, nextTime, mftEntries } = getCMSContent (cmsPayload mft)

    -- thisUpdate must be strictly before nextUpdate
    validateUpdateTimesOrder thisTime nextTime

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
    let payload = getCMSContent aspa.cmsPayload 
    validateAspaCore (getResources aspa) payload


-- | Validate CRL structural invariants.
validateCrlStructure :: CrlObject -> PureValidatorT ()
validateCrlStructure CrlObject { signCrl = SignCRL { thisUpdateTime, nextUpdateTime } } =    
    validateUpdateTimesOrder thisUpdateTime nextUpdateTime


-- | Validate X.509 certificate properties checkable without a parent certificate.
validateCertX509Structure :: CertificateWithSignature -> PureValidatorT ()
validateCertX509Structure CertificateWithSignature { cwsX509certificate = cert } = do
    -- notBefore must be strictly before notAfter        
    let (nb, na) = certValidity cert
    when (newInstant nb >= newInstant na) $
        vError CertValidityPeriodInvalid

    -- Serial number must be positive and within 20 octets (RFC 5280 §4.1.2.2)
    case makeSerial $ certSerial cert of 
        Left e  -> vError $ SerialNumberOutOfBounds $ U.convert e
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