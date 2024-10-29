{-# LANGUAGE MultiWayIf          #-}
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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
    deriving newtype (WithSKI, WithAKI, WithHash)

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
    let spki = getSubjectPublicKeyInfo $ cwsX509certificate $ getCertWithSignature taCert
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

--
-- Use the tiebreaker logic proposed by 
-- https://datatracker.ietf.org/doc/draft-spaghetti-sidrops-rpki-ta-tiebreaker/
--
-- Emit a warning when deciding to use the cached certificate 
-- instead of the fetched one.
-- 
chooseTaCert :: CaCerObject -> CaCerObject -> PureValidatorT CaCerObject
chooseTaCert cert cachedCert = do
    let validities = bimap Instant Instant . certValidity . cwsX509certificate . getCertWithSignature
    let (notBefore, notAfter) = validities cert
    let (cachedNotBefore, cachedNotAfter) = validities cachedCert
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
    
    signatureCheck $ validateCertSignature cert parentCert

    when (isRevoked cert vcrl) $ 
        vPureError RevokedResourceCertificate

    validateObjectValidityPeriod cert now    

    unless (correctSkiAki cert parentCert) $
        vPureError $ AKIIsNotEqualsToParentSKI (getAKI cert) (getSKI parentCert)

    Validated <$> validateResourceCertExtensions @_ @childCertType cert    
  where
    correctSkiAki c (getSKI -> SKI s) =
        maybe False (\(AKI a) -> a == s) $ getAKI c


validateObjectValidityPeriod :: WithValidityPeriod c => c -> Now -> PureValidatorT ()
validateObjectValidityPeriod c (Now now) = do 
    let (notBefore, notAfter) = getValidityPeriod c
    when (now < notBefore) $ 
        vPureError $ ObjectValidityIsInTheFuture notBefore notAfter
    when (now > notAfter) $ 
        vPureError $ ObjectIsExpired notBefore notAfter


validateResources ::
    (WithRawResourceCertificate child, 
     WithRawResourceCertificate parent,
     OfCertType parent 'CACert) =>
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
    forall child parent.
    ( WithRawResourceCertificate child
    , WithRawResourceCertificate parent
    , WithSKI parent
    , WithAKI child
    , WithSKI child
    , WithValidityPeriod child
    , WithSerial child
    , child `OfCertType` BGPCert
    , parent `OfCertType` CACert
    ) =>
    Now ->
    child ->
    parent ->
    Validated CrlObject ->
    PureValidatorT (Validated child, BGPSecPayload)
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
    let bgpSecSpki = getSubjectPublicKeyInfo cwsX509
    pure (Validated bgpCert, BGPSecPayload {..})


ipMustBeEmpty :: RSet (IntervalSet a) -> ValidationError -> PureValidatorT ()
ipMustBeEmpty ips errConstructor = 
    case ips of 
        Inherit -> vError errConstructor
        RS i    -> unless (IS.null i) $ vError errConstructor  
    

-- | Validate CRL object with the parent certificate
validateCrl ::    
    (WithRawResourceCertificate c, WithSKI c) =>
    Now ->
    CrlObject ->
    c ->
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
            ipMustBeEmpty ipv4 (SplNotIpResources (ipToList Ipv4P ipv4))
            ipMustBeEmpty ipv6 (SplNotIpResources (ipToList Ipv6P ipv6))

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
            let eeCert = toPrefixesAndAsns $ resources rc 
            validateNested (rsc' ^. #rscResources) eeCert            
            
    pure $ Validated rsc

validateAspa ::
    (WithRawResourceCertificate parent, WithSKI parent, OfCertType parent 'CACert) =>
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
            ipMustBeEmpty ipv4 AspaIPv4Present
            ipMustBeEmpty ipv6 AspaIPv6Present

            asnSet <- case asns of 
                        Inherit -> vError AspaNoAsn
                        RS s    -> pure s

            let Aspa {..} = getCMSContent aspaCms         

            unless ((AS customer) `IS.isInside` asnSet) $ 
                vError $ AspaAsNotOnEECert customer (IS.toList asnSet)

            when (customer `Set.member` providers) $
                vError $ AspaOverlappingCustomerProvider customer $ Set.toList providers
    
    pure $ Validated aspa
    


validateCms :: forall a c .
    (WithRawResourceCertificate c, 
    WithSKI c, 
    OfCertType c 'CACert) =>
    ValidationRFC ->
    Now ->
    CMS a ->
    c ->
    Validated CrlObject ->
    Maybe (VerifiedRS PrefixesAndAsns) ->
    (CMS a -> PureValidatorT ()) ->
    PureValidatorT ()
validateCms validationRFC now cms parentCert crl verifiedResources extraValidation = do
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
    , OfCertType parent 'CACert
    , OfCertType child childCertType) =>    
    child  ->
    Located parent ->
    PureValidatorT ()
validateAIA cert parentCert =    
    for_ (getSiaExt $ cwsX509certificate $ getCertWithSignature cert) $ \sia -> do 
        for_ (extractSiaValue sia id_pe_sia) $ \ext -> do             
            let locations = parentCert ^. #locations
            case extractURI ext of 
                Left e            -> vPureWarning $ BrokenUri (Text.pack $ show ext) e
                Right (URI siaUrl) -> do                     
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
