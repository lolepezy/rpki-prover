{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes         #-}

module RPKI.Validation.ObjectValidation where
    
import           Control.Monad

import           Control.Lens
import           Data.Generics.Product.Typed

import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.Text                          as Text
import           Data.Foldable (for_)
import           Data.String.Interpolate.IsString

import qualified Data.Set                           as Set

import           Data.X509
import           Data.X509.Validation               hiding (InvalidSignature)
import           Data.ASN1.BitArray
import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Types
import           GHC.Generics

import           RPKI.AppMonad
import           RPKI.Config
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
import           RPKI.Resources.Resources


newtype Validated a = Validated a
    deriving stock (Show, Eq, Generic)

-- TODO That one needs to be refactored in such a way that certificate type is 
-- known after parsing the certificate on the type level.
validateResourceCertExtensions :: WithResourceCertificate c => c -> PureValidatorT c
validateResourceCertExtensions cert = do     
    let extensions = getExts $ cwsX509certificate $ getCertWithSignature cert

    -- Do not check for SKI, AKI and resource extensions -- they 
    -- are validated when parsing certificates and are crucial for 
    -- the whole tree validation.
    getCertificateType extensions >>= \case                        
        CACert -> do             
            validateCaBasicConstraint extensions
            validatePolicyExtension extensions                        
        BGPCert -> do             
            -- https://datatracker.ietf.org/doc/html/rfc8209#section-3.1.3
            noBasicContraint extensions            
        EECert  -> do 
            -- TODO Something else here?
            noBasicContraint extensions                        

    -- Same (or almost same?) for all types
    validateNoUnknownCriticalExtensions extensions            
    
    pure cert


getCertificateType :: [ExtensionRaw] -> PureValidatorT CertType
getCertificateType extensions =
    withExtension extensions id_ce_keyUsage $ \bs parsed ->             
        case parsed of 
            -- Bits position are from
            -- https://datatracker.ietf.org/doc/html/rfc5280#section-4.2.1.3
            -- 
            -- Which bits needs to set and where
            -- https://datatracker.ietf.org/doc/html/rfc6487#section-4.8.4
            [BitString ba@(BitArray 7 _)] -> do 
                unless (bitArrayGetBit ba 5) $ vPureError $ BrokenKeyUsage "keyCertSign bit is not set"
                unless (bitArrayGetBit ba 6) $ vPureError $ BrokenKeyUsage "cRLSign bit is not set"
                for_ [0..4] $ \bit -> 
                    when (bitArrayGetBit ba bit) $ vPureError $ BrokenKeyUsage 
                        [i|Bit #{bit} is set, only keyCertSign and cRLSign must be set.|]

                pure CACert            

            -- There must be only the `digitalSignature` bit 
            [BitString ba@(BitArray 1 _)] ->                     
                let badExtKU = vPureError $ CertBrokenExtension id_ce_extKeyUsage bs
                in case extVal extensions id_ce_extKeyUsage of
                    Nothing -> do                             
                        unless (bitArrayGetBit ba 0) $ vPureError $ BrokenKeyUsage "digitalSignature bit is not set"
                        pure EECert
                    Just bs1 
                        | BS.null bs1 -> badExtKU
                        | otherwise ->
                            case decodeASN1 DER (LBS.fromStrict bs1) of
                                Left _  -> badExtKU
                                Right [Start Sequence, OID oid, End Sequence]
                                    | oid == id_kp_bgpsecRouter -> pure BGPCert
                                    | otherwise -> badExtKU                                            
                                _ -> badExtKU
                
            _ -> vPureError $ UnknownCriticalCertificateExtension id_ce_keyUsage bs

-- https://datatracker.ietf.org/doc/html/rfc6487#section-4.8            
validateCaBasicConstraint :: [ExtensionRaw] -> PureValidatorT ()
validateCaBasicConstraint extensions = 
    withExtension extensions id_ce_basicConstraints $ \bs parsed ->
            case parsed of 
                [Start Sequence, Boolean _, End Sequence] -> pure ()                
                _ -> vPureError $ CertBrokenExtension id_ce_basicConstraints bs

noBasicContraint :: [ExtensionRaw] -> PureValidatorT ()
noBasicContraint extensions = 
    for_ (extVal extensions id_ce_basicConstraints) $ \bs -> 
        vPureError $ UnknownCriticalCertificateExtension id_ce_basicConstraints bs  

validatePolicyExtension :: [ExtensionRaw] -> PureValidatorT ()
validatePolicyExtension extensions = 
    withExtension extensions id_ce_certificatePolicies $ \bs parsed ->
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
validateNoUnknownCriticalExtensions extensions = do                        
    for_ extensions $ \ExtensionRaw {..} -> do 
        when (extRawCritical && extRawOID `notElem` allowedCriticalOIDs) $ 
            vPureError $ UnknownCriticalCertificateExtension extRawOID extRawContent

withExtension :: [ExtensionRaw]
            -> OID
            -> (BS.ByteString -> [ASN1] -> PureValidatorT r)
            -> PureValidatorT r
withExtension extensions oid f = do 
    case extVal extensions oid of
        Nothing -> vPureError $ MissingCriticalExtension oid
        Just bs 
            | BS.null bs -> vPureError $ MissingCriticalExtension oid
            | otherwise -> do 
                case decodeASN1 DER (LBS.fromStrict bs) of
                    Left _  -> vPureError $ CertBrokenExtension oid bs        
                    Right z -> f bs z
        

-- | Validated specifically the TA's self-signed certificate
-- 
validateTACert :: TAL -> RpkiURL -> RpkiObject -> PureValidatorT CerObject
validateTACert tal u (CerRO taCert) = do
    let spki = subjectPublicKeyInfo $ cwsX509certificate $ getCertWithSignature taCert
    unless (publicKeyInfo tal == spki) $ vPureError $ SPKIMismatch (publicKeyInfo tal) spki
    validateTaCertAKI taCert u
    signatureCheck $ validateCertSignature taCert taCert
    validateResourceCertExtensions taCert    
      
validateTACert _ _ _ = vPureError UnknownObjectAsTACert


validateTaCertAKI :: (WithAKI taCert, WithSKI taCert) 
                    => taCert -> RpkiURL -> PureValidatorT ()
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
validateTACertWithPreviousCert :: CerObject -> CerObject -> PureValidatorT CerObject
validateTACertWithPreviousCert cert previousCert = do
    let validities = bimap Instant Instant . certValidity . cwsX509certificate . getCertWithSignature
    let (before, after) = validities cert
    let (prevBefore, prevAfter) = validities previousCert
    if before < prevBefore || after < prevAfter 
        then do
            void $ vPureWarning $ TACertOlderThanPrevious {..}
            pure previousCert 
        else 
            pure cert

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
    ( WithResourceCertificate c
    , WithResourceCertificate parent
    , WithSKI parent
    , WithAKI c
    ) =>
    Now ->
    c ->    
    parent ->
    Validated CrlObject ->    
    PureValidatorT (Validated c)
validateResourceCert (Now now) cert parentCert vcrl = do
    let (before, after) = certValidity $ cwsX509certificate $ getCertWithSignature cert
    signatureCheck $ validateCertSignature cert parentCert

    when (isRevoked cert vcrl) $ 
        vPureError RevokedResourceCertificate

    when (now < Instant before) $ 
        vPureError $ CertificateIsInTheFuture (Instant before) (Instant after)

    when (now > Instant after) $ 
        vPureError $ CertificateIsExpired (Instant before) (Instant after)

    unless (correctSkiAki cert parentCert) $
        vPureError $ AKIIsNotEqualsToParentSKI (getAKI cert) (getSKI parentCert)

    void $ validateResourceCertExtensions cert
    pure $ Validated cert
  where
    correctSkiAki c (getSKI -> SKI s) =
        maybe False (\(AKI a) -> a == s) $ getAKI c


validateResources ::
    (WithResourceCertificate c, WithResourceCertificate parent) =>
    Maybe (VerifiedRS PrefixesAndAsns) ->
    c ->
    parent ->
    PureValidatorT (VerifiedRS PrefixesAndAsns)
validateResources
    verifiedResources
    (getRC -> ResourceCertificate cert)
    (getRC -> ResourceCertificate parentCert) =
        validateChildParentResources
            (validationRFC cert)
            ((polyRFC cert) ^. typed)
            ((polyRFC parentCert) ^. typed)
            verifiedResources

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

validateRsc ::
    (WithResourceCertificate c, WithSKI c) =>
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
            let ResourceCertificate rc = getRC $ getEEResourceCert $ unCMS rscCms
            let eeCert = toPrefixesAndAsns $ resources $ polyRFC rc 
            validateNested (rsc' ^. #rscResources) eeCert            
            
    pure $ Validated rsc

validateAspa ::
    (WithResourceCertificate c, WithSKI c) =>
    Now ->
    AspaObject ->
    c ->
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

validateCms ::
    (WithResourceCertificate c, WithSKI c) =>
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
            | otherwise -> pure nextUpdate

-- | Validate the thisUpdateTeim for objects that have it (MFT, CRLs)
validateThisUpdate :: Now -> Instant -> PureValidatorT Instant
validateThisUpdate (Now now) thisUpdateTime
    | thisUpdateTime >= now = vPureError $ ThisUpdateTimeIsInTheFuture thisUpdateTime now
    | otherwise = pure thisUpdateTime

-- | Check if CMS is on the revocation list
isRevoked :: WithResourceCertificate c => c -> Validated CrlObject -> Bool
isRevoked cert (Validated crlObject) =
    Set.member serial revokenSerials
  where
    SignCRL{..} = signCrl crlObject
    serial = getSerial cert

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
