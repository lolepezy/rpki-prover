{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module RPKI.Validation.Common where

import           Control.Monad

import           Control.Lens
import           Data.Foldable
import qualified Data.Set.NonEmpty                as NESet
import qualified Data.Set                         as Set
import qualified Data.Map.Strict                  as Map
import qualified Data.Text                        as Text

import qualified Data.ByteString.Short            as BSS

import           Data.X509
import           Data.ASN1.Types                  (ASN1(..), ASN1Object(..))
import           Data.ASN1.BitArray               (BitArray(..))
import qualified Data.X509                        as X509
import qualified Crypto.PubKey.RSA.Types          as RSA
import qualified Crypto.Hash.SHA1                 as SHA1

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Parse.Parse
import           RPKI.Resources.Resources
import           RPKI.Resources.Types
import qualified RPKI.Resources.IntervalContainers as IS
import           RPKI.Time                        (newInstant)
import qualified RPKI.Util as U

-- Validated and ValidatedRpkiObject are defined in RPKI.Domain and re-exported
-- from there so that both Store and Validation layers can use them.
createVerifiedResources :: WithResources c => c -> VerifiedRS PrefixesAndAsns
createVerifiedResources c = 
    VerifiedRS $ toPrefixesAndAsns $ getResources c

class WithCertExtensions a where
    getCertExtensions :: a -> [X509.ExtensionRaw]

instance WithCertExtensions CertificateWithSignature where
    getCertExtensions = getExtsSign

instance WithCertExtensions RawResourceCertificate where
    getCertExtensions = getCertExtensions . certX509

instance WithCertExtensions ResourceCertificate where
    getCertExtensions = getCertExtensions . getRawCert

instance WithCertExtensions CaCerObject where
    getCertExtensions = getCertExtensions . getRawCert

instance WithCertExtensions EECerObject where
    getCertExtensions = getCertExtensions . getRawCert

instance WithCertExtensions BgpCerObject where
    getCertExtensions = getCertExtensions . getRawCert

instance WithCertExtensions a => WithCertExtensions (Located a) where
    getCertExtensions = getCertExtensions . payload

instance WithCertExtensions (ValidatedCert t) where
    getCertExtensions ValidatedCert {..} = extensions

instance WithCertExtensions ValidatedEECert where
    getCertExtensions ValidatedEECert { extensions = exts } = exts

validateMftFileName :: Monad m => Text.Text -> ValidatorT m ()
validateMftFileName filename =                
    case Text.splitOn "." filename of 
        [ mainName, extension ] -> do                    
            unless (isSupportedExtension $ Text.toLower extension) $ 
                vError $ BadFileNameOnMFT filename 
                            ("Unsupported filename extension " <> extension)

            unless (Text.all U.isValidFileNameCharacter mainName) $ do 
                let badChars = Text.filter (not . U.isValidFileNameCharacter) mainName
                vError $ BadFileNameOnMFT filename 
                            ("Unsupported characters in filename: '" <> badChars <> "'")

        _ -> 
            vError $ BadFileNameOnMFT filename 
                        "Filename doesn't have exactly one DOT"      

-- TODO Is there a more reliable way to find it?
findCrlOnMft :: MftObject -> [MftPair]
findCrlOnMft mft = filter (\(MftPair name _) -> ".crl" `Text.isSuffixOf` name) $
    mftEntries $ getCMSContent $ cmsPayload mft


-- | Check that manifest URL in the certificate is the same as the one 
-- the manifest was actually fetched from.
validateMftLocation :: (WithCertExtensions c, Monad m, WithLocations c, WithLocations mft) =>
                        mft -> c -> ValidatorT m ()
validateMftLocation mft parentCertficate = 
    case getManifestUriExt $ getCertExtensions parentCertficate of
        Nothing     -> vError NoMFTSIA
        Just mftSIA -> do 
            unless (".mft" `Text.isSuffixOf` (unURI mftSIA)) $ 
                vWarn $ MFTBadSIA mftSIA
            unless ("rsync://" `Text.isPrefixOf` (unURI mftSIA)) $ 
                vWarn $ MFTBadSIA mftSIA                
            let mftLocations = getLocations mft
            when (Set.null $ NESet.filter ((mftSIA ==) . getURL) $ unLocations mftLocations) $ 
                vError $ MFTOnDifferentLocation mftSIA mftLocations


-- | Validate that the object has only one location: if not, 
-- it's generally is a warning, not really an error.
validateObjectLocations :: (WithLocations a, Monad m) => a -> ValidatorT m ()
validateObjectLocations (getLocations -> Locations locSet) =    
    when (NESet.size locSet > 1) $ 
        vWarn $ ObjectHasMultipleLocations $ neSetToList locSet

-- | Check that CRL URL in the certificate is the same as the one 
-- the CRL was actually fetched from. 
-- 
checkCrlLocation :: (Monad m, WithLocations a, WithCertExtensions c) => a
                    -> c
                    -> ValidatorT m ()
checkCrlLocation crl parentCertificate = 
    for_ (getCrlDistributionPointExt $ getCertExtensions parentCertificate) $ \crlDP -> do
        let crlLocations = getLocations crl
        when (Set.null $ NESet.filter ((crlDP ==) . getURL) $ unLocations crlLocations) $ 
            vError $ CRLOnDifferentLocation crlDP crlLocations


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
    when (null [ () | ContentTypeAttr _ <- attrs ]) $     
        vError ContentTypeAttrMissing
    
    when (null [ () | SigningTime _ _ <- attrs ]) $ 
        vError SigningTimeMissing

    when (null [ () | MessageDigest _ <- attrs ]) $     
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
    unless (null dupFileNames) $
        vError $ DuplicateManifestFilenames dupFileNames

    -- No two entries may share the same hash
    let hashes    = [ h | MftPair _ h <- mftEntries ]
    let dupHashes = duplicatesOf hashes
    unless (null dupHashes) $
        vError $ NonUniqueManifestEntries
            [ (h, [ n | MftPair n h' <- mftEntries, h' == h ])
            | h <- dupHashes ]


-- | Validate ASPA content invariants.
validateAspaContent :: AspaObject -> PureValidatorT ()
validateAspaContent aspa = do 
    let a = getCMSContent $ cmsPayload aspa
    when (Set.null $ providers $ getCMSContent (cmsPayload aspa)) $
        vError AspaNoAsn    

    -- https://www.ietf.org/archive/id/draft-ietf-sidrops-aspa-profile-12.html#name-aspa-validation
    let AllResources ipv4 ipv6 asns = getRawCert (getEEResourceCert $ unCMS (cmsPayload aspa)) ^. #resources
    resourceSetMustBeEmpty ipv4 AspaIPv4Present
    resourceSetMustBeEmpty ipv6 AspaIPv6Present

    asnSet <- case asns of 
                Inherit -> vError AspaNoAsn
                RS s    -> pure s

    let Aspa {..} = getCMSContent (cmsPayload aspa)

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