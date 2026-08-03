{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module RPKI.Validation.Common where

import           Control.Monad

import           Control.Lens
import           Data.Generics.Product.Typed

import           Data.Foldable
import qualified Data.Set.NonEmpty                as NESet
import qualified Data.Set                         as Set
import qualified Data.Map.Strict                  as Map
import qualified Data.Text                        as Text
import           GHC.Generics

import qualified Data.ByteString.Short            as BSS
import           Data.Maybe                       (listToMaybe)

import           Data.ASN1.Types                  (ASN1(..), ASN1Object(..))
import           Data.ASN1.BitArray               (BitArray(..))
import           Data.X509                        (PubKey(..), certPubKey, certValidity, certSerial)
import qualified Crypto.PubKey.RSA.Types          as RSA
import qualified Crypto.Hash.SHA1                 as SHA1

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Parse.Parse
import           RPKI.Resources.Resources
import           RPKI.Resources.Types
import           RPKI.Time                        (newInstant)
import qualified RPKI.Util as U

-- Validated and ValidatedRpkiObject are defined in RPKI.Domain and re-exported
-- from there so that both Store and Validation layers can use them.
createVerifiedResources :: CaCerObject -> VerifiedRS PrefixesAndAsns
createVerifiedResources certificate = 
    VerifiedRS $ toPrefixesAndAsns $ getRawCert certificate ^. typed

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
validateMftLocation :: (WithRawResourceCertificate c, Monad m, WithLocations c, WithLocations mft) =>
                        mft -> c -> ValidatorT m ()
validateMftLocation mft parentCertficate = 
    case getManifestUri $ cwsX509certificate $ getCertWithSignature parentCertficate of
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
checkCrlLocation :: (Monad m, WithLocations a) => a
                    -> CertificateWithSignature
                    -> ValidatorT m ()
checkCrlLocation crl parentCertificate = 
    for_ (getCrlDistributionPoint $ cwsX509certificate parentCertificate) $ \crlDP -> do
        let crlLocations = getLocations crl
        when (Set.null $ NESet.filter ((crlDP ==) . getURL) $ unLocations crlLocations) $ 
            vError $ CRLOnDifferentLocation crlDP crlLocations


-- | Full structural validation including multiple-location check.
-- Use in contexts (e.g. TopDown) where the full 'Located' object is available.
prevalidate :: Monad m => Located RpkiObject -> ValidatorT m ValidatedRpkiObject
prevalidate located@(Located _ rpkiObject) = do
    validateObjectLocations located
    prevalidateObject rpkiObject


-- | Self-contained structural validation without a location context.
-- Used at object-save time (when only one URL is known) and wherever
-- constructing a 'Located' wrapper is unnecessary overhead.
prevalidateObject :: Monad m => RpkiObject -> ValidatorT m ValidatedRpkiObject
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


-- | Convert a fully-parsed 'RpkiObject' to its minimized post-prevalidation
-- representation.  Drops all fields that are constant-after-validation
-- (CMS version, digest-algorithm OIDs, duplicate content-type, SignerInfo SID)
-- and extracts the few surviving fields (signing-time, CMS signature,
-- raw signed-attributes bytes, cert public-key, extensions, etc.).
toValidatedRpkiObject :: RpkiObject -> ValidatedRpkiObject
toValidatedRpkiObject = \case
    CerRO ca    -> VCerRO  $ extractCaCert ca
    CrlRO crl   -> VCrlRO  crl
    MftRO mft   -> VMftRO  $ extractCMSObject mft
    RoaRO roa   -> VRoaRO  $ extractCMSObject roa
    GbrRO gbr   -> VGbrRO  $ extractCMSObject gbr
    AspaRO aspa -> VAspaRO $ extractCMSObject aspa
    SplRO spl   -> VSplRO  $ extractCMSObject spl
    BgpRO bgp   -> VBgpRO  $ extractBgpCert bgp
    RscRO rsc   -> VRscRO  $ extractCMSObject rsc

-- Internal helpers --------------------------------------------------------

extractCertFields :: (WithRawResourceCertificate a) => a -> SKI -> Maybe AKI -> Hash
                  -> ValidatedCaCert
extractCertFields certHolder certSki certAki certHash =
    let rc        = getRawCert certHolder
        cws       = certX509 rc
        cert      = cwsX509certificate cws
        (nb, na)  = certValidity cert
    in ValidatedCaCert
        { hash       = certHash
        , ski        = certSki
        , aki        = certAki
        , resources  = rc ^. #resources
        , pubKey     = certPubKey cert
        , serial     = Serial $ certSerial cert
        , validity   = ValidityPeriod (newInstant nb) (newInstant na)
        , extensions = getExts cert
        , encoded    = cwsEncoded cws
        , signature  = cwsSignature cws
        , sigAlg     = cwsSignatureAlgorithm cws
        }

extractCaCert :: CaCerObject -> ValidatedCaCert
extractCaCert ca@CaCerObject { hash, ski, aki } = extractCertFields ca ski aki hash

extractBgpCert :: BgpCerObject -> ValidatedBgpCert
extractBgpCert BgpCerObject { hash, ski, aki, certificate } =
    let rc        = getRawCert certificate
        cws       = certX509 rc
        cert      = cwsX509certificate cws
        (nb, na)  = certValidity cert
    in ValidatedBgpCert
        { hash
        , ski
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
    let CMS SignedObject { soContent = SignedData { scEncapContentInfo, scCertificate, scSignerInfos } }
              = cmsPayload
        SignerInfos { signature = cmsSignature, signedAttrs }
              = scSignerInfos
        SignedAttributes attrs signedAttrsBS
              = signedAttrs
        signingTime = listToMaybe [ newInstant dt | SigningTime dt _ <- attrs ]
        eeCert      = extractEECert scCertificate
        content     = cContent scEncapContentInfo
    in ValidatedCMSObject { hash, content, eeCert, signingTime, cmsSignature, signedAttrsBS }


-- | Validate self-contained structural properties of a CA certificate.
validateCaCertStructure :: Monad m => CaCerObject -> ValidatorT m ()
validateCaCertStructure ca@CaCerObject { ski = SKI (KI skiBytes) } = do
    let certWS = getCertWithSignature ca
    validateCertX509Structure certWS
    validateSKIMatchesPublicKey skiBytes certWS


-- | Validate self-contained structural properties of a BGP security certificate.
validateBgpCertStructure :: Monad m => BgpCerObject -> ValidatorT m ()
validateBgpCertStructure bgp@BgpCerObject { ski = SKI (KI skiBytes) } = do
    let certWS = getCertWithSignature bgp
    validateCertX509Structure certWS
    let pubKey = certPubKey $ cwsX509certificate certWS
    case pubKey of
        PubKeyEC _ -> pure ()
        _          -> vError $ InvalidPublicKey "BGPsec certificate must use an EC public key"
    validateSKIMatchesPublicKey skiBytes certWS


-- | Validate the CMS envelope structure common to all signed objects.
validateCmsStructure :: Monad m => CMSBasedObject a -> ValidatorT m ()
validateCmsStructure cmsObj = do
    let CMS SignedObject { soContent = sd } = cmsPayload cmsObj
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
    for_ attrs $ \case
        BinarySigningTime _    -> vError BinarySigningTimePresent
        UnknownAttribute oid _ -> vError $ UnexpectedSignedAttribute oid
        ContentTypeAttr ct     ->
            unless (ct == eContentType scEncapContentInfo) $
                vError EECertContentTypeMismatch
        _ -> pure ()

    -- SignerInfo SID must equal the EE certificate's SKI
    let SKI (KI skiBytes)        = getSKI scCertificate
    let SignerIdentifier siBytes  = siSid
    unless (skiBytes == siBytes) $ vError EECertSKIMismatch

    -- Validate the embedded EE certificate
    validateCertX509Structure (getCertWithSignature scCertificate)
    validateSKIMatchesPublicKey skiBytes (getCertWithSignature scCertificate)


-- | Validate manifest-specific structural invariants.
validateMftStructure :: Monad m => MftObject -> ValidatorT m ()
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
validateAspaContent :: Monad m => AspaObject -> ValidatorT m ()
validateAspaContent aspa =
    when (Set.null $ providers $ getCMSContent (cmsPayload aspa)) $
        vError AspaNoAsn


-- | Validate CRL structural invariants.
validateCrlStructure :: Monad m => CrlObject -> ValidatorT m ()
validateCrlStructure CrlObject { signCrl = SignCRL { thisUpdateTime, nextUpdateTime } } =
    case nextUpdateTime of
        Nothing         -> vError NextUpdateTimeNotSet
        Just nextUpdate ->
            when (nextUpdate <= thisUpdateTime) $
                vError $ NextUpdateTimeBeforeThisUpdateTime nextUpdate thisUpdateTime


-- | Validate X.509 certificate properties checkable without a parent certificate.
validateCertX509Structure :: Monad m => CertificateWithSignature -> ValidatorT m ()
validateCertX509Structure CertificateWithSignature { cwsX509certificate = cert } = do
    -- notBefore must be strictly before notAfter
    let (nb, na) = certValidity cert
    when (newInstant nb >= newInstant na) $
        vError CertValidityPeriodInvalid

    -- Serial number must be positive and within 20 octets (RFC 5280 §4.1.2.2)
    let serial = certSerial cert
    when (serial <= 0 || serial >= 2 ^ (160 :: Int)) $
        vError SerialNumberOutOfBounds

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
validateSKIMatchesPublicKey :: Monad m
                            => BSS.ShortByteString   -- ^ raw bytes of the SKI extension KI
                            -> CertificateWithSignature
                            -> ValidatorT m ()
validateSKIMatchesPublicKey skiBytes certWS = do
    let pubKey = certPubKey $ cwsX509certificate certWS
    -- toASN1 produces the SubjectPublicKeyInfo flat ASN1 sequence.
    -- The subjectPublicKey BIT STRING value is the preimage for the SHA-1 SKI.
    case [ bs | BitString (BitArray _ bs) <- toASN1 pubKey [] ] of
        (keyBytes : _) ->
            unless (BSS.fromShort skiBytes == SHA1.hash keyBytes) $
                vError SKINotMatchingPublicKey
        _ ->
            vError $ InvalidKI "Cannot extract subjectPublicKey bit string from public key"


-- | Return deduplicated elements that appear more than once in the input list.
duplicatesOf :: Ord a => [a] -> [a]
duplicatesOf =
    Map.keys . Map.filter (> 1) . Map.fromListWith (+) . map (, 1 :: Int)