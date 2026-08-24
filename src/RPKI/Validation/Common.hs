{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module RPKI.Validation.Common where

import           Control.Monad

import           Data.Foldable
import qualified Data.Set.NonEmpty                as NESet
import qualified Data.Set                         as Set
import qualified Data.Text                        as Text
import qualified Data.X509                        as X509

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Parse.Parse
import           RPKI.Resources.Resources
import           RPKI.Resources.Types

import qualified RPKI.Util as U

-- Validated and WellStructuredRpkiObject are defined in RPKI.Domain and re-exported
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
findCrlOnMft :: Manifest -> [MftPair]
findCrlOnMft mft = filter (\(MftPair name _) -> ".crl" `Text.isSuffixOf` name) $ mft.mftEntries 


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

