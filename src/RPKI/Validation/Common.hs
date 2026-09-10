{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module RPKI.Validation.Common where

import           Effectful
import           Control.Monad

import           Data.Foldable
import qualified Data.Set.NonEmpty                as NESet
import qualified Data.Set                         as Set
import qualified Data.Text                        as Text

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

class WithCertUris a where
    getCertUris :: a -> CertUris

instance WithCertUris (WellStructuredCert t) where
    getCertUris WellStructuredCert { certUris = uris } = uris

instance WithCertUris WellStructuredEECert where
    getCertUris WellStructuredEECert { certUris = uris } = uris

instance WithCertUris a => WithCertUris (Located a) where
    getCertUris = getCertUris . payload

validateMftFileName :: Validator es => Text.Text -> Eff es ()
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
validateMftLocation :: (Validator es, WithCertUris c, WithLocations c, WithLocations mft) =>
                        mft -> c -> Eff es ()
validateMftLocation mft parentCertficate = 
    case manifestUri $ getCertUris parentCertficate of
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
validateObjectLocations :: (Validator es, WithLocations a) => a -> Eff es ()
validateObjectLocations (getLocations -> Locations locSet) =    
    when (NESet.size locSet > 1) $ 
        vWarn $ ObjectHasMultipleLocations $ neSetToList locSet

-- | Check that CRL URL in the certificate is the same as the one 
-- the CRL was actually fetched from. 
-- 
checkCrlLocation :: (Validator es, WithLocations a, WithCertUris c) => a
                    -> c
                    -> Eff es ()
checkCrlLocation crl parentCertificate = 
    for_ (crlDPUri $ getCertUris parentCertificate) $ \crlDP -> do
        let crlLocations = getLocations crl
        when (Set.null $ NESet.filter ((crlDP ==) . getURL) $ unLocations crlLocations) $ 
            vError $ CRLOnDifferentLocation crlDP crlLocations

