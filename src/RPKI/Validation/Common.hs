{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE RecordWildCards            #-}

module RPKI.Validation.Common where

import           Control.Monad

import           Control.Lens ((^.))
import           Data.Generics.Product.Typed

import           Data.Foldable
import qualified Data.Set.NonEmpty                as NESet
import qualified Data.List.NonEmpty               as NonEmpty
import qualified Data.Set                         as Set
import qualified Data.Text                        as Text

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Parse.Parse
import           RPKI.Resources.Resources
import           RPKI.Validation.Types
import           RPKI.Resources.Types
import qualified RPKI.Util as U
import           RPKI.Time  


createVerifiedResources :: CaCerObject -> VerifiedRS PrefixesAndAsns
createVerifiedResources c = 
    VerifiedRS $ toPrefixesAndAsns $ getRawCert c ^. typed

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


updateMfts :: AnMft -> Now -> MftsMeta -> MftsMeta 
updateMfts newMft (Now now) (MftsMeta mfts) = MftsMeta mfts'   
  where
    mfts' = NonEmpty.fromList 
        $ filterOutDefinitelyInvalid 
        $ sortedMfts 
        $ NonEmpty.toList mfts    

    -- insert new manifest while keeping the list sorted backwards by nextUpdateTime
    sortedMfts [] = [newMft]
    sortedMfts (mft: otherMfts) 
        | newMft `expiresLater` mft = newMft : mft : otherMfts
        | otherwise                 = mft : sortedMfts otherMfts
      
    expiresLater m1 m2 = m1 ^. #nextUpdate > m2 ^. #nextUpdate
    
    -- filter out all MFTs that are already expired and will never be valid,
    -- but keep at least one, so that the list is never empty and we don't
    -- get "no MFT" error instead of "there's a manifest but it's expired"
    filterOutDefinitelyInvalid = go (0 :: Int)
      where
        go _ [] = []
        go !n (mft: mfts_) = 
            if mft ^. #nextUpdate < now 
                then if n == 0 
                    then mft : go 1 mfts_ 
                    else go 0 mfts_ 
                else mft : go (n + 1) mfts_

pickMft :: MftsMeta -> Now -> [AnMft]
pickMft (MftsMeta mfts) (Now now) = 
    -- skip MFTs that are not valid yet
    filter (\m -> m ^. #thisUpdate <= now) 
    $ NonEmpty.toList mfts

newMftMeta :: ObjectKey -> MftObject -> AnMft
newMftMeta key mftObject = let 
        (thisUpdate, nextUpdate) = getValidityPeriod mftObject
    in AnMft {..}

newMfts :: ObjectKey -> MftObject -> MftsMeta
newMfts key mftObject = let 
        (thisUpdate, nextUpdate) = getValidityPeriod mftObject
    in MftsMeta (NonEmpty.singleton $ AnMft {..})

deleteMft :: ObjectKey -> MftsMeta -> Maybe MftsMeta
deleteMft key (MftsMeta mfts) =
    fmap MftsMeta $ NonEmpty.nonEmpty $ NonEmpty.filter (\m -> m ^. #key /= key) mfts    
    