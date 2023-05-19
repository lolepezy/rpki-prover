{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE DerivingVia                #-}

module RPKI.Validation.Common where

import           Control.Concurrent.STM
import           Control.Monad.Except

import           Control.Lens
import           Data.Generics.Product.Typed
import           Data.Generics.Product.Fields
import           GHC.Generics (Generic)

import           Data.Foldable
import qualified Data.Set.NonEmpty                as NESet
import qualified Data.Set                         as Set
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text
import           Data.Tuple.Strict

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Parse.Parse
import           RPKI.Resources.Resources
import           RPKI.Resources.Types
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import           RPKI.Util                        (fmtLocations)


data ValidManifests'State = FillingUp | FetchingFromDB | Merged 
    deriving stock (Generic)

data ValidManifests = ValidManifests {
        state  :: ValidManifests'State,
        valids :: Map AKI Hash
    }
    deriving stock (Generic)

makeValidManifests :: ValidManifests 
makeValidManifests = ValidManifests FillingUp mempty

createVerifiedResources :: CaCerObject -> VerifiedRS PrefixesAndAsns
createVerifiedResources certificate = 
    VerifiedRS $ toPrefixesAndAsns $ getRawCert certificate ^. typed

allowedMftFileNameCharacters :: [Char]
allowedMftFileNameCharacters = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "-_"

validateMftFileName :: Monad m => Text.Text -> ValidatorT m ()
validateMftFileName filename =                
    case Text.splitOn "." filename of 
        [ mainName, extension ] -> do                    
            unless (isSupportedExtension $ Text.toLower extension) $ 
                vError $ BadFileNameOnMFT filename 
                            ("Unsupported filename extension " <> extension)

            unless (Text.all (`elem` allowedMftFileNameCharacters) mainName) $ do 
                let badChars = Text.filter (`notElem` allowedMftFileNameCharacters) mainName
                vError $ BadFileNameOnMFT filename 
                            ("Unsupported characters in filename: '" <> badChars <> "'")

        _somethingElse -> 
            vError $ BadFileNameOnMFT filename 
                        "Filename doesn't have exactly one DOT"      

findLatestMft :: (MonadIO m, HasField' "objectStore" s1 (RpkiObjectStore s), Storage s) =>
                TVar s1 -> AKI -> m (Maybe (Located MftObject))
findLatestMft database childrenAki = liftIO $ do 
    objectStore' <- (^. #objectStore) <$> readTVarIO database
    roTx objectStore' $ \tx -> 
        findLatestMftByAKI tx objectStore' childrenAki

-- findLatestCachedValidMft :: (MonadIO m, HasField' "objectStore" s1 (RpkiObjectStore s), Storage s) =>
--                 TVar s1 -> AKI -> m (Maybe (Located MftObject))
-- findLatestCachedValidMft database childrenAki = liftIO $ do 
--     objectStore' <- (^. #objectStore) <$> readTVarIO database
--     roTx objectStore' $ \tx -> 
--         getLatestValidMftByAKI tx objectStore' childrenAki


-- 
-- Reusable piece for the cases when a "fetch" has failed so we are falling 
-- back to a latest valid cached manifest for this CA
-- https://datatracker.ietf.org/doc/html/rfc9286#section-6.6
-- 
-- This function doesn't make much sense by itself, it's just a chunk of code 
-- reusable by both TopDown and BottomUp.
-- 
tryLatestValidCachedManifest :: (MonadIO m, Storage s, WithHash mft) =>
        AppContext s
    -> (Located MftObject -> AKI -> Locations -> ValidatorT m b)
    -> Maybe mft
    -> TVar ValidManifests
    -> AKI
    -> Locations
    -> AppError
    -> ValidatorT m b
tryLatestValidCachedManifest AppContext{..} useManifest latestMft validManifests childrenAki certLocations e = do
    db <- liftIO $ readTVarIO database
    -- z <- roTx objectStore' $ \tx -> 
    --         getLatestValidMftByAKI tx objectStore' childrenAki
    validMfts <- loadValidManifests db validManifests    
    case Map.lookup childrenAki validMfts of
        Nothing   -> throwError e
        Just hash -> do                        
            z <- liftIO $ roTx db $ \tx -> getByHash tx db hash
            latestValidMft <- case z of 
                                Just (Located loc (MftRO mft)) -> pure $ Located loc mft
                                _                              -> throwError e                
            let mftLoc = fmtLocations $ getLocations latestValidMft            
            case latestMft of 
                Nothing -> do 
                    appWarn e      
                    logWarn logger [i|Failed to process manifest #{mftLoc}: #{e}, will try previous valid version.|]
                    useManifest latestValidMft childrenAki certLocations                                
                Just latestMft'
                    | getHash latestMft' == getHash latestValidMft 
                        -- it doesn't make sense to try the same manifest again
                        -- just re-trow the error
                        -> throwError e
                    | otherwise -> do 
                        appWarn e                                    
                        logWarn logger $ [i|Failed to process latest manifest #{mftLoc}: #{e},|] <> 
                                         [i|] fetch is invalid, will try latest valid one from previous fetch(es).|]
                        useManifest latestValidMft childrenAki certLocations


loadValidManifests :: (MonadIO m, Storage s) => 
                    DB s -> TVar ValidManifests -> m (Map AKI Hash) 
loadValidManifests db validManifests = liftIO $ do 
    join $ atomically $ do 
        vm <- readTVar validManifests
        case vm ^. #state of 
            FetchingFromDB  -> retry
            Merged          -> pure $ pure $ vm ^. #valids
            FillingUp       -> do 
                writeTVar validManifests $ vm & #state .~ FetchingFromDB
                pure $ do 
                    validMfts <- roTx db $ \tx -> getLatestValidMfts tx db
                    atomically $ do                                             
                        let v' = Map.unionWith (\a _ -> a) (vm ^. #valids) validMfts
                        writeTVar validManifests $ ValidManifests Merged v'
                        pure v'                                                                                                


-- TODO Is there a more reliable way to find it?
findCrlOnMft :: MftObject -> [T2 Text.Text Hash]
findCrlOnMft mft = filter (\(T2 name _) -> ".crl" `Text.isSuffixOf` name) $
    mftEntries $ getCMSContent $ cmsPayload mft


-- | Check that manifest URL in the certificate is the same as the one 
-- the manifest was actually fetched from.
validateMftLocation :: (WithRawResourceCertificate c, Monad m, WithLocations c, WithLocations mft) =>
                        mft -> c -> ValidatorT m ()
validateMftLocation mft certficate = 
    case getManifestUri $ cwsX509certificate $ getCertWithSignature certficate of
        Nothing     -> vError $ NoMFTSIA $ getLocations certficate
        Just mftSIA -> do 
            let mftLocations = getLocations mft
            when (Set.null $ NESet.filter ((mftSIA ==) . getURL) $ unLocations mftLocations) $ 
                vWarn $ MFTOnDifferentLocation mftSIA mftLocations                    


-- | Validate that the object has only one location: if not, 
-- it's generally is a warning, not really an error.
validateObjectLocations :: (WithLocations a, Monad m) => a -> ValidatorT m ()
validateObjectLocations (getLocations -> locs@(Locations locSet)) =
    inSubObjectVScope (locationsToText locs) $ 
        when (NESet.size locSet > 1) $ 
            vWarn $ ObjectHasMultipleLocations $ neSetToList locSet

-- | Check that CRL URL in the certificate is the same as the one 
-- the CRL was actually fetched from. 
-- 
checkCrlLocation :: (Monad m, WithLocations a) => a
                    -> CertificateWithSignature
                    -> ValidatorT m ()
checkCrlLocation crl eeCert = 
    for_ (getCrlDistributionPoint $ cwsX509certificate eeCert) $ \crlDP -> do
        let crlLocations = getLocations crl
        when (Set.null $ NESet.filter ((crlDP ==) . getURL) $ unLocations crlLocations) $ 
            vError $ CRLOnDifferentLocation crlDP crlLocations

