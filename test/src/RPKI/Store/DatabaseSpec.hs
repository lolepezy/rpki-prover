{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module RPKI.Store.DatabaseSpec where

import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString                   as BS
import           Data.Foldable
import qualified Data.List                         as List
import qualified Data.Map.Strict                   as Map
import           Data.Maybe
import           Data.Ord
import qualified Data.Text                         as Text

import           System.Directory
import           System.IO.Temp

import           Test.QuickCheck.Arbitrary.Generic
import           Test.Tasty
import qualified Test.Tasty.HUnit                  as HU
import qualified Test.Tasty.QuickCheck             as QC


import           RPKI.AppMonad
import           RPKI.AppState
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Repository
import           RPKI.Store.Base.LMDB
import           RPKI.Store.Base.Map               as M
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import           RPKI.Store.Repository
import           RPKI.Store.Util
import           RPKI.Time

import           RPKI.RepositorySpec
import RPKI.Parse.Parse


storeGroup :: TestTree
storeGroup = testGroup "LMDB storage tests"
    [
        objectStoreGroup,
        validationResultStoreGroup,
        repositoryStoreGroup,
        txGroup
    ]

objectStoreGroup :: TestTree
objectStoreGroup = withDB $ \io -> testGroup "Object storage test"
    [
        HU.testCase "Should insert and get back" (should_insert_and_get_all_back_from_object_store io),        
        HU.testCase "Should order manifests accoring to their dates" (should_order_manifests_properly io)
    ]

validationResultStoreGroup :: TestTree
validationResultStoreGroup = withDB $ \io -> testGroup "Validation result storage test"
    [
        HU.testCase "Should insert and get back" (should_insert_and_get_all_back_from_validation_result_store io)        
    ]

repositoryStoreGroup :: TestTree
repositoryStoreGroup = withDB $ \io -> testGroup "Repository LMDB storage test"
    [
        HU.testCase "Should insert and get a repository" (should_insert_and_get_all_back_from_repository_store io)
        -- HU.testCase "Should use repository change set properly" (should_read_create_change_set_and_apply_repository_store io)
    ]

txGroup :: TestTree
txGroup = withDB $ \io -> testGroup "App transaction test"
    [
        HU.testCase "Should rollback App transactions properly" (shouldRollbackAppTx io)        
    ]



should_insert_and_get_all_back_from_object_store :: IO ((FilePath, LmdbEnv), DB LmdbStorage) -> HU.Assertion
should_insert_and_get_all_back_from_object_store io = do  
    (_, DB {..}) <- io
    aki1 :: AKI <- QC.generate arbitrary
    aki2 :: AKI <- QC.generate arbitrary
    ros :: [RpkiObject] <- removeMftNumberDuplicates <$> generateSome

    let (firstHalf, secondHalf) = List.splitAt (List.length ros `div` 2) ros

    let ros1 = List.map (replaceAKI aki1) firstHalf
    let ros2 = List.map (replaceAKI aki2) secondHalf
    let ros' = ros1 <> ros2 

    Now now <- thisInstant 

    rwTx objectStore $ \tx -> 
        for_ ros' $ \ro -> 
            putObject tx objectStore (toStorableObject ro) (instantToVersion now)

    allObjects <- roTx objectStore $ \tx -> getAll tx objectStore
    HU.assertEqual 
        "Not the same objects" 
        (List.sortOn getHash allObjects) 
        (List.sortOn getHash ros')
    
    compareLatestMfts objectStore ros1 aki1
    compareLatestMfts objectStore ros2 aki2  
    
    let (toDelete, toKeep) = List.splitAt (List.length ros1 `div` 2) ros1

    rwTx objectStore $ \tx -> 
        forM_ toDelete $ \ro -> 
            deleteObject tx objectStore (getHash ro)

    compareLatestMfts objectStore toKeep aki1
    compareLatestMfts objectStore ros2 aki2  

    where
        removeMftNumberDuplicates = List.nubBy sameMftNumber
            where 
                sameMftNumber ro1 ro2 = 
                    case (ro1, ro2) of
                        (MftRO mft1, MftRO mft2) -> getMftTimingMark mft1 == getMftTimingMark mft2
                        _ -> False

        compareLatestMfts objectStore ros a = do
            mftLatest <- roTx objectStore $ \tx -> findLatestMftByAKI tx objectStore a         
            
            let mftLatest' = listToMaybe $ List.sortOn (Down . getMftTimingMark)
                    [ mft | MftRO mft <- ros, getAKI mft == Just a ]
                
            HU.assertEqual "Not the same manifests" mftLatest mftLatest'


should_order_manifests_properly :: IO ((FilePath, LmdbEnv), DB LmdbStorage) -> HU.Assertion
should_order_manifests_properly io = do  
    (_, DB {..}) <- io
    Right mft1 <- readObjectFromFile "./test/data/afrinic_mft1.mft"
    Right mft2 <- readObjectFromFile "./test/data/afrinic_mft2.mft"

    Now now <- thisInstant 
    let worldVersion = instantToVersion now

    rwTx objectStore $ \tx -> do        
            putObject tx objectStore (toStorableObject mft1) worldVersion
            putObject tx objectStore (toStorableObject mft2) worldVersion

    -- they have the same AKIs
    let Just aki1 = getAKI mft1
    Just mftLatest <- roTx objectStore $ \tx -> findLatestMftByAKI tx objectStore aki1

    HU.assertEqual "Not the same manifests" (MftRO mftLatest) mft2


should_insert_and_get_all_back_from_validation_result_store :: IO ((FilePath, LmdbEnv), DB LmdbStorage) -> HU.Assertion
should_insert_and_get_all_back_from_validation_result_store io = do  
    (_, DB {..}) <- io
    vrs :: Validations <- QC.generate arbitrary      

    world <- getWorldVerionIO =<< newAppState

    rwTx validationsStore $ \tx -> putValidations tx validationsStore world vrs
    vrs' <- roTx validationsStore $ \tx -> validationsForVersion tx validationsStore world

    HU.assertEqual "Not the same Validations" (Just vrs) vrs'


should_insert_and_get_all_back_from_repository_store :: IO ((FilePath, LmdbEnv), DB LmdbStorage) -> HU.Assertion
should_insert_and_get_all_back_from_repository_store io = do  
    (_, DB {..}) <- io

    taName1 <- TaName <$> QC.generate arbitrary

    let rsyncPPs = fromRsyncPPs repositoriesURIs 
    rrdpMap :: RrdpMap <- QC.generate arbitrary    

    let lastSuccess = Map.fromList [ (RrdpU u, FetchLastSuccess t) | 
            RrdpRepository { uri = u, status = FetchedAt t } <- Map.elems $ unRrdpMap rrdpMap ]

    let createdPPs = rsyncPPs <> PublicationPoints rrdpMap mempty (LastSuccededMap lastSuccess)

    storedPps1 <- roTx repositoryStore $ \tx -> 
                    getTaPublicationPoints tx repositoryStore taName1

    let changeSet1 = changeSet storedPps1 createdPPs

    rwTx repositoryStore $ \tx -> 
            applyChangeSet tx repositoryStore changeSet1 taName1

    storedPps2 <- roTx repositoryStore $ \tx -> 
                    getTaPublicationPoints tx repositoryStore taName1

    HU.assertEqual "Not the same publication points" createdPPs storedPps2

    rsyncPPs2 <- fromRsyncPPs <$> QC.generate (QC.sublistOf repositoriesURIs)

    let RrdpMap rrdpsM = rrdpMap    
    keys <- QC.generate (QC.sublistOf $ Map.keys rrdpsM)
    let rrdpMap2 = RrdpMap $ Map.filterWithKey (\u _ -> u `elem` keys) rrdpsM

    let shrunkPPs = rsyncPPs2 <> PublicationPoints rrdpMap2 mempty mempty

    let changeSet2 = changeSet storedPps2 shrunkPPs

    rwTx repositoryStore $ \tx -> 
            applyChangeSet tx repositoryStore changeSet2 taName1

    storedPps3 <- roTx repositoryStore $ \tx -> 
                    getTaPublicationPoints tx repositoryStore taName1    

    HU.assertEqual "Not the same publication points after shrinking" shrunkPPs storedPps3

    
shouldRollbackAppTx :: IO ((FilePath, LmdbEnv), DB LmdbStorage) -> HU.Assertion
shouldRollbackAppTx io = do  
    ((_, env), DB {..}) <- io

    let storage' = LmdbStorage env
    z :: SMap "test" LmdbStorage Int String <- SMap storage' <$> createLmdbStore env

    void $ runValidatorT (vContext "bla") $ rwAppTx storage' $ \tx -> do
        liftIO $ M.put tx z 1 "aa"
        appError $ UnspecifiedE "Test" "Test problem"

    void $ runValidatorT (vContext "bla") $ rwAppTx storage' $ \tx ->
        liftIO $ M.put tx z 2 "bb"        

    let throwFromTx =
            void $ runValidatorT (vContext "bla") $ rwAppTx storage' $ \tx -> liftIO $ do
                    M.put tx z 3 "cc"        
                    throwIO RatioZeroDenominator

    Left (SomeException e) <- try throwFromTx    
    HU.assertEqual "Must be the right type of exception" 
            (fromException (toException e)) 
            (Just RatioZeroDenominator)

    void $ runValidatorT (vContext "bla") $ roAppTx storage' $ \tx -> liftIO $ do         
         v1 <- M.get tx z 1  
         HU.assertEqual "Must not be there" v1 Nothing
         v2 <- M.get tx z 2  
         HU.assertEqual "Must be rolled back by appError" v2 (Just "bb")
         v3 <- M.get tx z 3  
         HU.assertEqual "Must be rolled back by exception" v3 Nothing
    
-- should_report_multi_or_not_properly :: IO ((FilePath, LmdbEnv), DB LmdbStorage) -> HU.Assertion
-- should_report_multi_or_not_properly io = do  
--     (_, DB {..}) <- io
--     roTx objects objectStore
--     pure ()



generateSome :: Arbitrary a => IO [a]
generateSome = forM [1 :: Int .. 1000] $ const $ QC.generate arbitrary      

withDB :: (IO ((FilePath, LmdbEnv), DB LmdbStorage) -> TestTree) -> TestTree
withDB = withResource (makeLmdbStuff createDatabase) releaseLmdb


makeLmdbStuff :: (LmdbEnv -> IO b) -> IO ((FilePath, LmdbEnv), b)
makeLmdbStuff mkStore = do 
    dir <- createTempDirectory "/tmp" "lmdb-test"
    e <- mkLmdb dir 1000 1000 
    store <- mkStore e
    pure ((dir, e), store)

releaseLmdb :: ((FilePath, LmdbEnv), b) -> IO ()
releaseLmdb ((dir, LmdbEnv{..}), _) = do   
    closeLmdb nativeEnv
    removeDirectoryRecursive dir

readObjectFromFile :: FilePath -> IO (ParseResult RpkiObject)
readObjectFromFile path = do 
    bs <- BS.readFile path
    pure $! readObject (RsyncU $ RsyncURL $ URI $ Text.pack path) bs

replaceAKI :: AKI -> RpkiObject -> RpkiObject
replaceAKI a = go 
    where
        go (CerRO c) = CerRO $ c { aki = Just a }
        go (CrlRO c) = CrlRO $ c { aki = a }
        go (MftRO c) = MftRO $ c { cmsPayload = mapCms $ cmsPayload c }
        go (RoaRO c) = RoaRO $ c { cmsPayload = mapCms $ cmsPayload c }
        go (GbrRO c) = GbrRO $ c { cmsPayload = mapCms $ cmsPayload c }

        mapCms :: CMS a -> CMS a
        mapCms (CMS so) = CMS $ so { soContent = sc { scCertificate = ee' } }
            where 
                ee = scCertificate sc
                sc = soContent so
                ee' :: EECerObject = ee { aki = a :: AKI }

