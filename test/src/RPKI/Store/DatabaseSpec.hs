{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module RPKI.Store.DatabaseSpec where

import           Control.Monad
import qualified Data.ByteString                   as BS
import           Data.Foldable
import           Data.List                         as List
import qualified Data.Map.Strict                   as Map
import           Data.Maybe
import qualified Data.Text                         as Text

import           System.Directory
import           System.IO.Temp

import           Test.QuickCheck.Arbitrary.Generic
import           Test.Tasty
import qualified Test.Tasty.HUnit                  as HU
import qualified Test.Tasty.QuickCheck             as QC

import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Orphans
import           RPKI.Repository
import           RPKI.Store.Base.LMDB
import           RPKI.Store.Base.Map               as M
import           RPKI.Store.Base.MultiMap          as MM
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import           RPKI.Store.Data
import           RPKI.Store.Database
import           RPKI.Store.Repository
import           RPKI.Version
import           RPKI.Time

import           RPKI.Store.Base.LMDB              (LmdbEnv)
import           RPKI.Store.Util

import           RPKI.RepositorySpec
import GHC.Generics (Generic)




storeGroup :: TestTree
storeGroup = testGroup "LMDB storage tests"
    [
        objectStoreGroup,
        validationResultStoreGroup,
        repositoryStoreGroup
    ]

objectStoreGroup :: TestTree
objectStoreGroup = withDB $ \io -> testGroup "Object storage test"
    [
        HU.testCase "Should insert and get back" (should_insert_and_get_all_back_from_object_store io)        
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

    extracted <- roTx objectStore $ \tx -> getAll tx objectStore
    HU.assertEqual "Not the same objects" (sortOn getHash extracted) (sortOn getHash ros')
    
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
                        (MftRO mft1, MftRO mft2) -> getMftNumber mft1 == getMftNumber mft2
                        _ -> False

        compareLatestMfts objectStore ros a = do
            mftLatest <- roTx objectStore $ \tx -> findLatestMftByAKI tx objectStore a         
            
            let mftLatest' = listToMaybe $ sortOn (negate . getMftNumber)
                    [ mft | MftRO mft <- ros, getAKI mft == Just a ]
                
            HU.assertEqual "Not the same manifests" mftLatest mftLatest'


should_insert_and_get_all_back_from_validation_result_store :: IO ((FilePath, LmdbEnv), DB LmdbStorage) -> HU.Assertion
should_insert_and_get_all_back_from_validation_result_store io = do  
    (_, DB {..}) <- io
    vrs :: [VResult] <- forM [1 :: Int .. 100] $ const $ QC.generate arbitrary      

    world <- getWorldVerion =<< createDynamicState

    rwTx resultStore $ \tx -> for_ vrs $ \vr -> putVResult tx resultStore world vr
    vrs' <- roTx resultStore $ \tx -> allVResults tx resultStore world

    HU.assertEqual "Not the same VResult" (sort vrs) (sort vrs')


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


replaceAKI :: AKI -> RpkiObject -> RpkiObject
replaceAKI a = go 
    where
        go (CerRO c) = CerRO $ withContent c (\_ w -> withMeta w (\_ _ -> Just a))
        go (CrlRO c) = CrlRO $ withContent c (\_ w -> withMeta w (\_ _ -> a))
        go (MftRO c) = MftRO $ withContent c (\_ cms -> mapCms cms)
        go (RoaRO c) = RoaRO $ withContent c (\_ cms -> mapCms cms)
        go (GbrRO c) = GbrRO $ withContent c (\_ cms -> mapCms cms)

        mapCms :: CMS a -> CMS a
        mapCms (CMS so) = CMS $ so { soContent = sc { scCertificate = ee' } }
            where 
                ee = scCertificate sc
                sc = soContent so
                ee' = withMeta ee (\_ _ -> a)        
