{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NumericUnderscores        #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}

module RPKI.Store.DatabaseSpec where

import           Control.Monad
import qualified Data.ByteString                   as BS
import           Data.Foldable
import           Data.List                         as List
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
import           RPKI.Version
import           RPKI.Orphans
import           RPKI.Repository
import           RPKI.Store.Base.LMDB
import           RPKI.Store.Base.Map               as M
import           RPKI.Store.Base.MultiMap          as MM
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import           RPKI.Store.Data
import           RPKI.Store.Repository
import           RPKI.Store.Database

import           RPKI.Store.Base.LMDB              (LmdbEnv)

import           RPKI.Store.Util




storeGroup :: TestTree
storeGroup = testGroup "LMDB storage tests"
    [
        objectStoreGroup,
        validationResultStoreGroup
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
        -- HU.testCase "Should insert and get a repository" (should_insert_and_get_all_back_from_repository_store io),
        -- HU.testCase "Should use repository change set properly" (should_read_create_change_set_and_apply_repository_store io)
    ]



sizes :: Storage s =>
        RpkiObjectStore s ->  IO (Int, Int, Int)
sizes objectStore =
    roTx objectStore $ \tx ->
        (,,) <$> M.size tx (objects objectStore)
            <*> MM.size tx (byAKI objectStore)
            <*> MM.size tx (mftByAKI objectStore)


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

    rwTx objectStore $ \tx -> 
        for_ ros' $ \ro -> 
            putObject tx objectStore $ toStorableObject ro  
    
    sizes1 <- sizes objectStore

    extracted <- roTx objectStore $ \tx -> getAll tx objectStore
    HU.assertEqual "Not the same objects" (sortOn getHash extracted) (sortOn getHash ros')
    
    compareLatestMfts objectStore ros1 aki1
    compareLatestMfts objectStore ros2 aki2  
    
    let (toDelete, toKeep) = List.splitAt (List.length ros1 `div` 2) ros1

    rwTx objectStore $ \tx -> 
        forM_ toDelete $ \ro -> 
        deleteObject tx objectStore (getHash ro)

    sizes2 <- sizes objectStore

    compareLatestMfts objectStore toKeep aki1
    compareLatestMfts objectStore ros2 aki2  

    where
        removeMftNumberDuplicates ros = List.nubBy sameMftNumber ros
            where 
                sameMftNumber ro1 ro2 = 
                    case (ro1, ro2) of
                        (MftRO mft1, MftRO mft2) -> getMftNumber mft1 == getMftNumber mft2
                        _ -> False

        compareLatestMfts objectStore ros a = do
            mftLatest <- roTx objectStore $ \tx -> findLatestMftByAKI tx objectStore a         
            
            let mftLatest' = listToMaybe $ sortOn (negate . getMftNumber) $
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



-- should_insert_and_get_all_back_from_repository_store :: IO ((FilePath, Env), RepositoryStore LmdbStorage) -> HU.Assertion
-- should_insert_and_get_all_back_from_repository_store io = do  
--     (_, repositoryStore) <- io 

--     ros :: [Repository] <- nubBy (\r1 r2 -> repositoryURI r1 == repositoryURI r2) <$> 
--         (forM [1 :: Int .. 100] $ const $ QC.generate arbitrary)

--     taName1 <- TaName <$> QC.generate arbitrary
--     taName2 <- QC.generate $ (TaName <$> arbitrary) `QC.suchThat` (/= taName1)

--     withTas <- forM ros $ \r -> do        
--         ta <- QC.generate $ QC.elements [taName1, taName2]
--         pure (r, ta)        

--     rwTx repositoryStore $ \tx -> 
--         forM_ withTas $ \(repository, ta) ->            
--             putRepository tx repositoryStore repository ta

--     reposForTa1 <- roTx repositoryStore $ \tx -> getTaPublicationPoints tx repositoryStore taName1
--     reposForTa2 <- roTx repositoryStore $ \tx -> getTaPublicationPoints tx repositoryStore taName2

--     let reposByTa ta = List.sort $ List.map fst $ List.filter ((== ta) . snd) withTas

--     let assertForTa ta repos = HU.assertEqual 
--             ("Not the same repositories for " <> show ta) 
--             repos (reposFromList $ reposByTa ta)

--     assertForTa taName1 reposForTa1
--     assertForTa taName2 reposForTa2


-- should_read_create_change_set_and_apply_repository_store :: IO ((FilePath, Env), RepositoryStore LmdbStorage) -> HU.Assertion
-- should_read_create_change_set_and_apply_repository_store io = do  
--     (_, repositoryStore) <- io 

--     ros :: [Repository] <- nubBy (\r1 r2 -> repositoryURI r1 == repositoryURI r2) <$> 
--         (forM [1 :: Int .. 100] $ const $ QC.generate arbitrary)

--     taName1 <- TaName <$> QC.generate arbitrary
--     taName2 <- QC.generate $ (TaName <$> arbitrary) `QC.suchThat` (/= taName1)

--     withTas <- forM ros $ \r -> do        
--         ta <- QC.generate $ QC.elements [taName1, taName2]
--         pure (r, ta)        

--     rwTx repositoryStore $ \tx -> 
--         forM_ withTas $ \(repository, ta) ->            
--             putRepository tx repositoryStore repository ta

--     reposForTa1 <- roTx repositoryStore $ \tx -> getTaPublicationPoints tx repositoryStore taName1
--     reposForTa2 <- roTx repositoryStore $ \tx -> getTaPublicationPoints tx repositoryStore taName2

--     let reposByTa ta = List.sort $ List.map fst $ List.filter ((== ta) . snd) withTas

--     let assertForTa ta repos = HU.assertEqual 
--             ("Not the same repositories for " <> show ta) 
--             repos (reposFromList $ reposByTa ta)

--     assertForTa taName1 reposForTa1
--     assertForTa taName2 reposForTa2
    

generateSome :: Arbitrary a => IO [a]
generateSome = forM [1 :: Int .. 1000] $ const $ QC.generate arbitrary      

withDB :: (IO ((FilePath, LmdbEnv), DB LmdbStorage) -> TestTree) -> TestTree
withDB testTree = withResource (makeLmdbStuff createDatabase) releaseLmdb testTree


makeLmdbStuff :: (LmdbEnv -> IO b) -> IO ((FilePath, LmdbEnv), b)
makeLmdbStuff mkStore = do 
    dir <- createTempDirectory "/tmp" "lmdb-test"
    e <- mkLmdb dir 100
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
