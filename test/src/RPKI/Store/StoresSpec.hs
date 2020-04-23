{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NumericUnderscores        #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}

module RPKI.Store.StoresSpec where

import           Control.Monad
import           Data.List                         as List
import           Data.Maybe

import           System.Directory
import           System.IO.Temp

import           Test.QuickCheck.Arbitrary.Generic
import           Test.Tasty
import qualified Test.Tasty.HUnit                  as HU
import qualified Test.Tasty.QuickCheck             as QC

import           RPKI.Domain
import           RPKI.Repository
import           RPKI.Orphans
import           RPKI.Store.Base.LMDB
import           RPKI.Store.Base.Map               as M
import           RPKI.Store.Base.MultiMap          as MM
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import           RPKI.Store.Data
import           RPKI.Store.Stores
import           RPKI.Store.Repository

import           RPKI.Store.Util



storeGroup :: TestTree
storeGroup = withObjectStore $ \io -> testGroup "Rpki object LMDB storage test"
    [
        HU.testCase "Should insert and get" (should_insert_and_get_all_back_from_object_store io)        
    ]

repositoryStoreGroup :: TestTree
repositoryStoreGroup = withRepositoryStore $ \io -> testGroup "Repository LMDB storage test"
    [
        HU.testCase "Should insert and get a repository" (should_insert_and_get_all_back_from_repository_store io)        
    ]



sizes :: Storage s =>
        RpkiObjectStore s ->  IO (Int, Int, Int)
sizes objectStore =
    roTx objectStore $ \tx -> do
        (,,) <$> M.size tx (objects objectStore)
            <*> MM.size tx (byAKI objectStore)
            <*> MM.size tx (mftByAKI objectStore)


should_insert_and_get_all_back_from_object_store :: IO ((FilePath, Env), RpkiObjectStore LmdbStorage) -> HU.Assertion
should_insert_and_get_all_back_from_object_store io = do  
    (_, objectStore) <- io
    aki1 :: AKI <- QC.generate arbitrary
    aki2 :: AKI <- QC.generate arbitrary
    ros :: [RpkiObject] <- removeMftNumberDuplicates <$> generateSome

    let (firstHalf, secondHalf) = List.splitAt (List.length ros `div` 2) ros

    let ros1 = List.map (replaceAKI aki1) firstHalf
    let ros2 = List.map (replaceAKI aki2) secondHalf
    let ros' = ros1 <> ros2 

    rwTx objectStore $ \tx -> do
        forM_ ros' $ \ro -> 
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


should_insert_and_get_all_back_from_repository_store :: IO ((FilePath, Env), RepositoryStore LmdbStorage) -> HU.Assertion
should_insert_and_get_all_back_from_repository_store io = do  
    (_, repositoryStore) <- io 

    ros :: [Repository] <- nubBy (\r1 r2 -> repositoryURI r1 == repositoryURI r2) <$> 
        (forM [1 :: Int .. 100] $ const $ QC.generate arbitrary)

    taName1 <- TaName <$> QC.generate arbitrary
    taName2 <- QC.generate $ (TaName <$> arbitrary) `QC.suchThat` (/= taName1)

    withTas <- forM ros $ \r -> do        
        ta <- QC.generate $ QC.elements [taName1, taName2]
        pure (r, ta)        

    rwTx repositoryStore $ \tx -> 
        forM_ withTas $ \(repository, ta) ->            
            putRepository tx repositoryStore repository ta

    reposForTa1 <- roTx repositoryStore $ \tx -> getRepositoriesForTA tx repositoryStore taName1
    reposForTa2 <- roTx repositoryStore $ \tx -> getRepositoriesForTA tx repositoryStore taName2

    let reposByTa ta = List.sort $ List.map fst $ List.filter ((== ta) . snd) withTas

    let assertForTa ta repos = HU.assertEqual 
            ("Not the same repositories for " <> show ta) 
            repos (reposFromList $ reposByTa ta)

    assertForTa taName1 reposForTa1
    assertForTa taName2 reposForTa2
    

generateSome :: Arbitrary a => IO [a]
generateSome = forM [1 :: Int .. 1000] $ const $ QC.generate arbitrary      

withObjectStore :: (IO ((FilePath, Env), RpkiObjectStore LmdbStorage) -> TestTree) -> TestTree
withObjectStore testTree = withResource (makeLmdbStuff createObjectStore) releaseLmdb testTree

withRepositoryStore :: (IO ((FilePath, Env), RepositoryStore LmdbStorage) -> TestTree) -> TestTree
withRepositoryStore testTree = withResource (makeLmdbStuff createRepositoryStore) releaseLmdb testTree

withMM :: (IO ((FilePath, Env), SMultiMap "testMM" LmdbStorage Int String) -> TestTree) -> TestTree
withMM testTree = withResource (makeLmdbStuff mkMMap) releaseLmdb testTree
    where
        mkMMap e = do   
            mm <- createMulti e
            return $ SMultiMap (LmdbStorage e) mm


makeLmdbStuff mkStore = do 
    dir <- createTempDirectory "/tmp" "lmdb-test"
    e <- mkLmdb dir
    store <- mkStore e
    pure ((dir, e), store)

releaseLmdb ((dir, e), _) = do   
    closeLmdb e
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
