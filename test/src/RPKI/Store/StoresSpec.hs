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

import Control.Exception (bracket)
import           Control.Monad
import           Data.Maybe
import           Data.List as L

import Control.Lens

import           System.IO.Temp
import           System.Directory

import           Test.QuickCheck.Arbitrary.Generic
import           Test.QuickCheck.Monadic
import           Test.Tasty
import qualified Test.Tasty.QuickCheck             as QC
import qualified Test.Tasty.HUnit as HU

import           RPKI.Domain
import           RPKI.Orphans
import           RPKI.Store.Base.LMDB
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import           RPKI.Store.Base.Map as M
import           RPKI.Store.Base.MultiMap as MM
import           RPKI.Store.Stores

import           RPKI.Store.Util


objectStoreGroup :: TestTree
objectStoreGroup = withObjectStore $ \io -> testGroup "Rpki object LMDB storage test"
  [
    HU.testCase "Should insert and get" (should_insert_and_get_all_back_from_object_store io)
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
  ros :: [RpkiObject] <- removeMftNumberDuplicates <$> generateSomeObjects

  let (firstHalf, secondHalf) = L.splitAt (L.length ros `div` 2) ros

  let ros1 = L.map (replaceAKI aki1) firstHalf
  let ros2 = L.map (replaceAKI aki2) secondHalf
  let ros' = ros1 <> ros2 

  rwTx objectStore $ \tx -> do
    forM_ ros' $ \ro -> 
      putObject tx objectStore $ toStorableObject ro  
  
  sizes1 <- sizes objectStore

  extracted <- roTx objectStore $ \tx -> getAll tx objectStore
  HU.assertEqual "Not the same objects" (sortOn getHash extracted) (sortOn getHash ros')
  
  compareLatestMfts objectStore ros1 aki1
  compareLatestMfts objectStore ros2 aki2  
  
  let (toDelete, toKeep) = L.splitAt (L.length ros1 `div` 2) ros1

  rwTx objectStore $ \tx -> 
    forM_ toDelete $ \ro -> 
      deleteObject tx objectStore (getHash ro)

  sizes2 <- sizes objectStore

  compareLatestMfts objectStore toKeep aki1
  compareLatestMfts objectStore ros2 aki2  

  where
    removeMftNumberDuplicates ros = L.nubBy sameMftNumber ros
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

generateSomeObjects :: IO [RpkiObject]
generateSomeObjects = forM [1 :: Int .. 1000] $ const $ QC.generate arbitrary      
      

withObjectStore :: (IO ((FilePath, Env), RpkiObjectStore LmdbStorage) -> TestTree) -> TestTree
withObjectStore testTree = withResource (makeLmdbStuff createObjectStore) releaseLmdb testTree

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
