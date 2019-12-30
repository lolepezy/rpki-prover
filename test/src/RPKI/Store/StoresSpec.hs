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
objectStoreGroup = lmdbTree $ \io -> testGroup "Rpki object LMDB storage test"
  [
    HU.testCase "Should insert and get" (should_insert_and_get_all_back io)
  ]

sizes :: Storage s =>
         RpkiObjectStore s ->  IO (Int, Int, Int)
sizes objectStore =
  roTx objectStore $ \tx -> do
    (,,) <$> M.size tx (objects objectStore)
         <*> MM.size tx (byAKI objectStore)
         <*> MM.size tx (mftByAKI objectStore)


should_insert_and_get_all_back :: IO ((FilePath, Env), RpkiObjectStore LmdbStorage) -> HU.Assertion
should_insert_and_get_all_back io = do  
  (_, objectStore) <- io
  aki1 :: AKI <- QC.generate arbitrary
  aki2 :: AKI <- QC.generate arbitrary
  ros :: [RpkiObject] <- generateSomeObjects
  let n = L.length ros
  let (firstHalf, secondHalf) = L.splitAt (n `div` 2) ros

  let ros1 = L.map (replaceAKI aki1) firstHalf
  let ros2 = L.map (replaceAKI aki2) secondHalf
  let ros' = ros1 <> ros2 

  rwTx objectStore $ \tx -> do
    forM_ ros' $ \ro -> 
      putObject tx objectStore $ toStorableObject ro  
  
  (s1, s2, s3) <- sizes objectStore

  extracted <- roTx objectStore $ \tx -> getAll tx objectStore
  HU.assert $ sortOn getHash extracted == sortOn getHash ros'
  
  compareLatestMfts objectStore ros1 aki1
  compareLatestMfts objectStore ros2 aki2  
  where
    compareLatestMfts objectStore ros a = do
      mftLatest <- roTx objectStore $ \tx -> findLatestMftByAKI tx objectStore a
      let mftLatest' = listToMaybe $ sortOn (negate . getMftNumber) $
            [ mft | MftRO mft <- ros, getAKI mft == Just a ]
      HU.assert $ mftLatest == mftLatest'

generateSomeObjects :: IO [RpkiObject]
generateSomeObjects = go
  where 
    go = do  
      ros :: [RpkiObject] <- QC.generate arbitrary
      if (L.length ros < 10) 
        then go 
        else pure ros 

lmdbTree :: (IO ((FilePath, Env), RpkiObjectStore LmdbStorage) -> TestTree) -> TestTree
lmdbTree testTree = withResource makeLmdbStuff releaseLmdb testTree

makeLmdbStuff = do 
  dir <- createTempDirectory "/tmp" "lmdb-test"
  putStrLn $ "Creating LMDB in " <> show dir
  e <- mkLmdb dir
  objectStore <- createObjectStore e
  pure ((dir, e), objectStore)

releaseLmdb ((dir, e), _) = do   
  closeLmdb e
  removeDirectoryRecursive dir
  putStrLn $ "Closed LMDB in " <> show dir


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
