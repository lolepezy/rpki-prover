{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

module RPKI.Store.StoresSpec where

import Control.Exception (bracket)
import           Control.Monad
import           Data.Maybe
import           Data.List as L
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T

import           System.IO.Temp

import           System.Directory

import Data.Data (Typeable)
import Data.List.NonEmpty
import GHC.Generics

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

  let ros1 = L.map (setAKI aki1) firstHalf
  let ros2 = L.map (setAKI aki2) secondHalf
  let ros' = ros1 <> ros2 

  rwTx objectStore $ \tx -> do
    forM_ ros' $ \ro -> 
      putObject tx objectStore $ toStorableObject ro  
  
  (s1, s2, s3) <- sizes objectStore

  extracted <- roTx objectStore $ \tx -> getAll tx objectStore
  HU.assert $ sortOn (hash . getMeta) extracted == sortOn (hash . getMeta) ros'
  
  compareLatestMfts objectStore ros1 aki1
  compareLatestMfts objectStore ros2 aki2  
  where 
    setAKI a (CerRO (FullMeta m s, x)) = CerRO (FullMeta m { aki = Just a } s, x)
    setAKI a (MftRO (FullMeta m s, x)) = MftRO (FullMeta m { aki = Just a } s, x)
    setAKI a (RoaRO (FullMeta m s, x)) = RoaRO (FullMeta m { aki = Just a } s, x)
    setAKI a (GbrRO (FullMeta m s, x)) = GbrRO (FullMeta m { aki = Just a } s, x)
    setAKI a (CrlRO (m, x)) = CrlRO (m { aki = Just a }, x)

    compareLatestMfts objectStore ros a = do
      mftLatest <- roTx objectStore $ \tx -> findLatestMftByAKI tx objectStore a
      let mftLatest' = listToMaybe $ sortOn (negate . getMftNumber) $
            [ mft | MftRO mft@(FullMeta RpkiMeta { aki = Just a } _, _) <- ros ]      
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

