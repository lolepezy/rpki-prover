{-# LANGUAGE OverloadedStrings #-}

module RPKI.Store.StoresSpec where

import           Control.Monad
import           Data.Maybe

import           System.IO.Temp
import           System.Directory

import           Test.QuickCheck.Arbitrary.Generic
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck             as QC

import           RPKI.Domain
import           RPKI.Orphans
import           RPKI.Store.Base.LMDB
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import           RPKI.Store.Stores

import           RPKI.Store.Util


objectStoreGroup :: TestTree
objectStoreGroup = lmdbTree $ \io -> testGroup "Rpki object LMDB storage test"
  [
    QC.testProperty
      "Store object and check if it's there"
      (prop_stored_object_is_in_the_store io)

    -- QC.testProperty
    --   "Store object and check if it's there, also check by AKI"
    --   (prop_stored_object_is_in_the_store_taken_by_aki io)
  ]

prop_stored_object_is_in_the_store :: IO ((a, Env), RpkiObjectStore LmdbStorage) -> QC.Property
prop_stored_object_is_in_the_store io = monadicIO $ do  
  (_, objectStore) <- run io
  ro :: RpkiObject <- pick arbitrary
  run $ rwTx objectStore $ \tx -> putObject tx objectStore $ toStorableObject ro
  ro' <- run $ roTx objectStore $ \tx -> getByHash tx objectStore (getHash ro)
  assert $ Just ro == ro'
  
  run $ rwTx objectStore $ \tx -> deleteObject tx objectStore (getHash ro)
  ro' <- run $ roTx objectStore $ \tx -> getByHash tx objectStore (getHash ro)
  assert $ isNothing ro'


prop_stored_object_is_in_the_store_taken_by_aki :: IO (Env, RpkiObjectStore LmdbStorage) -> QC.Property
prop_stored_object_is_in_the_store_taken_by_aki io = monadicIO $ do  
  (_, objectStore) <- run io
  ro <- pick arbitrary
  run $ rwTx objectStore $ \tx -> putObject tx objectStore $ toStorableObject ro
  case ro of  
    MftRO _ -> 
      case getAKI ro of
        Just aki' -> do
          ro' <- run $ roTx objectStore $ \tx -> findLatestMftByAKI tx objectStore aki'
          assert $ Just ro == (MftRO <$> ro')
        Nothing -> pure ()
    _ -> pure () 
    
  

lmdbTree :: (IO ((FilePath, Env), RpkiObjectStore LmdbStorage) -> TestTree) -> TestTree
lmdbTree testTree = withResource 
  makeLmdbStuff
  releaseLmdb
  testTree
  where 
    makeLmdbStuff = do 
      dir <- createTempDirectory "/tmp" "lmdb-test"
      putStrLn $ "Creating LMDB in " <> show dir
      e <- mkLmdb
      objectStore <- createObjectStore e
      pure ((dir, e), objectStore)
    releaseLmdb ((dir, e), _) = do 
      closeLmdb e
      removeDirectoryRecursive dir

    

