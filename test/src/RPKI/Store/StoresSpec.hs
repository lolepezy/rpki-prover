{-# LANGUAGE OverloadedStrings #-}

module RPKI.Store.StoresSpec where

import           Test.QuickCheck.Arbitrary.Generic
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck                as QC


objectStoreGroup :: TestTree
objectStoreGroup = testGroup "Rpki object LMDB storage test"
  [
    QC.testProperty
      "Store object and check if it's there"
      prop_stored_object_is_in_the_store
  ]

prop_stored_object_is_in_the_store :: QC.Property
prop_stored_object_is_in_the_store = monadicIO $ do  
  assert True