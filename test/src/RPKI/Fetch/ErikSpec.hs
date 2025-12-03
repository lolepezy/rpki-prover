{-# LANGUAGE OverloadedStrings   #-}

module RPKI.Fetch.ErikSpec where

import           RPKI.TestTypes
import           RPKI.Fetch.ErikRelay
import           Test.Tasty
import qualified Test.Tasty.HUnit                  as HU

erikSpec :: TestTree
erikSpec = testGroup "Unit tests for repostory updates" [ 
        HU.testCase "Fetch Erik test" testFetchErik
    ]

testFetchErik :: HU.Assertion
testFetchErik = do 
    withTestContext $ \testContext -> do
        -- fetchErik testContext 
        pure ()
    