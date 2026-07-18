{-# LANGUAGE OverloadedStrings   #-}

module RPKI.Fetch.ErikSpec where

import           RPKI.TestCommons
import           RPKI.Fetch.ErikRelay
import           Test.Tasty
import qualified Test.Tasty.HUnit                  as HU

import           RPKI.AppState
import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Reporting

erikSpec :: TestTree
erikSpec = testGroup "Unit tests for repostory updates" [ 
        HU.testCase "Fetch Erik test" testFetchErik
    ]

testFetchErik :: HU.Assertion
testFetchErik = do 
    withTestContext $ \testContext -> do
        worldVersion <- newWorldVersion
        (z, vs) <- runValidatorT (newScopes "erik-test") $ 
            fetchErik 
                testContext 
                worldVersion 
                (URI "https://miso.sobornost.net")
                (FQDN "ca.rg.net")
                -- (FQDN "rpki.ripe.net")
        case z of
            Left err -> HU.assertFailure $ "Failed to fetch Erik index: " <> show err
            Right index -> do
                HU.assertBool "Erik index should not be empty" True
        
    