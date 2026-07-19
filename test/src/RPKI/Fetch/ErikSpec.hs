{-# LANGUAGE OverloadedStrings   #-}

module RPKI.Fetch.ErikSpec where

import           RPKI.TestCommons
import           RPKI.Fetch.ErikRelay
import           Test.Tasty
import qualified Test.Tasty.HUnit                  as HU

import           RPKI.AppContext
import           RPKI.AppState
import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging

erikSpec :: TestTree
erikSpec = testGroup "Unit tests for repostory updates" [ 
        HU.testCase "Fetch Erik test" testFetchErik
    ]

testFetchErik :: HU.Assertion
testFetchErik = do 
    withTestContext $ \testContext@AppContext {..} -> do
        worldVersion <- newWorldVersion

        let fqdn = FQDN "rsync.paas.rpki.ripe.net"
        -- let fqdn = FQDN "krill.47272.net"
        let relayUri = URI "https://miso.sobornost.net"

        (z, _) <- runValidatorT (newScopes "erik-test") $ 
            fetchErik testContext worldVersion relayUri fqdn
        case z of
            Left err -> HU.assertFailure $ "Failed to fetch Erik index: " <> show err
            Right index -> do
                HU.assertBool "Erik index should not be empty" True

        logInfo logger "Fetched Erik index once."

        -- Now do the same fetch again and expect no actual fetches to happen, 
        -- it should only use cached data
        (z1, _) <- runValidatorT (newScopes "erik-test-2") $ 
            fetchErik testContext worldVersion relayUri fqdn
        case z1 of
            Left err -> HU.assertFailure $ "Failed to fetch Erik index second time: " <> show err
            Right index -> do
                HU.assertBool "Erik index should not be empty" True
        
    