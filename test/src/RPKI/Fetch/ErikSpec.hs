{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NumericUnderscores  #-}

module RPKI.Fetch.ErikSpec where

import           RPKI.TestCommons
import           RPKI.Fetch.ErikRelay
import           Test.Tasty
import qualified Test.Tasty.HUnit                  as HU

import           Control.Concurrent.STM
import qualified System.Timeout                    as Timeout
import           Data.Either                       (isRight)
import qualified Data.List                         as List

import           Effectful

import           RPKI.AppContext
import           RPKI.AppState
import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Fetch.RelayPool
import           RPKI.Reporting
import           RPKI.Logging
import qualified RPKI.Util                         as U

erikSpec :: TestTree
erikSpec = testGroup "Unit tests for repository updates" [ 
        HU.testCase "Fetch Erik test" testFetchErik
    ]

testFetchErik :: HU.Assertion
testFetchErik = do 
    withTestContext $ \testContext@AppContext {..} -> do
        worldVersion <- newWorldVersion

        -- let fqdn = FQDN "rsync.paas.rpki.ripe.net"
        let fqdn = FQDN "ca.rg.net"
        let relayUri = URI "https://miso.sobornost.net"
        -- let relayUri = URI "http://relay.rpki-servers.org"

        (z, _) <- runValidatorIO (newScopes "erik-test") $ 
            fetchErik testContext worldVersion [relayUri] fqdn
        case z of
            Left err -> HU.assertFailure $ "Failed to fetch Erik index: " <> show err
            Right index -> do
                HU.assertBool "Erik index should not be empty" True

        logInfo logger "Fetched Erik index once."

        -- Now do the same fetch again and expect no actual fetches to happen, 
        -- it should only use cached data
        (z1, _) <- runValidatorIO (newScopes "erik-test-2") $ 
            fetchErik testContext worldVersion [relayUri] fqdn
        case z1 of
            Left err -> HU.assertFailure $ "Failed to fetch Erik index second time: " <> show err
            Right index -> do
                HU.assertBool "Erik index should not be empty" True
        
    

{- Tests for the Erik work pool.

   These use the pool directly with synthetic work rather than a relay, since
   what is worth pinning down is the concurrency contract: it drains, it stops,
   it deduplicates, and a relay that refuses an item does not lose it.
-}
relayPoolSpec :: TestTree
relayPoolSpec = testGroup "Erik relay work pool" [
        poolTestCase "Drains a tree of work and deduplicates by hash" testPoolDrains,
        poolTestCase "Falls back to a relay that answers" testPoolFallsBack,
        poolTestCase "Gives up on an item no relay can serve" testPoolGivesUp,
        poolTestCase "Benches a failing relay and finishes without it" testPoolBenchesRelay,
        poolTestCase "Benches a relay that cannot serve the index" testIndexFailureBenches
    ]

{- | A test case with a deadline.

     A bug in the termination or eligibility rule shows up as a pool that never
     returns, and a hung test that fails is worth something where a hung test
     suite is not.
-}
poolTestCase :: String -> HU.Assertion -> TestTree
poolTestCase name assertion = HU.testCase name $
    Timeout.timeout 30_000_000 assertion >>= \case
        Just () -> pure ()
        Nothing -> HU.assertFailure "timed out: the work pool did not terminate"

-- | Work items are plain ints; the hash is derived from the int, so "same item"
-- and "same hash" coincide the way they do for content-addressed objects.
intHash :: Int -> Hash
intHash = U.sha256s . U.convert . show

withQuietLogger :: (AppLogger -> IO a) -> IO a
withQuietLogger = withLogger (newLogConfig ErrorL MainLog)

runPool :: AppLogger
        -> [URI]
        -> [Int]
        -> (URI -> Int -> Eff AppEffects [(Hash, Int)])
        -> IO (Either AppError (), WorkPool Int, Relays)
runPool logger uris initial process = do
    relays <- newRelays 3 uris
    pool   <- newWorkPool
    enqueue pool [ (intHash n, n) | n <- initial ]
    (r, _) <- runValidatorIO (newScopes "pool-test") $
                runRelayWorkers logger relays pool process
    pure (r, pool, relays)

{- A three-level tree in which the two level-1 items share all of their
   children, so half of the fan-out is duplicate references. The queue is keyed
   by hash, so every item must be processed exactly once.
-}
testPoolDrains :: HU.Assertion
testPoolDrains =
    withQuietLogger $ \logger -> do
        seen <- newTVarIO ([] :: [Int])
        let children n
                | n < 10    = [20, 21, 22, 23]   -- both roots point at the same children
                | n < 30    = [100 + n]
                | otherwise = []
        (r, pool, _) <- runPool logger [URI "https://a", URI "https://b"] [1, 2] $ \_ n -> do
            liftIO $ atomically $ modifyTVar' seen (n :)
            pure [ (intHash c, c) | c <- children n ]

        HU.assertBool ("Pool should have finished cleanly, got " <> show r) (isRight r)
        processed <- readTVarIO seen
        let expected = [1, 2] <> [20, 21, 22, 23] <> [120, 121, 122, 123]
        HU.assertEqual "every item processed exactly once"
            (List.sort expected) (List.sort processed)
        failures <- poolFailures pool
        HU.assertEqual "no failures" [] (map fst failures)

-- | The first relay refuses everything; the second answers. Nothing may be lost.
testPoolFallsBack :: HU.Assertion
testPoolFallsBack =
    withQuietLogger $ \logger -> do
        let deadUri = URI "https://dead"
        seen <- newTVarIO ([] :: [Int])
        (r, pool, _) <- runPool logger [deadUri, URI "https://alive"] [1 .. 20] $ \uri n ->
            if uri == deadUri
                then appError $ ErikE $ UnknownErikProblem "nope"
                else do
                    liftIO $ atomically $ modifyTVar' seen (n :)
                    pure []

        HU.assertBool ("Pool should have finished cleanly, got " <> show r) (isRight r)
        processed <- readTVarIO seen
        HU.assertEqual "every item served by the live relay"
            [1 .. 20] (List.sort processed)
        failures <- poolFailures pool
        HU.assertEqual "no item counted as failed" [] (map fst failures)

-- | One poisoned item that no relay can serve must end up in `failures` without
-- stalling the pool or taking the other items down with it.
testPoolGivesUp :: HU.Assertion
testPoolGivesUp =
    withQuietLogger $ \logger -> do
        seen <- newTVarIO ([] :: [Int])
        (r, pool, _) <- runPool logger [URI "https://a", URI "https://b"] [1 .. 10] $ \_ n ->
            if n == 7
                then appError $ ErikE $ UnknownErikProblem "poisoned"
                else do
                    liftIO $ atomically $ modifyTVar' seen (n :)
                    pure []

        HU.assertBool ("Pool should have finished cleanly, got " <> show r) (isRight r)
        processed <- readTVarIO seen
        HU.assertEqual "everything except the poisoned item went through"
            ([1 .. 6] <> [8 .. 10]) (List.sort processed)
        failures <- poolFailures pool
        HU.assertEqual "the poisoned item is reported once"
            [intHash 7] (map fst failures)

{- A relay failing repeatedly gets benched, and its share of the queue has to
   end up with the relay that is still answering -- including items it had
   already refused, which is the case that can deadlock if the eligibility rule
   is wrong.
-}
testPoolBenchesRelay :: HU.Assertion
testPoolBenchesRelay =
    withQuietLogger $ \logger -> do
        let deadUri = URI "https://dead"
        seen <- newTVarIO ([] :: [Int])
        (r, pool, relays) <- runPool logger [deadUri, URI "https://alive"] [1 .. 50] $ \uri n ->
            if uri == deadUri
                then appError $ ErikE $ UnknownErikProblem "nope"
                else do
                    liftIO $ atomically $ modifyTVar' seen (n :)
                    pure []

        HU.assertBool ("Pool should have finished cleanly, got " <> show r) (isRight r)
        processed <- readTVarIO seen
        HU.assertEqual "nothing is stranded on the benched relay"
            [1 .. 50] (List.sort processed)
        failures <- poolFailures pool
        HU.assertEqual "no item counted as failed" [] (map fst failures)

        -- The point of benching: the dead relay is taken out of rotation rather
        -- than being offered every one of the 50 items in turn.
        case [ relay | relay <- relayList relays, relay.relayUri == deadUri ] of
            [deadRelay] -> do
                stillAlive <- readTVarIO deadRelay.alive
                HU.assertBool "the failing relay should have been benched" (not stillAlive)
                failedCount <- readTVarIO deadRelay.failed
                HU.assertBool
                    ("a benched relay stops taking work, but this one failed "
                        <> show failedCount <> " of 50 items")
                    (failedCount <= benchThreshold relays * length (relayList relays))
            _ -> HU.assertFailure "no relay recorded for the dead URI"


{- The index fetch is the first thing a fetch does and the last cheap chance to
   notice a relay is unusable. A relay that fails it must be out of rotation
   before the work pool starts, or every one of its threads pays the same
   connect timeout over again to learn the same thing.
-}
testIndexFailureBenches :: HU.Assertion
testIndexFailureBenches =
    withQuietLogger $ \logger -> do
        let deadUri = URI "https://dead"
        let liveUri = URI "https://alive"
        relays <- newRelays 3 [deadUri, liveUri]
        (r, _) <- runValidatorIO (newScopes "index-test") $
            withAnyRelay logger relays $ \uri ->
                if uri == deadUri
                    then appError $ ErikE $ UnknownErikProblem "no index here"
                    else pure uri

        HU.assertEqual "the live relay served the index" (Right liveUri) r

        case [ relay | relay <- relayList relays, relay.relayUri == deadUri ] of
            [deadRelay] -> do
                stillAlive <- readTVarIO deadRelay.alive
                HU.assertBool
                    "a relay that cannot serve the index is benched straight away"
                    (not stillAlive)
            _ -> HU.assertFailure "no relay recorded for the dead URI"

        -- The live relay answered, so it must still be usable.
        case [ relay | relay <- relayList relays, relay.relayUri == liveUri ] of
            [liveRelay] -> do
                stillAlive <- readTVarIO liveRelay.alive
                HU.assertBool "the relay that answered stays in rotation" stillAlive
            _ -> HU.assertFailure "no relay recorded for the live URI"
