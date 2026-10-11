{-# LANGUAGE OverloadedStrings #-}

module RPKI.Workflow.SharedSpec where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad

import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set

import           Data.Hourglass           (Seconds)

import           Test.Tasty
import qualified Test.Tasty.HUnit         as HU

import           RPKI.Domain
import           RPKI.Logging
import           RPKI.Time
import           RPKI.Workflow.Shared


revalidationOnExpirySpec :: TestTree
revalidationOnExpirySpec = testGroup "Revalidation on expiry" [
        HU.testCase "Revalidates a TA right after its first object expires" shouldRevalidateOnExpiry,
        HU.testCase "Doesn't revalidate when the expiration time moves on" shouldNotRevalidateForOldExpiration,
        HU.testCase "Revalidates when validations find the same expiration time again" shouldRevalidateForSameExpiration
    ]


shouldRevalidateOnExpiry :: HU.Assertion
shouldRevalidateOnExpiry = withExpiryLoop $ \expirations tasToValidate -> do
    expiresAt <- inSeconds 1
    atomically $ writeTVar expirations $ Map.fromList [(ripe, expiresAt), (arin, mempty)]

    early <- revalidatedWithin 500_000 tasToValidate ripe
    HU.assertBool "Not before the object expires" (not early)

    revalidated <- revalidatedWithin 3_000_000 tasToValidate ripe
    HU.assertBool "After the object expired" revalidated

    left <- readTVarIO expirations
    HU.assertEqual "The expiration time is dropped once it has come" [arin] (Map.keys left)


shouldNotRevalidateForOldExpiration :: HU.Assertion
shouldNotRevalidateForOldExpiration = withExpiryLoop $ \expirations tasToValidate -> do
    soon  <- inSeconds 1
    later <- inSeconds 3600
    atomically $ writeTVar expirations $ Map.singleton ripe soon
    -- A validation found that the object is re-issued
    atomically $ modifyTVar' expirations $ Map.union $ Map.singleton ripe later

    revalidated <- revalidatedWithin 2_000_000 tasToValidate ripe
    HU.assertBool "Not at the old expiration time" (not revalidated)


-- That's how it used to go wrong: the same expiration time from a
-- validation after another one was taken as already scheduled.
shouldRevalidateForSameExpiration :: HU.Assertion
shouldRevalidateForSameExpiration = withExpiryLoop $ \expirations tasToValidate -> do
    expiresAt <- inSeconds 2
    atomically $ writeTVar expirations $ Map.singleton ripe expiresAt
    threadDelay 500_000
    atomically $ modifyTVar' expirations $ Map.union $ Map.singleton ripe expiresAt

    revalidated <- revalidatedWithin 4_000_000 tasToValidate ripe
    HU.assertBool "After the object expired" revalidated


-- The loop running, with the expiration times it waits
-- for and the TAs it asks to revalidate.
withExpiryLoop :: (TVar (Map TaName EarliestToExpire) -> TVar (Set TaName) -> IO a) -> IO a
withExpiryLoop f =
    withLogger (newLogConfig ErrorL MainLog) $ \logger -> do
        expirations   <- newTVarIO mempty
        tasToValidate <- newTVarIO mempty
        withAsync (revalidateOnExpiry logger expirations tasToValidate) $ \_ ->
            f expirations tasToValidate

inSeconds :: Seconds -> IO EarliestToExpire
inSeconds seconds = do
    Now now <- thisInstant
    pure $ EarliestToExpire $ momentAfter now seconds

-- Whether the TA is asked to be revalidated within the time
revalidatedWithin :: Int -> TVar (Set TaName) -> TaName -> IO Bool
revalidatedWithin microseconds tasToValidate taName = do
    timeIsUp <- registerDelay microseconds
    atomically $ do
        requested <- Set.member taName <$> readTVar tasToValidate
        up        <- readTVar timeIsUp
        unless (requested || up) retry
        pure requested

ripe, arin :: TaName
ripe = TaName "ripe"
arin = TaName "arin"
