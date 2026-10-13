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
revalidationOnExpirySpec = testGroup "Revalidation when TAs are due" [
        HU.testCase "Revalidates a TA right after its first object expires" shouldRevalidateOnExpiry,
        HU.testCase "Doesn't revalidate when the expiration time moves on" shouldNotRevalidateForOldExpiration,
        HU.testCase "Revalidates when validations find the same expiration time again" shouldRevalidateForSameExpiration,
        HU.testCase "Revalidates again an interval later, unless a validation tells otherwise" shouldRevalidatePeriodically,
        HU.testCase "Is due at the first object to expire, or an interval after the validation" shouldBeDueAfterValidation
    ]


shouldRevalidateOnExpiry :: HU.Assertion
shouldRevalidateOnExpiry = withDueLoop 3600 $ \nextValidation tasToValidate -> do
    expiresAt <- inSeconds 1
    later     <- inSeconds 3000
    atomically $ writeTVar nextValidation $ Map.fromList [(ripe, ValidityChange expiresAt), (arin, Periodic later)]

    early <- revalidatedWithin 500_000 tasToValidate ripe
    HU.assertBool "Not before the object expires" (not early)

    revalidated <- revalidatedWithin 3_000_000 tasToValidate ripe
    HU.assertBool "After the object expired" revalidated

    left <- readTVarIO nextValidation
    HU.assertBool "The TA is due again an interval later" $
        case Map.lookup ripe left of
            Just (Periodic t) -> t > later
            _                 -> False
    HU.assertEqual "The other TA isn't due yet" (Just $ Periodic later) (Map.lookup arin left)


shouldNotRevalidateForOldExpiration :: HU.Assertion
shouldNotRevalidateForOldExpiration = withDueLoop 3600 $ \nextValidation tasToValidate -> do
    soon  <- inSeconds 1
    later <- inSeconds 3600
    atomically $ writeTVar nextValidation $ Map.singleton ripe (ValidityChange soon)
    -- A validation found that the object is re-issued
    atomically $ modifyTVar' nextValidation $ Map.union $ Map.singleton ripe (ValidityChange later)

    revalidated <- revalidatedWithin 2_000_000 tasToValidate ripe
    HU.assertBool "Not at the old expiration time" (not revalidated)


-- That's how it used to go wrong: the same expiration time from a
-- validation after another one was taken as already scheduled.
shouldRevalidateForSameExpiration :: HU.Assertion
shouldRevalidateForSameExpiration = withDueLoop 3600 $ \nextValidation tasToValidate -> do
    expiresAt <- inSeconds 2
    atomically $ writeTVar nextValidation $ Map.singleton ripe (ValidityChange expiresAt)
    threadDelay 500_000
    atomically $ modifyTVar' nextValidation $ Map.union $ Map.singleton ripe (ValidityChange expiresAt)

    revalidated <- revalidatedWithin 4_000_000 tasToValidate ripe
    HU.assertBool "After the object expired" revalidated


-- E.g. the validation failed, then nothing replaces the moment
shouldRevalidatePeriodically :: HU.Assertion
shouldRevalidatePeriodically = withDueLoop 1 $ \nextValidation tasToValidate -> do
    soon <- inSeconds 0
    atomically $ writeTVar nextValidation $ Map.singleton ripe (ValidityChange soon)

    first_ <- revalidatedWithin 1_000_000 tasToValidate ripe
    HU.assertBool "When it's due" first_
    atomically $ writeTVar tasToValidate mempty

    early <- revalidatedWithin 500_000 tasToValidate ripe
    HU.assertBool "Not before the interval" (not early)

    again <- revalidatedWithin 2_500_000 tasToValidate ripe
    HU.assertBool "Again an interval later" again


shouldBeDueAfterValidation :: HU.Assertion
shouldBeDueAfterValidation = do
    let validatedAt = Instant 0
        inSeconds_ s = momentAfter validatedAt s
    HU.assertEqual "An object expires before the interval is over"
        (ValidityChange $ inSeconds_ 600)
        (dueAfterValidation 900 validatedAt (EarliestValidityChange $ inSeconds_ 600))
    HU.assertEqual "Nothing changes before the interval is over"
        (Periodic $ inSeconds_ 900)
        (dueAfterValidation 900 validatedAt (EarliestValidityChange $ inSeconds_ 3600))
    HU.assertEqual "Nothing ever changes"
        (Periodic $ inSeconds_ 900)
        (dueAfterValidation 900 validatedAt mempty)


-- The loop running, with the moments it waits for 
-- and the TAs it asks to revalidate.
withDueLoop :: Seconds -> (TVar (Map TaName Due) -> TVar (Set TaName) -> IO a) -> IO a
withDueLoop interval f =
    withLogger (newLogConfig ErrorL MainLog) $ \logger -> do
        nextValidation <- newTVarIO mempty
        tasToValidate  <- newTVarIO mempty
        withAsync (revalidateWhenDue logger interval nextValidation tasToValidate) $ \_ ->
            f nextValidation tasToValidate

inSeconds :: Seconds -> IO Instant
inSeconds seconds = do
    Now now <- thisInstant
    pure $ momentAfter now seconds

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
