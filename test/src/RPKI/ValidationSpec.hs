{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RPKI.ValidationSpec where

import Control.Lens ((^.))
import Control.Monad (replicateM)
import Data.List (sortOn)
import qualified Data.List.NonEmpty          as NonEmpty
import           Data.Ord

import           Data.Hourglass         

import           Test.Tasty
import           Test.QuickCheck.Arbitrary.Generic
import qualified Test.Tasty.QuickCheck             as QC
import           Test.QuickCheck.Gen

import           RPKI.Domain
import           RPKI.Orphans
import           RPKI.Time
import           RPKI.Validation.Types
import           RPKI.Validation.Common



validationSpec :: TestTree
validationSpec = testGroup "Validation" [
        updateMftsTests
    ]

-- Unit tests for the `updateMfts` function
updateMftsTests :: TestTree
updateMftsTests = testGroup "updateMfts function" [
        QC.testProperty "inserts new manifest maintaining sorted order" prop_maintains_sorted_order,
        QC.testProperty "preserves non-expired manifests" prop_preserves_non_expired
    ]
  where
    prop_maintains_sorted_order =
        QC.forAll arbitraryMftsAndTime $ \(mfts, newMft, now) ->
            let MftsMeta result = updateMfts newMft now mfts
                resultList = NonEmpty.toList result
            in isSortedByNextUpdate resultList    

    prop_preserves_non_expired =
        QC.forAll arbitraryNonExpiredMftsScenario $ \(mfts, newMft, now) ->
            let MftsMeta original = mfts
                MftsMeta result = updateMfts newMft now mfts
                originalNonExpired = filter (\m -> m ^. #nextUpdate >= unNow now) (NonEmpty.toList original)
                resultNonExpired = filter (\m -> m ^. #nextUpdate >= unNow now) (NonEmpty.toList result)
            in all (`elem` resultNonExpired) originalNonExpired

    isSortedByNextUpdate :: [AnMft] -> Bool
    isSortedByNextUpdate [] = True
    isSortedByNextUpdate [_] = True
    isSortedByNextUpdate (m1:m2:ms) = 
        (m1 ^. #nextUpdate >= m2 ^. #nextUpdate) && isSortedByNextUpdate (m2:ms)


arbitraryDateTime :: Gen Instant
arbitraryDateTime = do
    year <- QC.choose (2020, 2030)
    month <- arbitrary
    day <- QC.choose (1, 28)
    hour <- arbitrary
    minute <- arbitrary
    second <- arbitrary
    pure $ newInstant $ DateTime (Date year month day) (TimeOfDay hour minute second 0)

arbitraryAnMft :: Instant -> Gen AnMft
arbitraryAnMft (toDateTime -> baseTime) = do
    thisOffset <- QC.choose (-24, 24) -- hours before/after base
    nextOffset <- QC.choose (thisOffset, thisOffset + 48) -- next update after this update
    let thisUpd = timeAdd baseTime (Hours thisOffset)
        nextUpd = timeAdd baseTime (Hours nextOffset)
    pure $ AnMft {
        thisUpdate = newInstant thisUpd,
        nextUpdate = newInstant nextUpd,
        key = ObjectKey $ ArtificialKey 10
    }

arbitraryMfts :: Instant -> Gen MftsMeta
arbitraryMfts baseTime = do
    count <- QC.choose (1, 5)
    mfts <- replicateM count (arbitraryAnMft baseTime)
    -- Sort by nextUpdate descending to maintain invariant
    let sorted = sortOn (Down . (^. #nextUpdate)) mfts
    pure $ MftsMeta (NonEmpty.fromList sorted)

arbitraryMftsAndTime :: Gen (MftsMeta, AnMft, Now)
arbitraryMftsAndTime = do
    baseTime <- arbitraryDateTime
    mfts <- arbitraryMfts baseTime
    newMft <- arbitraryAnMft baseTime
    now <- Now <$> arbitraryDateTime
    pure (mfts, newMft, now)

-- Generate scenario where manifests are not expired
arbitraryNonExpiredMftsScenario :: Gen (MftsMeta, AnMft, Now)
arbitraryNonExpiredMftsScenario = do
    now :: Instant <- arbitraryDateTime
    -- Create manifests that are all valid
    validMfts <- replicateM 3 $ arbitraryAnMft (momentAfter now (3600 * 24))
    newMft <- arbitraryAnMft (momentAfter now (3600 * 12))
    let allMfts = sortOn (Down . (^. #nextUpdate)) validMfts
    pure (MftsMeta (NonEmpty.fromList allMfts), newMft, Now now)
