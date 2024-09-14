{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}


module RPKI.Store.DiffSpec where

import           Data.Set                 (Set)

import           Test.Tasty                 hiding (after)
import qualified Test.Tasty.QuickCheck      as QC

import           RPKI.Orphans

import           RPKI.Domain
import           RPKI.Store.Diff

diffGroup :: TestTree
diffGroup = testGroup "PublicationPoints" [
        QC.testProperty "Aspa applyDiff works" $ checkDiff @Aspa,
        QC.testProperty "Aspa applyDiff works" $ checkDiff @SplN
    ]

checkDiff :: Ord a => (Set a, Set a) -> Bool
checkDiff (before, after) = let 
    diff = makeSetDiff before after
    after' = applySetDiff before diff
    in after' == after