{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}

module RPKI.Store.DiffSpec where

import           Data.Text
import           Data.Set                 (Set)
import           Data.Map.Strict          (Map)
import           Data.Tuple.Strict

import           Test.Tasty                 hiding (after)
import qualified Test.Tasty.QuickCheck      as QC

import           RPKI.Orphans

import           RPKI.Reporting
import           RPKI.Domain
import           RPKI.Store.Diff

diffGroup :: TestTree
diffGroup = testGroup "PublicationPoints" [
        QC.testProperty "Aspa applyDiff works" $ checkDiff @(Set Aspa),
        QC.testProperty "SplN applyDiff works" $ checkDiff @(Set SplN),
        QC.testProperty "Gbr applyDiff works" $ checkDiff @(Set (T2 Hash Gbr)),
        QC.testProperty "BGPSecPayload applyDiff works" $ checkDiff @(Set BGPSecPayload),
        QC.testProperty "Trace applyDiff works" $ checkDiff @(Set Trace),
        QC.testProperty "Roas applyDiff works" $ checkDiff @Roas,
        QC.testProperty "Vrps applyDiff works" $ checkDiff @Vrps,
        QC.testProperty "Validations applyDiff works" $ checkDiff @Validations
    ]

checkDiff :: (Eq a, Diffable a) => (a, a) -> Bool
checkDiff (before, after) = 
    applyDiff before (makeDiff before after) == after