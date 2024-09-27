{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}

module RPKI.Store.DiffSpec where

import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import qualified Data.Map.Strict          as Map
import qualified Data.Map.Monoidal.Strict as MonoidalMap
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

        QC.testProperty "Roas applyDiff works" $ \(before, after) -> let
                noEmptyOnes = Roas . MonoidalMap.filter (not . MonoidalMap.null) . unRoas    
            in checkDiff (noEmptyOnes before, noEmptyOnes after),

        QC.testProperty "Vrps applyDiff works" $ \(before, after) -> let
                noEmptyOnes = Vrps . MonoidalMap.filter (not . Set.null) . unVrps    
            in checkDiff (noEmptyOnes before, noEmptyOnes after),

        QC.testProperty "Validations applyDiff works" $ \(before, after) -> let
                noEmptyOnes = Validations . Map.filter (not . Set.null) . unValidations    
            in checkDiff (noEmptyOnes before, noEmptyOnes after)
    ]


checkDiff :: (Eq a, Diffable a) => (a, a) -> Bool
checkDiff (before, after) = 
    applyDiff before (makeDiff before after) == after