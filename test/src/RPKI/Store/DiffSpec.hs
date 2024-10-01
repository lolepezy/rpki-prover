{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}

module RPKI.Store.DiffSpec where

import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Map.Strict          (Map)
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
diffGroup = localOption (QC.QuickCheckTests 300) $ testGroup "Diffs" [        
        QC.testProperty "Aspa applyDiff works" $ checkDiff @(Set Aspa),
        QC.testProperty "SplN applyDiff works" $ checkDiff @(Set SplN),
        QC.testProperty "Gbr applyDiff works" $ checkDiff @(Set (T2 Hash Gbr)),
        QC.testProperty "BGPSecPayload applyDiff works" $ checkDiff @(Set BGPSecPayload),
        QC.testProperty "Trace applyDiff works" $ checkDiff @(Set Trace),

        QC.testProperty "Roas applyDiff works" $ \(before :: Roas, after :: Roas) -> 
            checkDiff before after,

        QC.testProperty "Vrps applyDiff works" $ \(before :: Vrps, after :: Vrps) ->
            checkDiff before after,

        QC.testProperty "Validations applyDiff works" $ \(before :: Validations, after :: Validations) -> 
            checkDiff before after
    ]


checkDiff :: (Eq a, Compressable a, Diffable a) => a -> a -> Bool
checkDiff before after = 
    if (applyDiff before (makeDiff before after) == after)
        then True
        else let 
            -- The only reasons tests are not allowed to pass is
            -- empty map values that will be filtered out by the diff 
            -- procedure.
            bShrunk = shrink before
            aShrunk = shrink after
        in applyDiff bShrunk (makeDiff bShrunk aShrunk) == aShrunk        

class Compressable a where 
    shrink :: a -> a     
    shrink = id
    isEmpty :: a -> Bool
    isEmpty _ = False

instance Compressable (Set v) where    
    isEmpty = Set.null

instance Compressable Roas where    
    shrink = Roas . shrink . unRoas
    isEmpty = isEmpty . unRoas

instance Compressable Vrps where    
    shrink = Vrps . shrink . unVrps
    isEmpty = isEmpty . unVrps

instance Compressable Validations where    
    shrink = Validations . shrink . unValidations
    isEmpty = isEmpty . unValidations

instance Compressable v => Compressable (Map k v) where    
    shrink = Map.filter (not . isEmpty) . Map.map shrink 
    isEmpty = Map.null

instance Compressable v => Compressable (MonoidalMap.MonoidalMap k v) where    
    shrink = MonoidalMap.filter (not . isEmpty) . MonoidalMap.map shrink 
    isEmpty = MonoidalMap.null
