{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}

module RPKI.Store.DiffSpec where

import           Data.Set                 (Set)
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
diffGroup = testGroup "PublicationPoints" [
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


checkDiff :: (Eq a, Shrinkable a, Diffable a) => a -> a -> Bool
checkDiff before after = 
    if (applyDiff before (makeDiff before after) == after)
        then True
        else let 
            bShrunk = shrink before
            aShrunk = shrink after
        in applyDiff bShrunk (makeDiff bShrunk aShrunk) == aShrunk        

class Shrinkable a where 
    shrink :: a -> a     
    shrink = id
    isEmpty :: a -> Bool
    isEmpty _ = False

instance Shrinkable (Set v)

instance Shrinkable Roas where    
    shrink = Roas . shrink . unRoas
    isEmpty = isEmpty . unRoas

instance Shrinkable Vrps where    
    shrink = Vrps . shrink . unVrps
    isEmpty = isEmpty . unVrps

instance Shrinkable Validations where    
    shrink = Validations . shrink . unValidations
    isEmpty = isEmpty . unValidations

instance Shrinkable v => Shrinkable (Map k v) where    
    shrink = Map.filter (not . isEmpty) . Map.map shrink 
    isEmpty = Map.null

instance Shrinkable v => Shrinkable (MonoidalMap.MonoidalMap k v) where    
    shrink = MonoidalMap.filter (not . isEmpty) . MonoidalMap.map shrink 
    isEmpty = MonoidalMap.null
