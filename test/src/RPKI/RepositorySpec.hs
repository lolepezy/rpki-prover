{-# LANGUAGE OverloadedStrings #-}

module RPKI.RepositorySpec where

import           Data.List             as List
import qualified Data.Set              as Set
import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import           RPKI.Domain
import           RPKI.Repository


repositoryGroup :: TestTree
repositoryGroup = testGroup "Repositories" [
        QC.testProperty
            "Generates the same hierarchy regardless of the order"
            prop_creates_thesame_hierarchy_regardless_of_shuffle,
        QC.testProperty
            "Updating repository tree gives back correct update status"
            prop_updates_repository_tree_returns_correct_status
    ]

repositoriesURIs :: [RsyncRepository]
repositoriesURIs = map (\s -> RsyncRepository (URI $ "rsync://host1.com/" <> s) New) $ [
        "a",
        "a/b",
        "a/c",
        "a/z",
        "a/z/q",
        "a/z/q/zzz",
        "a/z/q/aa",
        "a/z/p/q",
        "b/a",
        "b/a/c",
        "a/z/q",
        "b/a/d",
        "b/a/e",
        "b/z",
        "different_root"      
    ]

prop_creates_thesame_hierarchy_regardless_of_shuffle :: QC.Property
prop_creates_thesame_hierarchy_regardless_of_shuffle = 
    QC.forAll (QC.shuffle repositoriesURIs) $ \repositories -> 
        let
            mergedForest = createRsyncForest repositories
            in mergedForest == initialForest && checkForest mergedForest
    where
        initialForest = createRsyncForest repositoriesURIs
        
        checkForest (Forest trees) = 
            urisAreNotNestedOnTheSameLevel trees && 
            treesAreSorted trees && 
            List.all childURINestedInParent trees
        
        urisAreNotNestedOnTheSameLevel trees = 
            List.null $
                List.filter (\(t1, t2) -> nested t1 t2) $
                [ (t1, t2) | t1 <- trees, t2 <- trees, t1 /= t2]
            where 
                nested (RsyncTree (RsyncRepository u1 _) _) (RsyncTree (RsyncRepository u2 _) _) = 
                    u1 `isParentOf` u2 || u2 `isParentOf` u1

        treesAreSorted trees = 
            uris == sortedURIs &&
            List.all (\(RsyncTree _ children) -> treesAreSorted children) trees
            where 
                uris = map (\(RsyncTree (RsyncRepository u _) _) -> u) trees
                sortedURIs = List.sort uris

        childURINestedInParent (RsyncTree (RsyncRepository uri' _) children) = 
            List.all (\(RsyncTree (RsyncRepository childUri _) _) -> uri' `isParentOf` childUri) children &&
            List.all childURINestedInParent children


prop_updates_repository_tree_returns_correct_status :: QC.Property
prop_updates_repository_tree_returns_correct_status = 
    QC.forAll (QC.sublistOf repositoriesURIs) $ \pickedUpUris -> 
        let                       
            leftOutURIs = Set.toList $ (Set.fromList repositoriesURIs) `Set.difference` (Set.fromList pickedUpUris)
            Forest mergedTrees = createRsyncForest pickedUpUris 

            allLeftOutAreMergedAsNew = all (\case
                    AddedNew       -> True
                    AlreadyThere _ -> False
                ) $ map (\r -> snd $ r `mergeInto` mergedTrees) leftOutURIs  
            
            allPickedAreMergedAsAlreadtyExisting = all (\r@(RsyncRepository u _) -> 
                    case r `mergeInto` mergedTrees of
                        (_, AddedNew)        -> False
                        (_, AlreadyThere (RsyncR (RsyncTree (RsyncRepository u' _) _))) -> u == u'
                    ) pickedUpUris  
            
            in allLeftOutAreMergedAsNew && allPickedAreMergedAsAlreadtyExisting
