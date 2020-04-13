{-# LANGUAGE OverloadedStrings #-}

module RPKI.RepositorySpec where

import           Data.List                         as List
import           Test.Tasty
import qualified Test.Tasty.QuickCheck             as QC

import           RPKI.Domain
import           RPKI.Repository


repositoryGroup :: TestTree
repositoryGroup = testGroup "Repositories" [
        QC.testProperty
            "Generates the same hierarchy regardless of the order"
            prop_creates_thesame_hierarchy_regardless_of_shuffle
    ]

repositoriesURIs :: [RsyncRepository]
repositoriesURIs = map (\s -> RsyncRepository $ URI $ "rsync://host1.com/" <> s) $ [
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
            urisAreNotNested trees && 
            treesAreSorted trees && 
            List.all childURINestedInParent trees
        
        urisAreNotNested trees = 
            List.null $
                List.filter (\(t1, t2) -> nested t1 t2) $
                [ (t1, t2) | t1 <- trees, t2 <- trees, t1 /= t2]
            where 
                nested (RsyncTree (RsyncRepository u1) _) (RsyncTree (RsyncRepository u2) _) = 
                    u1 `isParentOf` u2 || u2 `isParentOf` u1

        treesAreSorted trees = 
            uris == sortedURIs &&
            List.all (\(RsyncTree _ children) -> treesAreSorted children) trees
            where 
                uris = map (\(RsyncTree (RsyncRepository u) _) -> u) trees
                sortedURIs = List.sort uris

        childURINestedInParent (RsyncTree (RsyncRepository uri') children) = 
            List.all (\(RsyncTree (RsyncRepository childUri) _) -> uri' `isParentOf` childUri) children &&
            List.all childURINestedInParent children
