{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module RPKI.RepositorySpec where

import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import           RPKI.Domain
import           RPKI.Repository
import           RPKI.Orphans

import           RPKI.Orphans


repositoryGroup :: TestTree
repositoryGroup = testGroup "PublicationPoints" [
        QC.testProperty
            "Generates the same hierarchy regardless of the order"
            prop_creates_same_hierarchy_regardless_of_shuffle_map,
        QC.testProperty
            "Make sure RsyncMap is a semigroup"
            prop_rsync_map_is_a_semigroup,

        QC.testProperty "FetchStatus is a semigroup" $ is_a_semigroup @FetchStatus,
        QC.testProperty "PublicationPoints is a semigroup" $ is_a_semigroup @PublicationPoints,
        QC.testProperty "RrdpRepository is a semigroup" $ is_a_semigroup @RrdpRepository,
        QC.testProperty "RsyncMap is a semigroup" $ is_a_semigroup @RsyncMap,
        QC.testProperty "RrdpMap is a semigroup" $ is_a_semigroup @RrdpMap            
    ]

is_a_semigroup :: Eq s => Semigroup s => (s, s, s) -> Bool
is_a_semigroup (s1, s2, s3) = s1 <> (s2 <> s3) == (s1 <> s2) <> s3

repositoriesURIs :: [RsyncPublicationPoint]
repositoriesURIs = map (RsyncPublicationPoint . RsyncURL . URI . ("rsync://host1.com/" <>)) [
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


prop_creates_same_hierarchy_regardless_of_shuffle_map :: QC.Property
prop_creates_same_hierarchy_regardless_of_shuffle_map = 
    QC.forAll (QC.shuffle repositoriesURIs) $ \rs ->         
        fromRsyncPPs rs == initialMap
    where
        initialMap = fromRsyncPPs repositoriesURIs 

prop_rsync_map_is_a_semigroup :: QC.Property
prop_rsync_map_is_a_semigroup = 
    QC.forAll (QC.sublistOf repositoriesURIs) $ \rs1 ->         
        QC.forAll (QC.sublistOf repositoriesURIs) $ \rs2 ->         
            QC.forAll (QC.sublistOf repositoriesURIs) $ \rs3 ->         
                let 
                    rm1 = fromRsyncPPs rs1
                    rm2 = fromRsyncPPs rs2
                    rm3 = fromRsyncPPs rs3
                    in rm1 <> (rm2 <> rm3) == (rm1 <> rm2) <> rm3    

