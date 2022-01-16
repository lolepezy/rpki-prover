{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module RPKI.RepositorySpec where

import qualified Data.ByteString.Short    as BSS

import           GHC.Generics

import           Test.Tasty
import           Test.QuickCheck.Arbitrary.Generic
import qualified Test.Tasty.HUnit                  as HU
import qualified Test.Tasty.QuickCheck             as QC

import           Test.QuickCheck.Gen

import           RPKI.Domain
import           RPKI.Repository
import           RPKI.Util 
import           RPKI.Orphans
import Data.Maybe (maybeToList, catMaybes)
import Control.Monad (replicateM)
import Data.List (sort)


repositoryGroup :: TestTree
repositoryGroup = testGroup "PublicationPoints" [
        QC.testProperty
            "Generates the same hierarchy regardless of the order"
            prop_creates_same_hierarchy_regardless_of_shuffle_map,

        QC.testProperty
            "Make sure RsyncMap is a commutative"
            prop_rsync_map_is_commutative,

        QC.testProperty
            "RsyncTree is commutative"
            prop_rsync_tree_commutative,

        QC.testProperty
            "Make sure RsyncMap is a semigroup (for a list of URLs)"
            prop_rsync_map_is_a_semigroup,

        QC.testProperty "FetchStatus is a semigroup" $ isASemigroup @FetchStatus,
        QC.testProperty "PublicationPoints is a semigroup" $ isASemigroup @PublicationPoints,
        QC.testProperty "RrdpRepository is a semigroup" $ isASemigroup @RrdpRepository,
        QC.testProperty "RsyncMap is a semigroup" $ isASemigroup @RsyncMap,
        QC.testProperty "RrdpMap is a semigroup" $ isASemigroup @RrdpMap            
    ]

isASemigroup :: Eq s => Semigroup s => (s, s, s) -> Bool
isASemigroup (s1, s2, s3) = s1 <> (s2 <> s3) == (s1 <> s2) <> s3

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

prop_rsync_map_is_commutative :: QC.Property
prop_rsync_map_is_commutative = 
    QC.forAll (QC.sublistOf repositoriesURIs) $ \rs1 ->         
        QC.forAll (QC.sublistOf repositoriesURIs) $ \rs2 ->                     
            let 
                rm1 = fromRsyncPPs rs1
                rm2 = fromRsyncPPs rs2                    
            in rm1 <> rm2 == rm2 <> rm1

prop_rsync_tree_commutative :: QC.Property
prop_rsync_tree_commutative = 
    QC.forAll (replicateM 200 generateRsyncPath) $ \urls -> 
        convertToRepos (sort urls) Pending == convertToRepos urls Pending

prop_rsync_tree_update :: QC.Property
prop_rsync_tree_update = 
    QC.forAll arbitrary $ \(newStatus :: FetchStatus) ->
        QC.forAll (replicateM 200 generateRsyncPath) $ \urls -> 
            QC.forAll (QC.sublistOf urls) $ \toUpdate ->         
                let 
                    tree = convertToRepos urls

                in True
    

convertToRepos :: [[BSS.ShortByteString]] -> FetchStatus -> RsyncRepos
convertToRepos urls status = foldr mergeToTree newTree urls 
  where          
    mergeToTree (toHostPath -> (host, path)) tree = 
        toTree host path status tree


toHostPath :: [BSS.ShortByteString] -> (RsyncHost, [RsyncPathChunk])
toHostPath [] = error "toHostPath: should never happen"
toHostPath (host : path) = (RsyncHost host, map RsyncPathChunk path)

generateRsyncPath :: Gen [BSS.ShortByteString]
generateRsyncPath = do 
    let hosts  = [ "rrdp.ripe.net", "ca.rg.net", "rpki-repository.nic.ad.jp", "repo-rpki.idnic.net" ]
    let level1 = Nothing : map Just [ "repo", "repository", "0", "A91A73810000", "member_repository" ]
    let levelChunks = map (replicate 5) ['a'..'z']
    let level2 = replicate 5 Nothing  <> map Just levelChunks
    let level3 = replicate 10 Nothing <> map Just levelChunks    
    h <- elements hosts    
    l1 <- elements level1
    l2 <- elements level2
    l3 <- elements level3
    pure $ map (BSS.toShort . convert) $ h : catMaybes [l1, l2, l3]    