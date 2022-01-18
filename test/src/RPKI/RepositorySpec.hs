{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module RPKI.RepositorySpec where

import Control.Monad (replicateM)

import Data.ByteString.Short (toShort)
import Data.Maybe (maybeToList, catMaybes)
import Data.List (sort, isPrefixOf, sortOn)

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
            "RsyncTree gets properly updated"
            prop_rsync_tree_update,

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
    QC.forAll (replicateM 200 generateRsyncUrl) $ \urls ->
        convertToRepos (sort urls) Pending == convertToRepos urls Pending

prop_rsync_tree_update :: QC.Property
prop_rsync_tree_update =
    QC.forAll arbitrary $ \(newStatus :: FetchStatus) ->
        QC.forAll (replicateM 100 generateRsyncUrl) $ \urls ->
            QC.forAll (QC.sublistOf urls) $ \toUpdate -> let
                tree = convertToRepos urls Pending
                -- this messy stuff basically means "try to find the shortest URLs to update"
                -- and "don't update a longer one if a shorter one exists".
                allShorter = map (\(h, p) -> filter (\(h', p') -> h == h' && p /= p' && p' `isPrefixOf` p) urls) toUpdate
                sameOrShorter =
                    zipWith (\original shorterOnes -> 
                        (case take 1 $ sortOn (length . snd) shorterOnes of
                                []   -> original
                                s :_ -> s)) 
                        toUpdate allShorter
                updatedTree = foldr (\(host, path) t -> toTree host path newStatus t) tree sameOrShorter
                sameOrLonger = filter (\(h, p) -> any (\(h', p') -> h == h' && (p == p' || p' `isPrefixOf` p)) toUpdate) urls
                in all (\(host, path) -> fetchStatusInTree host path updatedTree == newStatus) sameOrLonger


convertToRepos :: [(RsyncHost, [RsyncPathChunk])] -> FetchStatus -> RsyncRepos
convertToRepos urls status = foldr mergeToTree newTree urls
  where
    mergeToTree (host, path) tree = toTree host path status tree


generateRsyncUrl :: Gen (RsyncHost, [RsyncPathChunk])
generateRsyncUrl = do
    let hosts  = [ "rrdp.ripe.net", "ca.rg.net", "rpki-repository.nic.ad.jp", "repo-rpki.idnic.net" ]
    let level1 = Nothing : map Just [ "repo", "repository", "0", "A91A73810000", "member_repository" ]
    let levelChunks = map (replicate 5) ['a'..'z']
    let level2 = replicate 5 Nothing  <> map Just levelChunks
    let level3 = replicate 10 Nothing <> map Just levelChunks
    host <- elements hosts
    pathLevels <- catMaybes <$> mapM elements [level1, level2, level3]
    let rsyncHost = RsyncHost $ toShort host
    let path = map (RsyncPathChunk . toShort . convert) pathLevels
    pure (rsyncHost, path)  