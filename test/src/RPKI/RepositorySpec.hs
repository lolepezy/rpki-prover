{-# LANGUAGE OverloadedStrings #-}

module RPKI.RepositorySpec where

import Control.Monad (replicateM)

import Data.Maybe (catMaybes)
import Data.List (sort, isPrefixOf, sortOn)

import           Data.Either (isRight)
import qualified Data.Text                         as Text

import           Test.Tasty
import           Test.QuickCheck.Arbitrary.Generic
import qualified Test.Tasty.HUnit                  as HU
import qualified Test.Tasty.QuickCheck             as QC

import           Test.QuickCheck.Gen

import           RPKI.Domain
import           RPKI.Repository
import           RPKI.Util
import           RPKI.Orphans ()


repositoryGroup :: TestTree
repositoryGroup = testGroup "PublicationPoints" [
        QC.testProperty
            "RsyncTree is commutative"
            prop_rsync_tree_commutative,

        QC.testProperty
            "RsyncTree gets properly updated"
            prop_rsync_tree_update,
    
        -- QC.testProperty "FetchStatus is a semigroup" $ isASemigroup @FetchStatus,
        -- QC.testProperty "RrdpRepository is a semigroup" $ isASemigroup @RrdpRepository,
        -- QC.testProperty "RrdpRepository is a semigroup" $ isASemigroup @RepositoryMeta,
        QC.testProperty "RrdpMap is a semigroup" $ isASemigroup @RrdpMap,

        rsyncUrlSafetyGroup
    ]

{- | Rsync URLs are turned into local filesystem paths by `rsyncDestination`, and 
   the rsync client is invoked with --delete on the resulting directory. A URL 
   whose path escapes the rsync root would therefore let a CA certificate point 
   rsync at an arbitrary directory on the host.

   Note that `modern-uri` percent-decodes path pieces and does not remove 
   dot-segments, so both the literal and the encoded forms have to be rejected.
-}
rsyncUrlSafetyGroup :: TestTree
rsyncUrlSafetyGroup = testGroup "Rsync URL path safety" [
        HU.testCase "Accepts ordinary rsync URLs" $ do
            accepted "rsync://rpki.example.com/repository/"
            accepted "rsync://rpki.example.com/repo/subdir/object.cer"
            accepted "rsync://rpki.example.com:8730/repo/"
            accepted "rsync://rpki.example.com/a.b/c-d_e~f/",

        HU.testCase "Rejects dot-segments in the path" $ do
            rejected "rsync://rpki.example.com/a/../../../../etc/cron.d/"
            rejected "rsync://rpki.example.com/../etc/"
            rejected "rsync://rpki.example.com/./repo/"
            rejected "rsync://rpki.example.com/repo/..",

        HU.testCase "Rejects percent-encoded dot-segments" $ do
            rejected "rsync://rpki.example.com/a/%2e%2e/%2e%2e/etc/"
            rejected "rsync://rpki.example.com/%2E%2E/etc/",

        HU.testCase "Rejects percent-encoded path separators" $ do
            rejected "rsync://rpki.example.com/%2Fetc%2Fcron.d/"
            rejected "rsync://rpki.example.com/repo%2f..%2f..%2fetc/"
            rejected "rsync://rpki.example.com/repo/a%5Cb/",

        HU.testCase "Rejects a NUL byte in the path" $
            rejected "rsync://rpki.example.com/repo%00/",

        HU.testCase "Every accepted path chunk is a single safe path segment" $
            HU.assertBool "chunks must be usable as file names" $
                all safeChunks [ "rsync://rpki.example.com/repository/"
                               , "rsync://rpki.example.com/a/b/c/d.cer"
                               , "rsync://rpki.example.com:873/x/" ]
    ]
  where
    accepted u =
        HU.assertBool ("Should have accepted " <> Text.unpack u) $
            isRight $ parseRsyncURL u

    rejected u =
        case parseRsyncURL u of
            Left _  -> pure ()
            Right r -> HU.assertFailure $
                "Should have rejected " <> Text.unpack u <> ", but got " <> show r

    safeChunks u =
        case parseRsyncURL u of
            Left _                     -> False
            Right (RsyncURL _ chunks)  -> all isSafe chunks
      where
        isSafe (RsyncPathChunk c) =
            not (Text.null c)
                && c /= "." && c /= ".."
                && not (Text.any (\ch -> ch == '/' || ch == '\\' || ch == '\0') c)

isASemigroup :: Eq s => Semigroup s => (s, s, s) -> Bool
isASemigroup (s1, s2, s3) = s1 <> (s2 <> s3) == (s1 <> s2) <> s3

repositoriesURIs :: [RsyncPublicationPoint]
repositoriesURIs = map (RsyncPublicationPoint . toURL) [
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
  where
    toURL path = 
        either (\e -> error $ "Bad rsync URL in test data: " <> show e) id 
            $ parseRsyncURL ("rsync://host1.com/" <> path)

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
                allShorter = map (\(RsyncURL h p) -> 
                                    filter (\(RsyncURL h' p') -> 
                                        h == h' && p /= p' && p' `isPrefixOf` p) urls) toUpdate
                sameOrShorter =
                    zipWith (\original shorterOnes -> 
                        (case take 1 $ sortOn (\(RsyncURL _ p) -> length p) shorterOnes of
                                []   -> original
                                s :_ -> s)) 
                        toUpdate allShorter
                updatedTree = foldr (\u t -> toRsyncForest u (newMeta newStatus) t) tree sameOrShorter
                sameOrLonger = filter (\(RsyncURL h p) -> 
                                    any (\(RsyncURL h' p') -> 
                                        h == h' && (p == p' || p' `isPrefixOf` p)) toUpdate) urls
                in all (\url -> 
                    fmap snd (lookupInRsyncForest url updatedTree) == 
                        Just (newMeta newStatus)) sameOrLonger
    

convertToRepos :: [RsyncURL] -> FetchStatus -> RsyncForest
convertToRepos urls status = 
    foldr (\u t -> toRsyncForest u (newMeta status) t) newRsyncForestGen urls


generateRsyncUrl :: Gen RsyncURL
generateRsyncUrl = do
    let hosts  = [ "rrdp.ripe.net", "ca.rg.net", "rpki-repository.nic.ad.jp", "repo-rpki.idnic.net" ]
    let level1 = Nothing : map Just [ "repo", "repository", "0", "A91A73810000", "member_repository" ]
    let levelChunks = map (replicate 5) ['a'..'z']
    let level2 = replicate 5 Nothing  <> map Just levelChunks
    let level3 = replicate 10 Nothing <> map Just levelChunks
    host <- elements hosts
    pathLevels <- catMaybes <$> mapM elements [level1, level2, level3]
    let rsyncHost = RsyncHost (RsyncHostName host) Nothing
    let path = map (RsyncPathChunk . convert) pathLevels
    pure $ RsyncURL rsyncHost path


newMeta :: FetchStatus -> RepositoryMeta
newMeta status = let      
    refreshInterval = Nothing
    in RepositoryMeta {..}
