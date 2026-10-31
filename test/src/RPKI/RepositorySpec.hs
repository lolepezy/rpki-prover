module RPKI.RepositorySpec where

import           Control.Lens hiding (elements)
import           Data.Generics.Labels ()
import qualified Data.Map.Strict                   as Map
import qualified Data.Map.Monoidal.Strict          as MonoidalMap
import qualified Data.Set                          as Set
import           Data.List (sort, isPrefixOf, find)
import qualified Data.List                         as List
import           Data.Text (Text)
import qualified Data.Text                         as Text

import           Data.Either (isRight)

import           Test.Tasty
import qualified Test.Tasty.HUnit                  as HU
import qualified Test.Tasty.QuickCheck             as QC
import           Test.QuickCheck.Gen

import           RPKI.Domain
import           RPKI.Repository
import           RPKI.Time
import           RPKI.Util
import           RPKI.Orphans


repositoryGroup :: TestTree
repositoryGroup = testGroup "PublicationPoints" [
        rsyncForestProperties,
        rsyncForestUnitTests,
        rrdpMapGroup,
        fetchStatusGroup,
        rsyncUrlSafetyGroup,
        rsyncCollapsingGroup
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


-- RsyncForest: properties checked against a trivial list-based model

rsyncForestProperties :: TestTree
rsyncForestProperties = testGroup "RsyncForest properties" [
        QC.testProperty "behaves as the list-based model for arbitrary insert sequences" $
            QC.forAll genInserts $ \inserts ->
                sort (flattenRsyncTree (buildForest inserts))
                    == sort (foldl (\m (u, a) -> modelInsert u a m) [] inserts),

        QC.testProperty "leaves never cover each other" $
            QC.forAll genInserts $ \inserts -> let
                leaves = map fst $ flattenRsyncTree (buildForest inserts)
                in and [ not (covers l l') | l <- leaves, l' <- leaves, l /= l' ],

        QC.testProperty "insertion order does not matter when the metadata is the same" $
            QC.forAll (QC.listOf genRsyncUrl) $ \urls ->
                QC.forAll (shuffle urls) $ \shuffled ->
                    buildForest (withMeta pendingMeta urls) == buildForest (withMeta pendingMeta shuffled),

        QC.testProperty "inserting the same URL twice is the same as inserting it once" $
            QC.forAll genInserts $ \inserts ->
                QC.forAll genInsert $ \(u, m) -> let
                    f = buildForest inserts
                    once = toRsyncForest u m f
                    in toRsyncForest u m once == once,

        QC.testProperty "inserting a URL makes it findable with the new metadata, unless an ancestor is already there" $
            QC.forAll genInserts $ \inserts ->
                QC.forAll genInsert $ \(u, m) -> let
                    before = buildForest inserts
                    ancestorBefore = fst <$> lookupInRsyncForest u before
                    after = toRsyncForest u m before
                    in case (ancestorBefore, lookupInRsyncForest u after) of
                        -- there was an ancestor (or the same URL), it is kept in place,
                        -- and it only gets new metadata if it is exactly this URL
                        (Just a, Just (a', m')) ->
                            a == a' && (a /= u || m' == m)
                        -- no ancestors: URL becomes a leaf itself
                        (Nothing, Just (u', m')) -> u' == u && m' == m
                        _ -> False,

        QC.testProperty "lookup agrees with the model for arbitrary probe URLs" $
            QC.forAll genInserts $ \inserts ->
                QC.forAll genRsyncUrl $ \probe -> let
                    forest = buildForest inserts
                    model  = foldl (\m (u, a) -> modelInsert u a m) [] inserts
                    in lookupInRsyncForest probe forest == find ((`covers` probe) . fst) model,

        QC.testProperty "every URL that was inserted can be found" $
            QC.forAll genInserts $ \inserts -> let
                forest = buildForest inserts
                in all (\(u, _) ->
                        maybe False (\(found, _) -> found `covers` u) (lookupInRsyncForest u forest))
                    inserts,

        QC.testProperty "a strict ancestor of a leaf is not found" $
            QC.forAll genInserts $ \inserts -> let
                forest = buildForest inserts
                leaves = map fst $ flattenRsyncTree forest
                in and [ lookupInRsyncForest (RsyncURL h (take n p)) forest == Nothing
                       | RsyncURL h p <- leaves, n <- [0 .. length p - 1] ]
    ]


rsyncForestUnitTests :: TestTree
rsyncForestUnitTests = testGroup "RsyncForest unit tests" [
        HU.testCase "only the topmost URLs remain as leaves" $ do
            let forest = buildForest $ withMeta pendingMeta (map rsyncUrl repositoryPaths)
            flattenRsyncTree forest HU.@?=
                [ (rsyncUrl p, pendingMeta) | p <- ["a", "b/a", "b/z", "different_root"] ],

        HU.testCase "lookup of a nested URL returns the covering leaf" $ do
            let forest = buildForest $ withMeta pendingMeta (map rsyncUrl repositoryPaths)
            fmap fst (lookupInRsyncForest (rsyncUrl "a/z/q/deeper/still") forest)
                HU.@?= Just (rsyncUrl "a")
            fmap fst (lookupInRsyncForest (rsyncUrl "b/a/c/d") forest)
                HU.@?= Just (rsyncUrl "b/a"),

        HU.testCase "lookup of a parent of a leaf, a sibling, an unknown host and an empty forest" $ do
            let forest = buildForest $ withMeta pendingMeta (map rsyncUrl repositoryPaths)
            lookupInRsyncForest (rsyncUrl "b") forest HU.@?= Nothing
            lookupInRsyncForest (rsyncUrl "") forest HU.@?= Nothing
            lookupInRsyncForest (rsyncUrl "b/y") forest HU.@?= Nothing
            lookupInRsyncForest (rsyncUrl "c") forest HU.@?= Nothing
            lookupInRsyncForest (rsyncUrlOn "host2.com" "a") forest HU.@?= Nothing
            lookupInRsyncForest (rsyncUrl "a") (newRsyncForest :: RsyncForest) HU.@?= Nothing,

        HU.testCase "a path chunk is not matched as a string prefix" $ do
            let forest = buildForest [(rsyncUrl "a/bc", pendingMeta)]
            lookupInRsyncForest (rsyncUrl "a/b") forest HU.@?= Nothing
            lookupInRsyncForest (rsyncUrl "a/bcd") forest HU.@?= Nothing,

        HU.testCase "adding a deeper URL below an existing leaf does not change the forest" $ do
            let forest = buildForest [(rsyncUrl "a", failedMeta)]
            toRsyncForest (rsyncUrl "a/b/c") fetchedMeta forest HU.@?= forest,

        HU.testCase "adding a shorter URL replaces everything below it" $ do
            let forest = buildForest [(rsyncUrl "a/b", failedMeta), (rsyncUrl "a/c/d", failedMeta), (rsyncUrl "z", failedMeta)]
            let forest' = toRsyncForest (rsyncUrl "a") fetchedMeta forest
            flattenRsyncTree forest' HU.@?=
                [ (rsyncUrl "a", fetchedMeta), (rsyncUrl "z", failedMeta) ],

        HU.testCase "re-adding the same URL updates its metadata" $ do
            let forest = buildForest [(rsyncUrl "a/b", pendingMeta)]
            flattenRsyncTree (toRsyncForest (rsyncUrl "a/b") fetchedMeta forest)
                HU.@?= [ (rsyncUrl "a/b", fetchedMeta) ],

        HU.testCase "updating one sibling leaves the others alone" $ do
            let forest = buildForest [(rsyncUrl "a/b", pendingMeta), (rsyncUrl "a/c", pendingMeta)]
            flattenRsyncTree (toRsyncForest (rsyncUrl "a/b") fetchedMeta forest)
                HU.@?= [ (rsyncUrl "a/b", fetchedMeta), (rsyncUrl "a/c", pendingMeta) ],

        HU.testCase "hosts that differ only by port are different hosts" $ do
            let withPort = RsyncURL (RsyncHost (RsyncHostName "host1.com") (Just (RsyncPort 8730))) []
            let forest = buildForest [(rsyncUrl "a", pendingMeta), (withPort, fetchedMeta)]
            length (flattenRsyncTree forest) HU.@?= 2
            fmap snd (lookupInRsyncForest withPort forest) HU.@?= Just fetchedMeta
            fmap snd (lookupInRsyncForest (rsyncUrl "a") forest) HU.@?= Just pendingMeta,

        HU.testCase "a URL without a path is the root of the host" $ do
            let forest = buildForest [(rsyncUrl "a", pendingMeta), (rsyncUrl "b/c", pendingMeta)]
            let forest' = toRsyncForest (rsyncUrl "") fetchedMeta forest
            flattenRsyncTree forest' HU.@?= [ (rsyncUrl "", fetchedMeta) ]
            fmap snd (lookupInRsyncForest (rsyncUrl "x/y/z") forest') HU.@?= Just fetchedMeta
    ]


rsyncCollapsingGroup :: TestTree
rsyncCollapsingGroup = testGroup "Collapsing rsync repositories into their common parent" [
        HU.testCase "many repositories in one deep directory collapse into it, not into the module" $ do
            let fs = mconcat [ rsyncFetcheable ("module/foo/bar/a" <> num n) | n <- [1 .. 100 :: Int] ]
            collapseRsyncFetcheables 10 fs HU.@?=
                (rsyncFetcheable "module/foo/bar", Map.fromList [(rsyncUrl "module/foo/bar", 100)]),

        HU.testCase "directories with one repository each collapse into their common parent" $ do
            -- e.g. rsync://rpki-rsync.idnic.net/repo/<uuid>/0
            let fs = mconcat [ rsyncFetcheable ("repo/u" <> num n <> "/0") | n <- [1 .. 12 :: Int] ]
            fetcheableUrls (fst $ collapseRsyncFetcheables 10 fs) HU.@?= [rsyncUrl "repo"],

        HU.testCase "a sibling of a collapsed directory stays as it is" $ do
            let fs = mconcat [ rsyncFetcheable ("module/foo/bar/a" <> num n) | n <- [1 .. 20 :: Int] ]
                  <> rsyncFetcheable "module/foo/b"
            fetcheableUrls (fst $ collapseRsyncFetcheables 10 fs) HU.@?= 
                [rsyncUrl "module/foo/b", rsyncUrl "module/foo/bar"],

        HU.testCase "enough collapsed directories collapse further" $ do
            let fs = mconcat [ rsyncFetcheable ("module/org" <> num o <> "/x" <> num x) 
                             | o <- [1 .. 10 :: Int], x <- [1 .. 10 :: Int] ]
            collapseRsyncFetcheables 10 fs HU.@?= 
                (rsyncFetcheable "module", Map.fromList [(rsyncUrl "module", 100)]),

        HU.testCase "a repository nested in another one is fetched with it and doesn't count" $ do
            let fs = rsyncFetcheable "repo/ca" <> mconcat [ rsyncFetcheable ("repo/ca/child" <> num n) | n <- [1 .. 20 :: Int] ]
            collapseRsyncFetcheables 10 fs HU.@?= (rsyncFetcheable "repo/ca", mempty),

        HU.testCase "the root of a host is never collapsed" $ do
            let fs = mconcat [ rsyncFetcheable ("module" <> num n <> "/a") | n <- [1 .. 20 :: Int] ]
            collapseRsyncFetcheables 10 fs HU.@?= (fs, mempty)
            length (fetcheableUrls (fst $ collapseRsyncFetcheables 1 fs)) HU.@?= 20,

        HU.testCase "threshold 0 changes nothing" $ do
            let fs = mconcat [ rsyncFetcheable ("repo/" <> c) | c <- ["a", "b", "c"] ]
            collapseRsyncFetcheables 0 fs HU.@?= (fs, mempty),

        HU.testCase "rsync fallbacks are collapsed as well and merged under their primary" $ do
            let rrdp = RrdpU (RrdpURL $ URI "https://rrdp.host1.com/notification.xml")
            let fs = mconcat [ newFetcheables rrdp (Just $ RsyncU $ rsyncUrl ("repo/" <> c)) | c <- ["a", "b", "c"] ]
            let (Fetcheables collapsed, _) = collapseRsyncFetcheables 3 fs
            MonoidalMap.toList collapsed HU.@?= [(rrdp, Set.singleton $ RsyncU $ rsyncUrl "repo")],

        QC.testProperty "every repository is fetched by exactly one fetch that covers it" $
            QC.forAll genThreshold $ \threshold ->
                QC.forAll (QC.listOf genRsyncUrl) $ \urls -> let
                    fetched = fetcheableUrls $ fst $ collapseRsyncFetcheables threshold (fetcheablesOf urls)
                    in all (\u -> length (filter (`covers` u) fetched) == 1) urls,

        QC.testProperty "fetches never cover each other and there are no more of them than before" $
            QC.forAll genThreshold $ \threshold ->
                QC.forAll (QC.listOf genRsyncUrl) $ \urls -> let
                    fetched = fetcheableUrls $ fst $ collapseRsyncFetcheables threshold (fetcheablesOf urls)
                    in and [ not (covers f f') | f <- fetched, f' <- fetched, f /= f' ]
                        && length fetched <= length (List.nub urls),

        QC.testProperty "an introduced directory replaces at least threshold repositories, never the host root" $
            QC.forAll genThreshold $ \threshold ->
                QC.forAll (QC.listOf genRsyncUrl) $ \urls -> let
                    introduced = snd $ collapseRsyncFetcheables threshold (fetcheablesOf urls)
                    in and [ count >= threshold 
                                && not (null path) 
                                && count == length (List.nub $ filter (covers parent) urls)
                           | (parent@(RsyncURL _ path), count) <- Map.toList introduced ],

        QC.testProperty "threshold 1 fetches whole modules" $
            QC.forAll (QC.listOf genRsyncUrl) $ \urls ->
                all (\(RsyncURL _ path) -> length path <= 1) $
                    fetcheableUrls $ fst $ collapseRsyncFetcheables 1 (fetcheablesOf urls)
    ]
  where
    rsyncFetcheable path = newFetcheables (RsyncU $ rsyncUrl path) Nothing
    fetcheablesOf urls = mconcat [ newFetcheables (RsyncU u) Nothing | u <- urls ]
    fetcheableUrls (Fetcheables fs) = [ u | RsyncU u <- MonoidalMap.keys fs ]
    num n = Text.pack (show n)
    genThreshold = QC.choose (1, 4)


-- RrdpMap and its merging

rrdpMapGroup :: TestTree
rrdpMapGroup = testGroup "RrdpMap" [
        QC.testProperty "is associative" $ \(a :: RrdpMap, b, c) ->
            a <> (b <> c) == (a <> b) <> c,

        QC.testProperty "is idempotent" $ \(a :: RrdpMap) -> a <> a == a,

        QC.testProperty "has the union of keys" $ \(RrdpMap a, RrdpMap b) ->
            Map.keys (unRrdpMap (RrdpMap a <> RrdpMap b)) == Map.keys (Map.union a b),

        QC.testProperty "never loses the most recent status of a repository" $ \(RrdpMap a, RrdpMap b) -> let
            merged = unRrdpMap (RrdpMap a <> RrdpMap b)
            statusOf m k = (\r -> r ^. #meta . #status) <$> Map.lookup k m
            expected k = maximum $ [ s | Just s <- [statusOf a k, statusOf b k] ]
            in all (\k -> statusOf merged k == Just (expected k)) (Map.keys merged),

        QC.testProperty "mergeRrdp never makes the status older" $ \(r :: RrdpRepository, existing :: RrdpRepository) -> let
            existing' = existing & #uri .~ (r ^. #uri)
            pps = PublicationPoints (RrdpMap (Map.singleton (r ^. #uri) existing')) newRsyncForest
            merged = rrdpRepository (mergeRrdp r pps) (r ^. #uri)
            in fmap (\x -> x ^. #meta . #status) merged
                    == Just (max (r ^. #meta . #status) (existing' ^. #meta . #status)),

        QC.testProperty "mergeRrdp adds a repository that was not there" $ \(r :: RrdpRepository) ->
            rrdpRepository (mergeRrdp r (PublicationPoints (RrdpMap mempty) newRsyncForest)) (r ^. #uri) == Just r,

        HU.testCase "mergePP for rsync adds a new URL as Pending" $ do
            let pps = PublicationPoints (RrdpMap mempty) newRsyncForest
            let u = rsyncUrl "a/b"
            fmap (\r -> r ^. #meta) (rsyncRepository (mergePP (rsyncPP u) pps) u) 
                HU.@?= Just pendingMeta,

        HU.testCase "mergePP for rsync does not reset metadata of the exact same repository" $ do
            let u = rsyncUrl "a/b"
            let pps = PublicationPoints (RrdpMap mempty) (toRsyncForest u fetchedMeta newRsyncForest)
            fmap (\r -> r ^. #meta) (rsyncRepository (mergePP (rsyncPP u) pps) u) 
                HU.@?= Just fetchedMeta,

        HU.testCase "mergePP for rsync does not touch metadata of a covering repository" $ do
            let pps = PublicationPoints (RrdpMap mempty) (toRsyncForest (rsyncUrl "a") fetchedMeta newRsyncForest)
            let merged = mergePP (rsyncPP (rsyncUrl "a/b/c")) pps
            rsyncs merged HU.@?= rsyncs pps
    ]


fetchStatusGroup :: TestTree
fetchStatusGroup = testGroup "FetchStatus ordering" [
        HU.testCase "Pending is older than any fetch result" $ do
            HU.assertBool "pendingMeta < failedMeta"  $ Pending < FailedAt (Instant 1)
            HU.assertBool "pendingMeta < fetchedMeta" $ Pending < FetchedAt (Instant 1),

        HU.testCase "later result wins regardless of its kind" $ do
            HU.assertBool "failedMeta later" $ FetchedAt (Instant 1) < FailedAt (Instant 2)
            HU.assertBool "fetchedMeta later" $ FailedAt (Instant 1) < FetchedAt (Instant 2),

        HU.testCase "success wins over failure at the same moment" $
            HU.assertBool "fetchedMeta > failedMeta" $ FailedAt (Instant 1) < FetchedAt (Instant 1)
    ]


-- Helpers

pendingMeta, failedMeta, fetchedMeta :: RepositoryMeta
pendingMeta = RepositoryMeta Pending Nothing
failedMeta  = RepositoryMeta (FailedAt (Instant 100)) Nothing
fetchedMeta = RepositoryMeta (FetchedAt (Instant 200)) Nothing

repositoriesURIs :: [RsyncPublicationPoint]
repositoriesURIs = map (RsyncPublicationPoint . rsyncUrl) repositoryPaths

repositoryPaths :: [Text]
repositoryPaths = [
        "a", "a/b", "a/c", "a/z", "a/z/q", "a/z/q/zzz", "a/z/q/aa", "a/z/p/q",
        "b/a", "b/a/c", "a/z/q", "b/a/d", "b/a/e", "b/z", "different_root"
    ]

rsyncUrl :: Text -> RsyncURL
rsyncUrl = rsyncUrlOn "host1.com"

rsyncUrlOn :: Text -> Text -> RsyncURL
rsyncUrlOn host path =
    RsyncURL (RsyncHost (RsyncHostName host) Nothing)
             [ RsyncPathChunk c | c <- Text.splitOn "/" path, not (null' c) ]
  where
    null' = (== "")

buildForest :: [(RsyncURL, RepositoryMeta)] -> RsyncForest
buildForest = foldl (\f (u, m) -> toRsyncForest u m f) newRsyncForest

withMeta :: RepositoryMeta -> [RsyncURL] -> [(RsyncURL, RepositoryMeta)]
withMeta m = map (, m)

-- l covers u when l is the same as u or an ancestor of u
covers :: RsyncURL -> RsyncURL -> Bool
covers (RsyncURL h p) (RsyncURL h' p') = h == h' && p `isPrefixOf` p'

-- Reference implementation of what the forest has to do:
--  * if a leaf already covers the URL, only a leaf with exactly the same URL gets new metadata,
--  * otherwise the URL becomes a leaf and swallows all the leaves below it.
modelInsert :: RsyncURL -> RepositoryMeta -> [(RsyncURL, RepositoryMeta)] -> [(RsyncURL, RepositoryMeta)]
modelInsert u m model =
    case find ((`covers` u) . fst) model of
        Just (l, _)
            | l == u    -> [ if l' == u then (l', m) else x | x@(l', _) <- model ]
            | otherwise -> model
        Nothing -> (u, m) : [ x | x@(l, _) <- model, not (u `covers` l) ]


-- Generators. The alphabet is deliberately tiny, so that the URLs
-- often are prefixes of each other, otherwise the interesting cases are hit too rarely.

genRsyncUrl :: Gen RsyncURL
genRsyncUrl = do
    host <- frequency [
                (4, pure $ RsyncHost (RsyncHostName "host1.com") Nothing),
                (2, pure $ RsyncHost (RsyncHostName "host2.com") Nothing),
                (1, pure $ RsyncHost (RsyncHostName "host1.com") (Just (RsyncPort 8730)))
            ]
    len  <- frequency [ (1, pure 0), (3, pure 1), (4, pure 2), (3, pure 3), (2, pure 4) ]
    path <- vectorOf len (RsyncPathChunk <$> elements ["a", "b", "ab"])
    pure $ RsyncURL host path

genMeta :: Gen RepositoryMeta
genMeta = elements [ pendingMeta, failedMeta, fetchedMeta ]

genInsert :: Gen (RsyncURL, RepositoryMeta)
genInsert = (,) <$> genRsyncUrl <*> genMeta

genInserts :: Gen [(RsyncURL, RepositoryMeta)]
genInserts = scale (* 2) $ listOf genInsert
