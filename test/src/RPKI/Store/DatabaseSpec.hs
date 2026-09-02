{-# LANGUAGE OverloadedStrings #-}

module RPKI.Store.DatabaseSpec where

import           Control.Concurrent              (threadDelay)
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Data.Generics.Product.Typed

import qualified Data.ByteString                   as BS
import qualified Data.List                         as List
import qualified Data.Map.Strict                   as Map
import qualified Data.Set                          as Set
import qualified Data.Text                         as Text
import           Data.Proxy                        (Proxy(..))
import           Data.Int                          (Int64)

import           Database.SQLite.Simple            (Only(..), execute, query, query_)

import           Test.Tasty
import qualified Test.Tasty.HUnit                  as HU
import qualified Test.Tasty.QuickCheck             as QC

import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.AppState                   (instantToVersion)
import           RPKI.Domain
import           RPKI.Parse.Parse
import           RPKI.Reporting
import           RPKI.Repository
import           RPKI.RepositorySpec
import           RPKI.Store.AppStorage
import           RPKI.Store.Base.Storable
import           RPKI.Store.Database               (DB(..), Tx(..), roTx, rwTx)
import qualified RPKI.Store.Database               as DB
import           RPKI.Validation.ObjectValidation
import qualified RPKI.Store.SQLite                 as SQLite
import           RPKI.Store.Types
import           RPKI.TestCommons
import           RPKI.Time
import           RPKI.Util


databaseGroup :: TestTree
databaseGroup = testGroup "SQLite storage tests"
    [ objectStoreGroup
    , repositoryStoreGroup
    , versionStoreGroup
    , txGroup
    , dbGroup
    ]

objectStoreGroup :: TestTree
objectStoreGroup = testGroup "Object storage test"
    [ dbTestCase "Should order manifests according to their dates" shouldOrderManifests
    , dbTestCase "Should merge locations" shouldMergeObjectLocations
    , dbTestCase "Should deduplicate saveObject by hash" shouldDeduplicateSaveObjectByHash
    , dbTestCase "Should index certificates on saveObject" shouldIndexCertificateOnSaveObject
    ]

repositoryStoreGroup :: TestTree
repositoryStoreGroup = testGroup "Repository storage test"
    [ dbTestCase "Should insert and get an rsync repository" shouldSaveAndGetRsyncRepositories
    , dbTestCase "Should overwrite metadata and validations" shouldSaveMetaAndValidationAsCorrectSemigroup
    ]

versionStoreGroup :: TestTree
versionStoreGroup = testGroup "Version storage test"
    [ dbTestCase "Should insert and get a version" shouldSaveAndGetValidationVersion
    , dbTestCase "Should insert and get a version with data from previous versions"
        shouldSaveAndGetValidationVersionFilledWithPastData
    , dbTestCase "Should read payload getters with fallback and active TA filtering"
        shouldReadValidationOutcomePayloadQueries
    , dbTestCase "Should keep versions ordered and resolve previous version" shouldOrderAndLinkVersions
    , dbTestCase "Should delete version data including slurm" shouldDeleteValidationVersionData
    , dbTestCase "Should delete oldest versions once every TA has enough real data"
        shouldDeleteOldestVersionsOnceEveryTAHasEnoughRealData
    , dbTestCase "Should not delete anything while a TA never accumulates enough real data"
        shouldNotDeleteVersionsBlockedByLaggingTA
    ]

txGroup :: TestTree
txGroup = testGroup "App transaction test"
    [ dbTestCase "Should rollback App transactions properly" shouldRollbackAppTx
    , dbTestCase "Should preserve state from StateT in transactions" shouldPreserveStateInAppTx
    ]

dbGroup :: TestTree
dbGroup = testGroup "App database test"
    [ HU.testCase "Should reopen database without issues" shouldReopenDatabase
    ]


shouldMergeObjectLocations :: IO DB -> HU.Assertion
shouldMergeObjectLocations io = do
    db <- io
    Now now <- thisInstant

    [url1, url2, url3] :: [RpkiURL] <- take 3 . List.nub <$> replicateM 10 (QC.generate QC.arbitrary)

    ro1 :: ParsedRpkiObject <- QC.generate QC.arbitrary
    ro2 :: ParsedRpkiObject <- QC.generate QC.arbitrary

    let storeIt obj url = rwTx db $ \tx -> do
            k <- DB.saveObject tx db
                (OriginalRO (ObjectOriginal $ unStorable $ toStorable obj)
                            mempty
                            (getHash obj)
                            (getRpkiObjectType obj))
                (instantToVersion now)
            DB.linkObjectToUrl tx db url k

    let getIt h = roTx db $ \tx -> DB.getByHash tx db h

    storeIt ro1 url1
    storeIt ro1 url2
    storeIt ro1 url3
    storeIt ro2 url3

    Just (Located loc1 _) <- getIt (getHash ro1)
    HU.assertEqual "Wrong locations 1" loc1 (toLocations url1 <> toLocations url2 <> toLocations url3)

    Just (Located loc2 _) <- getIt (getHash ro2)
    HU.assertEqual "Wrong locations 2" loc2 (toLocations url3)

    verifyUrlCount db "case 1" 3

    rwTx db $ \tx -> DB.deleteObjectByHash tx db (getHash ro1)

    verifyUrlCount db "case 2" 3

    deletedUrls <- rwTx db DB.deleteDanglingUrls
    HU.assertEqual "Should have deleted 2 URLs" 2 deletedUrls

    verifyUrlCount db "case 3" 1

    Just (Located loc2' _) <- getIt (getHash ro2)
    HU.assertEqual "Wrong locations 3" loc2' (toLocations url3)
  where
    verifyUrlCount db' suffix expected = do
        actual <- roTx db' $ \(Tx conn) -> do
            rows <- query_ conn "SELECT COUNT(*) FROM urls" :: IO [Only Int]
            pure $ case rows of
                [Only n] -> n
                _        -> 0
        HU.assertEqual ("Wrong URL count " <> suffix) expected actual


shouldOrderManifests :: IO DB -> HU.Assertion
shouldOrderManifests io = do
    db <- io
    (Right (url1, mft1), _) <- runValidatorT (newScopes "read1") $ readObjectFromFile "./test/data/afrinic_mft1.mft"
    (Right (url2, mft2), _) <- runValidatorT (newScopes "read2") $ readObjectFromFile "./test/data/afrinic_mft2.mft"

    worldVersion <- newVersion

    rwTx db $ \tx -> do
        key1 <- DB.saveObject tx db (WellStructuredRO $ toValidatedRpkiObject mft1) worldVersion
        key2 <- DB.saveObject tx db (WellStructuredRO $ toValidatedRpkiObject mft2) worldVersion
        DB.linkObjectToUrl tx db url1 key1
        DB.linkObjectToUrl tx db url2 key2

    let Just aki1 = getAKI mft1
    [m1, m2] <- roTx db $ \tx -> DB.getMftsForAKI tx db aki1
    HU.assertBool "Manifests must be ordered by timing" (m1 ^. #nextTime >= m2 ^. #nextTime)

    Just (Keyed (Located _ mftLatest) _) <- roTx db $ \tx -> do
        MftMeta{..} : _ <- DB.getMftsForAKI tx db aki1
        DB.getMftByKey tx db key

    HU.assertEqual "Not the same manifests" (MftRO mftLatest) (toValidatedRpkiObject mft2)


shouldDeduplicateSaveObjectByHash :: IO DB -> HU.Assertion
shouldDeduplicateSaveObjectByHash io = do
    db <- io
    ro :: ParsedRpkiObject <- QC.generate QC.arbitrary
    let lifecycle = OriginalRO
            (ObjectOriginal $ unStorable $ toStorable ro)
            mempty
            (getHash ro)
            (getRpkiObjectType ro)

    wv1 <- newVersion
    threadDelay 10_000
    wv2 <- newVersion

    k1 <- rwTx db $ \tx -> DB.saveObject tx db lifecycle wv1
    k2 <- rwTx db $ \tx -> DB.saveObject tx db lifecycle wv2

    HU.assertEqual "Saving the same hash twice must return the same key" k1 k2

    rows <- roTx db $ \(Tx conn) ->
        query conn "SELECT COUNT(*) FROM objects WHERE hash = ?"
            (Only (SQLite.hashToBlob (getHash ro))) :: IO [Only Int64]

    let objectsWithHash = case rows of
            [Only n] -> n
            _        -> 0
    HU.assertEqual "Only one object row must exist for the hash" 1 objectsWithHash

    meta <- roTx db $ \tx -> DB.getObjectMeta tx db k1
    HU.assertEqual "Object metadata must come from the first insert"
        (Just $ ObjectMeta wv1 (getRpkiObjectType ro))
        meta


shouldIndexCertificateOnSaveObject :: IO DB -> HU.Assertion
shouldIndexCertificateOnSaveObject io = do
    db <- io
    cert <- QC.generate QC.arbitrary :: IO CaCerObject
    url <- QC.generate QC.arbitrary :: IO RpkiURL
    wv <- newVersion
    let wsCert = extractCert cert
    let ro = WellStructuredRO $ CerRO wsCert

    key <- rwTx db $ \tx -> do
        k <- DB.saveObject tx db ro wv
        DB.linkObjectToUrl tx db url k
        pure k

    bySki <- roTx db $ \tx -> DB.getBySKI tx db (getSKI wsCert)
    HU.assertBool "Certificate key must be indexed by SKI" (not $ null bySki)

    rows <- roTx db $ \(Tx conn) ->
        query conn "SELECT COUNT(*) FROM certificates WHERE object_key = ?"
            (Only (SQLite.toInt64 key)) :: IO [Only Int64]
    let certRows = case rows of
            [Only n] -> n
            _        -> 0
    HU.assertEqual "Exactly one certificates row must be created" 1 certRows

    fetched <- roTx db $ \tx -> DB.getFirstCaCertBySKI tx db (getSKI wsCert)
    case fetched of
        Just (Located _ fetchedCert) ->
            HU.assertEqual "Fetched cert by SKI must match the inserted cert" wsCert fetchedCert
        Nothing ->
            HU.assertFailure "Expected getFirstCaCertBySKI to return inserted certificate"


shouldSaveAndGetRsyncRepositories :: IO DB -> HU.Assertion
shouldSaveAndGetRsyncRepositories io = do
    db <- io

    repositories <- (<>)
        <$> replicateM 100 (QC.generate QC.arbitrary)
        <*> rsyncReposWithCommonHosts 100

    rwTx db $ \tx -> DB.saveRsyncRepositories tx db repositories

    let urls = [ u | r <- repositories, RsyncU u <- [getRpkiURL $ RsyncR r] ]
    repositories' <- roTx db $ \tx -> DB.getRsyncRepositories tx db urls

    HU.assertEqual "Not the same set of rsync repositories"
        (Set.fromList repositories)
        (Set.fromList $ Map.elems repositories')


shouldSaveMetaAndValidationAsCorrectSemigroup :: IO DB -> HU.Assertion
shouldSaveMetaAndValidationAsCorrectSemigroup io = do
    db <- io
    [rsync1] <- rsyncReposWithCommonHosts 1

    testOneRepository db rsync1

    Now now <- thisInstant
    testOneRepository db $ rsync1 & #meta . #status .~ FetchedAt now
    testOneRepository db $ rsync1 & #meta . #status .~ FailedAt now
    testOneRepository db $ rsync1 & #meta . #status .~ Pending
  where
    testOneRepository db rsync = do
        rwTx db $ \tx -> DB.saveRsyncRepositories tx db [rsync]
        let RsyncU url = getRpkiURL $ RsyncR rsync
        rs <- roTx db $ \tx -> DB.getRsyncRepositories tx db [url]
        let Just r = Map.lookup url rs
        HU.assertEqual "Same repository" r rsync


rsyncReposWithCommonHosts :: Int -> IO [RsyncRepository]
rsyncReposWithCommonHosts n =
    replicateM n $ QC.generate $ do
        hostName <- RsyncHostName <$> QC.elements ["host1", "host2", "host3"]
        let host = RsyncHost hostName Nothing
        pathChunks <- do
            n' <- QC.choose (1, 3)
            replicateM n' QC.arbitrary
        RsyncRepository (RsyncPublicationPoint $ RsyncURL host pathChunks) <$> QC.arbitrary


generateRepositories :: IO PublicationPoints
generateRepositories = do
    rrdpMap :: RrdpMap <- QC.generate QC.arbitrary
    let pps = PublicationPoints rrdpMap newRsyncForest
    pure $ List.foldr mergeRsyncPP pps repositoriesURIs


rrdpSubMap :: PublicationPoints -> IO RrdpMap
rrdpSubMap pps = do
    let RrdpMap rrdpsM = rrdps pps
    keys_ <- QC.generate (QC.sublistOf $ Map.keys rrdpsM)
    pure $ RrdpMap $ Map.filterWithKey (\u _ -> u `elem` keys_) rrdpsM


shouldSaveAndGetValidationVersion :: IO DB -> HU.Assertion
shouldSaveAndGetValidationVersion io = do
    db <- io

    worldVersion <- newVersion
    let taNames = map TaName ["ripe", "apnic", "afrinic"]
    seedActiveTaNames db taNames

    perTaResults <- QC.generate $ do
        taCount <- QC.choose (1, length taNames)
        let selectedTAs = take taCount taNames
        perTaMap <- QC.vectorOf taCount $ do
            payloads <- QC.arbitrary
            validationState <- QC.arbitrary
            pure (payloads, validationState)
        pure $ toPerTA $ zip selectedTAs perTaMap

    commonVS <- QC.generate QC.arbitrary

    rwTx db $ \tx ->
        DB.saveValidationVersion tx db worldVersion perTaResults commonVS

    storedValidations <- roTx db $ \tx -> DB.getValidationsPerTA tx db worldVersion
    storedMetrics <- roTx db $ \tx -> DB.getMetricsPerTA tx db worldVersion
    (commonValidations, commonMetrics, storedOutcomes) <-
        roTx db $ \tx -> DB.getValidationOutcomes tx db worldVersion

    let expectedValidations = fmap (\(_, vs) -> vs ^. typed) perTaResults
    let expectedMetrics = fmap (\(_, vs) -> vs ^. typed) perTaResults
    let expectedOutcomes = fmap (\(_, vs) -> (vs ^. typed, vs ^. typed)) perTaResults

    HU.assertEqual "Validations don't match" expectedValidations storedValidations
    HU.assertEqual "Metrics don't match" expectedMetrics storedMetrics
    HU.assertEqual "Common validations don't match"
        (commonVS ^. typed)
        commonValidations
    HU.assertEqual "Common metrics don't match"
        (commonVS ^. typed)
        commonMetrics
    HU.assertEqual "Validation outcomes don't match" expectedOutcomes storedOutcomes


shouldSaveAndGetValidationVersionFilledWithPastData :: IO DB -> HU.Assertion
shouldSaveAndGetValidationVersionFilledWithPastData io = do
    db <- io

    worldVersion1 <- newVersion
    threadDelay 10_000
    worldVersion2 <- newVersion
    threadDelay 10_000
    worldVersion3 <- newVersion

    let ripe = TaName "ripe"
    let apnic = TaName "apnic"
    let afrinic = TaName "afrinic"
    let taNames = [ripe, apnic, afrinic]
    seedActiveTaNames db taNames

    perTa1 <- QC.generate $ generatePerTa taNames
    perTa2 <- QC.generate $ generatePerTa [ripe]
    perTa3 <- QC.generate $ generatePerTa [afrinic]

    rwTx db $ \tx -> do
        commonVS <- QC.generate QC.arbitrary
        DB.saveValidationVersion tx db worldVersion1 perTa1 commonVS

    rwTx db $ \tx -> do
        commonVS <- QC.generate QC.arbitrary
        DB.saveValidationVersion tx db worldVersion2 perTa2 commonVS

    rwTx db $ \tx -> do
        commonVS <- QC.generate QC.arbitrary
        DB.saveValidationVersion tx db worldVersion3 perTa3 commonVS

    v1 <- roTx db $ \tx -> DB.getValidationsPerTA tx db worldVersion1
    v2 <- roTx db $ \tx -> DB.getValidationsPerTA tx db worldVersion2
    v3 <- roTx db $ \tx -> DB.getValidationsPerTA tx db worldVersion3

    let extract (_, vs) = vs ^. typed

    HU.assertEqual "1" (extract <$> perTa1 `getForTA` ripe) (v1 `getForTA` ripe)
    HU.assertEqual "2" (extract <$> perTa1 `getForTA` apnic) (v1 `getForTA` apnic)
    HU.assertEqual "3" (extract <$> perTa1 `getForTA` afrinic) (v1 `getForTA` afrinic)

    HU.assertEqual "1" (extract <$> perTa2 `getForTA` ripe) (v2 `getForTA` ripe)
    HU.assertEqual "2" (extract <$> perTa1 `getForTA` apnic) (v2 `getForTA` apnic)
    HU.assertEqual "3" (extract <$> perTa1 `getForTA` afrinic) (v2 `getForTA` afrinic)

    HU.assertEqual "1" (extract <$> perTa2 `getForTA` ripe) (v3 `getForTA` ripe)
    HU.assertEqual "2" (extract <$> perTa1 `getForTA` apnic) (v3 `getForTA` apnic)
    HU.assertEqual "3" (extract <$> perTa3 `getForTA` afrinic) (v3 `getForTA` afrinic)


shouldReadValidationOutcomePayloadQueries :: IO DB -> HU.Assertion
shouldReadValidationOutcomePayloadQueries io = do
    db <- io

    worldVersion1 <- newVersion
    threadDelay 10_000
    worldVersion2 <- newVersion
    threadDelay 10_000
    worldVersion3 <- newVersion

    let ripe = TaName "ripe"
    let apnic = TaName "apnic"
    let afrinic = TaName "afrinic"
    let taNames = [ripe, apnic, afrinic]
    seedActiveTaNames db taNames

    perTa1 <- QC.generate $ generatePerTa taNames
    perTa2 <- QC.generate $ generatePerTa [ripe]
    perTa3 <- QC.generate $ generatePerTa [afrinic]

    commonVS1 <- QC.generate QC.arbitrary
    commonVS2 <- QC.generate QC.arbitrary
    commonVS3 <- QC.generate QC.arbitrary

    rwTx db $ \tx ->
        DB.saveValidationVersion tx db worldVersion1 perTa1 commonVS1

    rwTx db $ \tx ->
        DB.saveValidationVersion tx db worldVersion2 perTa2 commonVS2

    rwTx db $ \tx ->
        DB.saveValidationVersion tx db worldVersion3 perTa3 commonVS3

    ripeV2 <- expectJust "Missing ripe data in version 2 fixture" (getForTA perTa2 ripe)
    apnicV1 <- expectJust "Missing apnic data in version 1 fixture" (getForTA perTa1 apnic)
    afrinicV3 <- expectJust "Missing afrinic data in version 3 fixture" (getForTA perTa3 afrinic)

    let expectedPerTaV3 = toPerTA
            [ (ripe, ripeV2)
            , (apnic, apnicV1)
            , (afrinic, afrinicV3)
            ]
        expectedRoasV3 = fmap (roas . fst) expectedPerTaV3
        expectedVrpsV3 = fmap toVrps expectedRoasV3
        expectedAspasV3 = Just $ Set.unions $ fmap (aspas . fst) expectedPerTaV3
        expectedGbrsV3 = Just $ Set.unions $ fmap (gbrs . fst) expectedPerTaV3
        expectedBgpsV3 = Just $ Set.unions $ fmap (bgpCerts . fst) expectedPerTaV3
        expectedSplsV3 = Just $ Set.unions $ fmap (spls . fst) expectedPerTaV3
        expectedValidationsV3 = fmap (\(_, vs) -> vs ^. typed) expectedPerTaV3
        expectedMetricsV3 = fmap (\(_, vs) -> vs ^. typed) expectedPerTaV3
        expectedOutcomesV3 = fmap (\(_, vs) -> (vs ^. typed, vs ^. typed)) expectedPerTaV3
        expectedLatestVersionsAll = toPerTA
            [ (ripe, worldVersion2)
            , (apnic, worldVersion1)
            , (afrinic, worldVersion3)
            ]

    commonMetricsV2 <- roTx db $ \tx -> DB.getCommonMetrics tx db worldVersion2
    HU.assertEqual "Common metrics should come from latest <= requested version"
        (commonVS2 ^. typed)
        commonMetricsV2

    storedRoasV3 <- roTx db $ \tx -> DB.getRoas tx db worldVersion3
    storedVrpsV3 <- roTx db $ \tx -> DB.getVrps tx db worldVersion3
    storedVrpsRipeV3 <- roTx db $ \tx -> DB.getVrpsForTA tx db worldVersion3 ripe
    storedVrpsApnicV3 <- roTx db $ \tx -> DB.getVrpsForTA tx db worldVersion3 apnic
    storedVrpsAfrinicV3 <- roTx db $ \tx -> DB.getVrpsForTA tx db worldVersion3 afrinic
    storedAspasV3 <- roTx db $ \tx -> DB.getAspas tx db worldVersion3
    storedGbrsV3 <- roTx db $ \tx -> DB.getGbrs tx db worldVersion3
    storedBgpsV3 <- roTx db $ \tx -> DB.getBgps tx db worldVersion3
    storedSplsV3 <- roTx db $ \tx -> DB.getSpls tx db worldVersion3
    storedValidationsV3 <- roTx db $ \tx -> DB.getValidationsPerTA tx db worldVersion3
    storedMetricsV3 <- roTx db $ \tx -> DB.getMetricsPerTA tx db worldVersion3
    (commonValidationsV3, commonMetricsV3, storedOutcomesV3) <-
        roTx db $ \tx -> DB.getValidationOutcomes tx db worldVersion3
    latestVersionsAll <- roTx db $ \tx -> DB.getLatestVersions tx db

    HU.assertEqual "ROAs should be selected from latest available rows per TA" expectedRoasV3 storedRoasV3
    HU.assertEqual "VRPs should be derived from latest ROAs per TA" expectedVrpsV3 storedVrpsV3
    HU.assertEqual "VRPs for ripe should come from version 2" (toVrps $ roas $ fst ripeV2) storedVrpsRipeV3
    HU.assertEqual "VRPs for apnic should fallback to version 1" (toVrps $ roas $ fst apnicV1) storedVrpsApnicV3
    HU.assertEqual "VRPs for afrinic should come from version 3" (toVrps $ roas $ fst afrinicV3) storedVrpsAfrinicV3
    HU.assertEqual "ASPAs should aggregate latest per-TA rows" expectedAspasV3 storedAspasV3
    HU.assertEqual "GBRs should aggregate latest per-TA rows" expectedGbrsV3 storedGbrsV3
    HU.assertEqual "BGP certs should aggregate latest per-TA rows" expectedBgpsV3 storedBgpsV3
    HU.assertEqual "SPLs should aggregate latest per-TA rows" expectedSplsV3 storedSplsV3
    HU.assertEqual "Per-TA validations should use latest available rows" expectedValidationsV3 storedValidationsV3
    HU.assertEqual "Per-TA metrics should use latest available rows" expectedMetricsV3 storedMetricsV3
    HU.assertEqual "Common validations should come from the latest common row"
        (commonVS3 ^. typed)
        commonValidationsV3
    HU.assertEqual "Common metrics should come from the latest common row"
        (commonVS3 ^. typed)
        commonMetricsV3
    HU.assertEqual "Validation outcomes should combine common and per-TA latest rows"
        expectedOutcomesV3
        storedOutcomesV3
    HU.assertEqual "Latest versions should be computed from validation_outcomes rows"
        expectedLatestVersionsAll
        latestVersionsAll

    rwTx db $ \tx -> DB.setActiveTAs tx db [ripe, afrinic]

    let expectedPerTaActive = toPerTA
            [ (ripe, ripeV2)
            , (afrinic, afrinicV3)
            ]
        expectedRoasActive = fmap (roas . fst) expectedPerTaActive
        expectedValidationsActive = fmap (\(_, vs) -> vs ^. typed) expectedPerTaActive
        expectedMetricsActive = fmap (\(_, vs) -> vs ^. typed) expectedPerTaActive
        expectedOutcomesActive = fmap (\(_, vs) -> (vs ^. typed, vs ^. typed)) expectedPerTaActive
        expectedLatestVersionsActive = toPerTA
            [ (ripe, worldVersion2)
            , (afrinic, worldVersion3)
            ]

    filteredRoas <- roTx db $ \tx -> DB.getRoas tx db worldVersion3
    filteredValidations <- roTx db $ \tx -> DB.getValidationsPerTA tx db worldVersion3
    filteredMetrics <- roTx db $ \tx -> DB.getMetricsPerTA tx db worldVersion3
    (_, _, filteredOutcomes) <- roTx db $ \tx -> DB.getValidationOutcomes tx db worldVersion3
    filteredLatest <- roTx db $ \tx -> DB.getLatestVersions tx db
    vrpsApnicInactive <- roTx db $ \tx -> DB.getVrpsForTA tx db worldVersion3 apnic

    HU.assertEqual "Inactive TA must be excluded from ROAs" expectedRoasActive filteredRoas
    HU.assertEqual "Inactive TA must be excluded from validations" expectedValidationsActive filteredValidations
    HU.assertEqual "Inactive TA must be excluded from metrics" expectedMetricsActive filteredMetrics
    HU.assertEqual "Inactive TA must be excluded from validation outcomes" expectedOutcomesActive filteredOutcomes
    HU.assertEqual "Inactive TA must be excluded from latest versions" expectedLatestVersionsActive filteredLatest
    HU.assertEqual "VRPs for inactive TA must be empty" mempty vrpsApnicInactive


shouldOrderAndLinkVersions :: IO DB -> HU.Assertion
shouldOrderAndLinkVersions io = do
    db <- io

    worldVersion1 <- newVersion
    threadDelay 10_000
    worldVersion2 <- newVersion
    threadDelay 10_000
    worldVersion3 <- newVersion

    let taNames = [TaName "ripe"]

    rwTx db $ \tx -> do
        common1 <- QC.generate QC.arbitrary
        perTa1  <- QC.generate $ generatePerTa taNames
        DB.saveValidationVersion tx db worldVersion1 perTa1 common1

        common2 <- QC.generate QC.arbitrary
        perTa2  <- QC.generate $ generatePerTa taNames
        DB.saveValidationVersion tx db worldVersion2 perTa2 common2

        common3 <- QC.generate QC.arbitrary
        perTa3  <- QC.generate $ generatePerTa taNames
        DB.saveValidationVersion tx db worldVersion3 perTa3 common3

    versions <- roTx db $ \tx -> DB.versionsBackwards tx db
    HU.assertEqual "Expected 3 stored versions" 3 (length versions)
    HU.assertEqual "Latest version should be first in descending list" worldVersion3 (head versions)

    latest <- roTx db $ \tx -> DB.getLatestVersion tx db
    HU.assertEqual "Latest version mismatch" (Just worldVersion3) latest

    prev3 <- roTx db $ \tx -> DB.previousVersion tx db worldVersion3
    prev2 <- roTx db $ \tx -> DB.previousVersion tx db worldVersion2
    prev1 <- roTx db $ \tx -> DB.previousVersion tx db worldVersion1

    HU.assertEqual "Previous of v3 should be v2" (Just worldVersion2) prev3
    HU.assertEqual "Previous of v2 should be v1" (Just worldVersion1) prev2
    HU.assertEqual "Previous of v1 should be empty" Nothing prev1


shouldDeleteValidationVersionData :: IO DB -> HU.Assertion
shouldDeleteValidationVersionData io = do
    db <- io

    worldVersion <- newVersion
    let taNames = [TaName "ripe", TaName "apnic"]

    perTa <- QC.generate $ generatePerTa taNames
    commonVS <- QC.generate QC.arbitrary
    let slurm = mempty

    rwTx db $ \tx -> do
        DB.saveValidationVersion tx db worldVersion perTa commonVS
        DB.saveSlurm tx db worldVersion slurm

    -- sanity check before delete
    versionsBefore <- roTx db $ \tx -> DB.versionsBackwards tx db
    slurmBefore <- roTx db $ \tx -> DB.getSlurm tx db worldVersion
    HU.assertBool "Version must exist before deletion" (worldVersion `elem` versionsBefore)
    HU.assertEqual "Slurm must exist before deletion" (Just slurm) slurmBefore

    rwTx db $ \tx -> DB.deleteValidationVersion tx db worldVersion

    versionsAfter <- roTx db $ \tx -> DB.versionsBackwards tx db
    slurmAfter <- roTx db $ \tx -> DB.getSlurm tx db worldVersion
    valsAfter <- roTx db $ \tx -> DB.getValidationsPerTA tx db worldVersion
    metricsAfter <- roTx db $ \tx -> DB.getMetricsPerTA tx db worldVersion

    HU.assertBool "Version must be gone after deletion" (worldVersion `notElem` versionsAfter)
    HU.assertEqual "Slurm must be gone" Nothing slurmAfter
    HU.assertEqual "Per-TA validations must be empty" mempty valsAfter
    HU.assertEqual "Per-TA metrics must be empty" mempty metricsAfter


shouldDeleteOldestVersionsOnceEveryTAHasEnoughRealData :: IO DB -> HU.Assertion
shouldDeleteOldestVersionsOnceEveryTAHasEnoughRealData io = do
    db <- io

    let ripe = TaName "ripe"
    let apnic = TaName "apnic"
    let taNames = [ripe, apnic]
    seedActiveTaNames db taNames

    -- Both TAs validate successfully every round.
    versions@[worldVersion1, worldVersion2, worldVersion3, worldVersion4, worldVersion5] <- newVersions 5
    forM_ versions $ \wv -> rwTx db $ \tx -> do
        perTa <- QC.generate $ generatePerTa taNames
        commonVS <- QC.generate QC.arbitrary
        DB.saveValidationVersion tx db wv perTa commonVS

    deleted <- rwTx db $ \tx -> DB.deleteOldestVersionsIfNeeded tx db 2

    HU.assertEqual "Should delete the three oldest rounds, keeping the newest 2 per TA"
        (List.sort [worldVersion1, worldVersion2, worldVersion3]) (List.sort deleted)

    remaining <- roTx db $ \tx -> DB.versionsBackwards tx db
    HU.assertEqual "Only the newest 2 versions should remain"
        (List.sort [worldVersion4, worldVersion5]) (List.sort remaining)


shouldNotDeleteVersionsBlockedByLaggingTA :: IO DB -> HU.Assertion
shouldNotDeleteVersionsBlockedByLaggingTA io = do
    db <- io

    let ripe = TaName "ripe"
    let apnic = TaName "apnic"
    let taNames = [ripe, apnic]
    seedActiveTaNames db taNames

    -- apnic only ever gets real data on the very first round; every later
    -- round only ripe validates. apnic can never accumulate 2 distinct real
    -- rounds, so nothing should ever be deleted, no matter how many rounds
    -- pass -- even though apnic gets no `validation_outcomes` row at all in
    -- any of the later rounds.
    versions@(worldVersion1 : _) <- newVersions 5

    rwTx db $ \tx -> do
        perTa1 <- QC.generate $ generatePerTa taNames
        commonVS1 <- QC.generate QC.arbitrary
        DB.saveValidationVersion tx db worldVersion1 perTa1 commonVS1

    forM_ (drop 1 versions) $ \wv -> rwTx db $ \tx -> do
        perTa <- QC.generate $ generatePerTa [ripe]
        commonVS <- QC.generate QC.arbitrary
        DB.saveValidationVersion tx db wv perTa commonVS

    deleted <- rwTx db $ \tx -> DB.deleteOldestVersionsIfNeeded tx db 2

    HU.assertEqual "Nothing should be deleted while apnic never reaches 2 real rounds" [] deleted

    remaining <- roTx db $ \tx -> DB.versionsBackwards tx db
    HU.assertBool "The round with apnic's only real data must still be present"
        (worldVersion1 `elem` remaining)


newVersions :: Int -> IO [WorldVersion]
newVersions n = forM [1 .. n] $ \i -> do
    when (i > 1) $ threadDelay 10_000
    newVersion


generatePerTa :: (QC.Arbitrary a, QC.Arbitrary b) => [TaName] -> QC.Gen (PerTA (a, b))
generatePerTa taNames = do
    perTaMap <- QC.vectorOf (length taNames) $ do
        payloads <- QC.arbitrary
        validationState <- QC.arbitrary
        pure (payloads, validationState)
    pure $ toPerTA $ zip taNames perTaMap


seedActiveTaNames :: DB -> [TaName] -> IO ()
seedActiveTaNames db taNames =
    rwTx db $ \(Tx conn) ->
        forM_ taNames $ \(TaName taName) ->
            execute conn
                "INSERT OR REPLACE INTO trust_anchors(ta_name, data, active) VALUES (?, ?, 1)"
                (taName, BS.empty)


shouldRollbackAppTx :: IO DB -> HU.Assertion
shouldRollbackAppTx io = do
    db <- io
    Now i1 <- thisInstant
    Now i2 <- thisInstant
    Now i3 <- thisInstant

    void $ runValidatorT (newScopes "tx-rollback") $ DB.rwAppTx db $ \tx -> do
        liftIO $ DB.setJobCompletionTime tx db "job-rollback" i1
        appError $ UnspecifiedE "Test" "Rollback requested"

    void $ runValidatorT (newScopes "tx-commit") $ DB.rwAppTx db $ \tx ->
        liftIO $ DB.setJobCompletionTime tx db "job-commit" i2

    let throwFromTx =
            void $ runValidatorT (newScopes "tx-throw") $ DB.rwAppTx db $ \tx -> do
                liftIO $ DB.setJobCompletionTime tx db "job-ex" i3
                liftIO $ throwIO DivideByZero

    Left (SomeException e) <- try throwFromTx
    HU.assertEqual "Must be the right type of exception"
        (fromException (toException e))
        (Just DivideByZero)

    jobs <- roTx db $ \tx -> DB.allJobs tx db
    HU.assertEqual "Rolled-back job must not be persisted" Nothing (lookup "job-rollback" jobs)
    HU.assertEqual "Committed job must be persisted" (Just i2) (lookup "job-commit" jobs)
    HU.assertEqual "Exception-rolled job must not be persisted" Nothing (lookup "job-ex" jobs)


shouldPreserveStateInAppTx :: IO DB -> HU.Assertion
shouldPreserveStateInAppTx io = do
    db <- io

    let addedObject = updateMetric @RrdpMetric @_ (#added %~ Map.unionWith (+) (Map.singleton (Just CER) 1))

    (_, ValidationState { validations = Validations validationMap, .. })
        <- runValidatorT (newScopes "root") $
            timedMetric (Proxy :: Proxy RrdpMetric) $ do
                appWarn $ UnspecifiedE "Error0" "text 0"
                void $ DB.rwAppTx db $ \tx -> do
                    addedObject
                    appWarn $ UnspecifiedE "Error1" "text 1"
                    inSubVScope "nested-1" $
                        appWarn $ UnspecifiedE "Error2" "text 2"
                    -- touch DB inside tx, but assertions are about state preservation
                    liftIO $ DB.getDatabaseVersion tx db
                appWarn $ UnspecifiedE "Error4" "text 4"
                addedObject

    HU.assertEqual "Root metric should count 2 objects"
        (Just $ mempty { added = Map.fromList [(Just CER, Count 2)], deleted = Map.empty })
        (stripTime <$> lookupMetric (newScope "root") (rrdpMetrics topDownMetric))

    HU.assertEqual "Nested metric should not be emitted"
        Nothing
        (stripTime <$> lookupMetric (subScope TextFocus "metric-nested-1" (newScope "root"))
                            (rrdpMetrics topDownMetric))

    HU.assertEqual "Root validations should have 3 warnings"
        (Map.lookup (newScope "root") validationMap)
        (Just $ Set.fromList
            [ VWarn (VWarning (UnspecifiedE "Error0" "text 0"))
            , VWarn (VWarning (UnspecifiedE "Error1" "text 1"))
            , VWarn (VWarning (UnspecifiedE "Error4" "text 4"))
            ])

    HU.assertEqual "Nested validations should have 1 warning"
        (Map.lookup (subScope TextFocus "nested-1" (newScope "root")) validationMap)
        (Just $ Set.fromList [VWarn (VWarning (UnspecifiedE "Error2" "text 2"))])


shouldReopenDatabase :: HU.Assertion
shouldReopenDatabase =
    withTestContext $ \appContext -> do
        db <- readTVarIO $ appContext ^. #database

        Now now <- thisInstant
        rwTx db $ \tx -> DB.setJobCompletionTime tx db "reopen-job" now

        reopenStorage appContext

        db' <- readTVarIO $ appContext ^. #database
        jobs <- roTx db' $ \tx -> DB.allJobs tx db'

        HU.assertEqual "Persisted data must remain available after reopen"
            (Just now)
            (lookup "reopen-job" jobs)


stripTime :: metric -> metric
stripTime = id

withDB :: (IO DB -> TestTree) -> TestTree
withDB mk = mk $
    withTestContext $ \appContext ->
        readTVarIO $ appContext ^. #database


ioTestCase :: TestName -> (IO DB -> HU.Assertion) -> TestTree
ioTestCase s f = withDB $ \io -> HU.testCase s (f io)

dbTestCase :: TestName -> (IO DB -> HU.Assertion) -> TestTree
dbTestCase = ioTestCase


readObjectFromFile :: FilePath -> ValidatorT IO (RpkiURL, ParsedRpkiObject)
readObjectFromFile path = do 
    bs <- liftIO $ BS.readFile path
    let Right url = parseRpkiURL $ "rsync://host/" <> Text.pack path
    o <- vHoist $ readObject url bs
    pure (url, o)

replaceAKI :: AKI -> ParsedRpkiObject -> ParsedRpkiObject
replaceAKI a = \case 
    CerRO c  -> CerRO $ c & #aki ?~ a
    BgpRO c  -> BgpRO $ c & #aki ?~ a
    CrlRO c  -> CrlRO $ c & #aki .~ a
    MftRO c  -> MftRO $ c & #cmsPayload %~ mapCms
    RoaRO c  -> RoaRO $ c & #cmsPayload %~ mapCms
    SplRO c  -> SplRO $ c & #cmsPayload %~ mapCms
    GbrRO c  -> GbrRO $ c & #cmsPayload %~ mapCms
    RscRO c  -> RscRO $ c & #cmsPayload %~ mapCms
    AspaRO c -> AspaRO $ c & #cmsPayload %~ mapCms
  where
    mapCms :: CMS a1 -> CMS a1
    mapCms (CMS so) = CMS $ so & #soContent . #scCertificate . #aki .~ a

-- Convert without validating, 
toValidatedRpkiObject :: ParsedRpkiObject -> WellStructuredRpkiObject
toValidatedRpkiObject = \case
    CerRO ca    -> CerRO  $ extractCert ca
    CrlRO crl   -> CrlRO  crl
    MftRO mft   -> MftRO  $ extractCMSObject mft
    RoaRO roa   -> RoaRO  $ extractCMSObject roa
    GbrRO gbr   -> GbrRO  $ extractCMSObject gbr
    AspaRO aspa -> AspaRO $ extractCMSObject aspa
    SplRO spl   -> SplRO  $ extractCMSObject spl
    BgpRO bgp   -> BgpRO  $ extractCert bgp
    RscRO rsc   -> RscRO  $ extractCMSObject rsc

newVersion :: MonadIO m => m WorldVersion
newVersion = instantToVersion . unNow <$> thisInstant

expectJust :: String -> Maybe a -> IO a
expectJust message = \case
    Just a  -> pure a
    Nothing -> HU.assertFailure message >> fail message
