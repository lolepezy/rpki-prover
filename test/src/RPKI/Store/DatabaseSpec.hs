{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}


module RPKI.Store.DatabaseSpec where

import           Control.Exception.Lifted

import           Control.Lens                     
import           Control.Monad
import           Control.Monad.Reader
import           Data.Generics.Product.Typed

import qualified Data.ByteString                   as BS
import qualified Data.List                         as List
import qualified Data.Map.Strict                   as Map
import           Data.Proxy
import qualified Data.Set as Set
import qualified Data.Text                         as Text

import           System.Directory
import           System.IO.Temp

import           Test.QuickCheck.Arbitrary.Generic
import           Test.Tasty
import qualified Test.Tasty.HUnit                  as HU
import qualified Test.Tasty.QuickCheck             as QC

import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.AppState
import           RPKI.Domain
import           RPKI.Config
import           RPKI.Logging
import           RPKI.Reporting
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.Store.Base.LMDB
import           RPKI.Store.Base.Map               as M
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database    (DB(..))
import qualified RPKI.Store.Database    as DB
import           RPKI.Store.Types

import qualified RPKI.Store.MakeLmdb as Lmdb

import           RPKI.Time
import           RPKI.Util

import           RPKI.RepositorySpec

import Data.Generics.Product (HasField)
import Control.Concurrent (threadDelay)
import RPKI.Store.Base.Serialisation (serialise_)


databaseGroup :: TestTree
databaseGroup = testGroup "LMDB storage tests"
    [
        objectStoreGroup,
        repositoryStoreGroup,
        versionStoreGroup,
        txGroup,
        mapGroup
    ]

objectStoreGroup :: TestTree
objectStoreGroup = testGroup "Object storage test"
    [                    
        -- dbTestCase "Should insert and get back" shouldInsertAndGetAllBackFromObjectStore,        
        dbTestCase "Should order manifests according to their dates" shouldOrderManifests,
        dbTestCase "Should merge locations" shouldMergeObjectLocations
    ]        

repositoryStoreGroup :: TestTree
repositoryStoreGroup = testGroup "Repository LMDB storage test"
    [
        dbTestCase "Should insert and get an rsync repository" shouldSaveAndGetRsyncRepositories,
        dbTestCase "Should overwrite metadata and validations" shouldSaveMetaAndValidationAsCorrectSemigroup
    ]        

versionStoreGroup :: TestTree
versionStoreGroup = testGroup "Version LMDB storage test"
    [
        dbTestCase "Should insert and get a version" shouldSaveAndGetValidationVersion,        
        dbTestCase "Should insert and get a version with data from previous versions" 
            shouldSaveAndGetValidationVersionFilledWithPastData
    ]        

txGroup :: TestTree
txGroup = testGroup "App transaction test"
    [
        ioTestCase "Should rollback App transactions properly" shouldRollbackAppTx,        
        ioTestCase "Should preserve state from StateT in transactions" shouldPreserveStateInAppTx
    ]

mapGroup :: TestTree
mapGroup = testGroup "SafeMap tests"
    [
        ioTestCase "Should put and get from SafeMap" shouldSafePutAndGet        
    ]


shouldMergeObjectLocations :: Storage s => IO (DB s) -> HU.Assertion
shouldMergeObjectLocations io = do 
    
    db <- io
    Now now <- thisInstant 

    [url1, url2, url3] :: [RpkiURL] <- take 3 . List.nub <$> replicateM 10 (QC.generate arbitrary)

    ro1 :: RpkiObject <- QC.generate arbitrary    
    ro2 :: RpkiObject <- QC.generate arbitrary        
    
    let storeIt obj url = rwTx db $ \tx -> do        
            DB.saveObject tx db (toStorableObject obj) (instantToVersion now)
            DB.linkObjectToUrl tx db url (getHash obj)

    let getIt hash = roTx db $ \tx -> DB.getByHash tx db hash    

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

    -- should only clean up URLs
    deletedUrls <- rwTx db $ DB.deleteDanglingUrls db
    HU.assertEqual "Should have deleted 2 URLs" 2 deletedUrls

    verifyUrlCount db "case 3" 1

    Just (Located loc2 _) <- getIt (getHash ro2)
    HU.assertEqual "Wrong locations 3" loc2 (toLocations url3)
    where 
        verifyUrlCount db@DB {..} s count = do 
            uriToUriKey <- roTx db $ \tx -> M.all tx (DB.uriToUriKey objectStore)
            uriKeyToUri <- roTx db $ \tx -> M.all tx (DB.uriKeyToUri objectStore)
            HU.assertEqual ("Not all URLs one way " <> s) count (length uriToUriKey)
            HU.assertEqual ("Not all URLs backwards " <> s) count (length uriKeyToUri)        


-- shouldInsertAndGetAllBackFromObjectStore :: Storage s => IO (DB s) -> HU.Assertion
-- shouldInsertAndGetAllBackFromObjectStore io = do  
--     db <- io
--     aki1 :: AKI <- QC.generate arbitrary
--     aki2 :: AKI <- QC.generate arbitrary
--     ros :: [Located RpkiObject] <- removeMftNumberDuplicates <$> generateSome    

--     let (firstHalf, secondHalf) = List.splitAt (List.length ros `div` 2) ros

--     let ros1 = List.map (typed @RpkiObject %~ replaceAKI aki1) firstHalf
--     let ros2 = List.map (typed @RpkiObject %~ replaceAKI aki2) secondHalf
--     let ros' = ros1 <> ros2 

--     Now now <- thisInstant     

--     rwTx db $ \tx -> 
--         for_ ros' $ \(Located (Locations locations) ro) -> do             
--             void $ DB.saveObject tx db (toStorableObject ro) (instantToVersion now)
--             forM_ locations $ \url -> 
--                 DB.linkObjectToUrl tx db url (getHash ro)

--     allObjects <- roTx db $ \tx -> DB.getAll tx db
--     HU.assertEqual 
--         "Not the same objects" 
--         (List.sortOn (getHash . (^. #payload)) allObjects) 
--         (List.sortOn (getHash . (^. #payload)) ros')
        
--     compareLatestMfts db ros1 aki1    
--     compareLatestMfts db ros2 aki2  
    
--     let (toDelete, toKeep) = List.splitAt (List.length ros1 `div` 2) ros1

--     rwTx db $ \tx -> 
--         forM_ toDelete $ \(Located _ ro) -> 
--             DB.deleteObjectByHash tx db (getHash ro)
    
--     compareLatestMfts db ros2 aki2      
--     compareLatestMfts db toKeep aki1
    
--   where
--     removeMftNumberDuplicates = List.nubBy $ \ro1 ro2 ->
--             case (ro1, ro2) of
--                 (Located _ (MftRO mft1), Located _ (MftRO mft2)) -> 
--                     DB.getMftTimingMark mft1 == DB.getMftTimingMark mft2
--                 _ -> False

--     compareLatestMfts db ros a = do
--         -- mftLatest <- roTx db $ \tx -> DB.findLatestMftByAKI tx db a         
--         mftLatest <- roTx db $ \tx -> do 
--             MftMeta {..} : _ <- DB.getMftsForAKI tx db a
--             DB.getMftByKey tx db key
        
--         let mftLatest' = listToMaybe $ List.sortOn (Down . DB.getMftMeta)
--                 [ mft | Located _ (MftRO mft) <- ros, getAKI mft == Just a ]                                    
        
--         HU.assertEqual "Not the same manifests" ((^. #object . #payload) <$> mftLatest) mftLatest'                    


shouldOrderManifests :: Storage s => IO (DB s) -> HU.Assertion
shouldOrderManifests io = do  
    db@DB {..} <- io
    (Right (url1, mft1), _) <- runValidatorT (newScopes "read1") $ readObjectFromFile "./test/data/afrinic_mft1.mft"
    (Right (url2, mft2), _) <- runValidatorT (newScopes "read2") $ readObjectFromFile "./test/data/afrinic_mft2.mft"
    
    worldVersion <- newVersion

    rwTx objectStore $ \tx -> do        
            void $ DB.saveObject tx db (toStorableObject mft1) worldVersion
            void $ DB.saveObject tx db (toStorableObject mft2) worldVersion
            DB.linkObjectToUrl tx db url1 (getHash mft1)
            DB.linkObjectToUrl tx db url2 (getHash mft2)


    -- they have the same AKIs
    let Just aki1 = getAKI mft1
    [m1, m2] <- roTx objectStore $ \tx -> DB.getMftsForAKI tx db aki1

    HU.assertBool "Bla" (m1 ^. #nextTime >= m2 ^. #nextTime)

    Just (Keyed (Located _ mftLatest) _) <- roTx objectStore $ \tx -> do 
            MftMeta {..} : _ <- DB.getMftsForAKI tx db aki1
            DB.getMftByKey tx db key

    HU.assertEqual "Not the same manifests" (MftRO mftLatest) mft2


shouldSaveAndGetRsyncRepositories :: Storage s => IO (DB s) -> HU.Assertion
shouldSaveAndGetRsyncRepositories io = do  
    db <- io

    repositories <- (<>) <$> 
            replicateM 100 (QC.generate arbitrary) <*>
            rsyncReposWithCommonHosts 100
    
    rwTx db $ \tx -> DB.saveRsyncRepositories tx db repositories

    let urls = [ u | r <- repositories, RsyncU u <- [ getRpkiURL $ RsyncR r]]
    repositories' <- roTx db $ \tx -> DB.getRsyncRepositories tx db urls

    HU.assertEqual "Not the same set of rsync repositories" 
        (Set.fromList repositories) 
        (Set.fromList $ Map.elems repositories')
    


shouldSaveMetaAndValidationAsCorrectSemigroup :: Storage s => IO (DB s) -> HU.Assertion
shouldSaveMetaAndValidationAsCorrectSemigroup io = do
    db <- io
    [rsync1] <- rsyncReposWithCommonHosts 1

    testOneRepository db rsync1

    -- update meta
    Now now <- thisInstant    
    testOneRepository db $ rsync1 & #meta . #status .~ FetchedAt now
    testOneRepository db $ rsync1 & #meta . #status .~ FailedAt now
    testOneRepository db $ rsync1 & #meta . #status .~ Pending

    pure ()
  where
    testOneRepository db rsync = do
        rwTx db $ \tx -> DB.saveRsyncRepositories tx db [rsync]
        let RsyncU url = getRpkiURL $ RsyncR rsync
        rs <- roTx db $ \tx -> DB.getRsyncRepositories tx db [url]
        let Just r = Map.lookup url rs
        HU.assertEqual "Same repository" r rsync



rsyncReposWithCommonHosts :: Int -> IO [RsyncRepository]
rsyncReposWithCommonHosts n = do
    replicateM n $ QC.generate $ do 
        hostName  <- RsyncHostName <$> QC.elements [ "host1", "host2", "host3" ]        
        let host = RsyncHost hostName Nothing
        pathChunks <- do 
            n <- QC.choose (1, 3)
            replicateM n arbitrary                    
        RsyncRepository (RsyncPublicationPoint $ RsyncURL host pathChunks) <$> arbitrary      


generateRepositories :: IO PublicationPoints
generateRepositories = do     
    rrdpMap :: RrdpMap <- QC.generate arbitrary        
    let pps = PublicationPoints rrdpMap newRsyncForest
    pure $ List.foldr mergeRsyncPP pps repositoriesURIs    
    

rrdpSubMap :: PublicationPoints -> IO RrdpMap
rrdpSubMap pps = do 
    let RrdpMap rrdpsM = rrdps pps
    keys_ <- QC.generate (QC.sublistOf $ Map.keys rrdpsM)
    pure $ RrdpMap $ Map.filterWithKey (\u _ -> u `elem` keys_) rrdpsM


shouldSaveAndGetValidationVersion :: Storage s => IO (DB s) -> HU.Assertion
shouldSaveAndGetValidationVersion io = do
    db <- io

    worldVersion <- newVersion 
    let taNames = map TaName [ "ripe", "apnic", "afrinic" ]

    perTaResults <- QC.generate $ do
        taCount <- QC.choose (1, length taNames)
        let selectedTAs = take taCount taNames
        perTaMap <- QC.vectorOf taCount $ do
            payloads <- arbitrary
            validationState <- arbitrary
            pure (payloads, validationState)
        pure $ toPerTA $ zip selectedTAs perTaMap
    
    commonVS <- QC.generate arbitrary

    rwTx db $ \tx -> 
        DB.saveValidationVersion tx db worldVersion taNames perTaResults commonVS

    storedValidations <- roTx db $ \tx -> DB.getValidationsPerTA tx db worldVersion
    storedMetrics <- roTx db $ \tx -> DB.getMetricsPerTA tx db worldVersion

    HU.assertEqual "Validations don't match" (fmap (\(_, vs) -> vs ^. typed) perTaResults) storedValidations
    HU.assertEqual "Metrics don't match" (fmap (\(_, vs) -> vs ^. typed) perTaResults) storedMetrics


shouldSaveAndGetValidationVersionFilledWithPastData :: Storage s => IO (DB s) -> HU.Assertion
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
    let taNames = [ ripe, apnic, afrinic ]

    perTa1 <- QC.generate $ generatePerTa taNames
    perTa2 <- QC.generate $ generatePerTa [ ripe ]
    perTa3 <- QC.generate $ generatePerTa [ afrinic ]
    
    rwTx db $ \tx -> do 
        commonVS <- QC.generate arbitrary
        DB.saveValidationVersion tx db worldVersion1 taNames perTa1 commonVS

    rwTx db $ \tx -> do 
        commonVS <- QC.generate arbitrary
        DB.saveValidationVersion tx db worldVersion2 taNames perTa2 commonVS        

    rwTx db $ \tx -> do 
        commonVS <- QC.generate arbitrary
        DB.saveValidationVersion tx db worldVersion3 taNames perTa3 commonVS

    v1 <- roTx db $ \tx -> DB.getValidationsPerTA tx db worldVersion1
    m1 <- roTx db $ \tx -> DB.getMetricsPerTA tx db worldVersion1    

    v2 <- roTx db $ \tx -> DB.getValidationsPerTA tx db worldVersion2
    m2 <- roTx db $ \tx -> DB.getMetricsPerTA tx db worldVersion2

    v3 <- roTx db $ \tx -> DB.getValidationsPerTA tx db worldVersion3
    m3 <- roTx db $ \tx -> DB.getMetricsPerTA tx db worldVersion3

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

    HU.assertEqual "RIPE data in version 1 doesn't match original input" 
        (extract <$> perTa1 `getForTA` ripe) (v1 `getForTA` ripe)
    HU.assertEqual "APNIC data in version 1 doesn't match original input" 
        (extract <$> perTa1 `getForTA` apnic) (v1 `getForTA` apnic)
    HU.assertEqual "AFRINIC data in version 1 doesn't match original input" 
        (extract <$> perTa1 `getForTA` afrinic) (v1 `getForTA` afrinic)

    HU.assertEqual "RIPE data in version 2 doesn't match updated input" 
        (extract <$> perTa2 `getForTA` ripe) (v2 `getForTA` ripe)
    HU.assertEqual "APNIC data in version 2 doesn't match inherited data from version 1" 
        (extract <$> perTa1 `getForTA` apnic) (v2 `getForTA` apnic)
    HU.assertEqual "AFRINIC data in version 2 doesn't match inherited data from version 1" 
        (extract <$> perTa1 `getForTA` afrinic) (v2 `getForTA` afrinic)

    HU.assertEqual "RIPE data in version 3 doesn't match inherited data from version 2" 
        (extract <$> perTa2 `getForTA` ripe) (v3 `getForTA` ripe)
    HU.assertEqual "APNIC data in version 3 doesn't match inherited data from version 1" 
        (extract <$> perTa1 `getForTA` apnic) (v3 `getForTA` apnic)
    HU.assertEqual "AFRINIC data in version 3 doesn't match updated input" 
        (extract <$> perTa3 `getForTA` afrinic) (v3 `getForTA` afrinic)


generatePerTa :: (Arbitrary a, Arbitrary b) => [TaName] -> QC.Gen (PerTA (a, b))
generatePerTa taNames = do        
    perTaMap <- QC.vectorOf (length taNames) $ do
        payloads <- arbitrary
        validationState <- arbitrary
        pure (payloads, validationState)
    pure $ toPerTA $ zip taNames perTaMap


shouldRollbackAppTx :: IO ((FilePath, LmdbEnv), DB LmdbStorage) -> HU.Assertion
shouldRollbackAppTx io = do  
    ((_, env), _) <- io

    let storage' = LmdbStorage env
    z :: SMap "test" LmdbStorage Int String <- SMap storage' <$> createLmdbStore env

    void $ runValidatorT (newScopes "bla") $ DB.rwAppTx storage' $ \tx -> do
        liftIO $ M.put tx z 1 "aa"
        appError $ UnspecifiedE "Test" "Test problem"

    void $ runValidatorT (newScopes "bla") $ DB.rwAppTx storage' $ \tx ->
        liftIO $ M.put tx z 2 "bb"        

    let throwFromTx =
            void $ runValidatorT (newScopes "bla") $ DB.rwAppTx storage' $ \tx -> liftIO $ do
                    M.put tx z 3 "cc"        
                    throwIO RatioZeroDenominator

    Left (SomeException e) <- try throwFromTx    
    HU.assertEqual "Must be the right type of exception" 
            (fromException (toException e)) 
            (Just RatioZeroDenominator)

    void $ runValidatorT (newScopes "bla") $ DB.roAppTx storage' $ \tx -> liftIO $ do         
         v1 <- M.get tx z 1  
         HU.assertEqual "Must not be there" v1 Nothing
         v2 <- M.get tx z 2  
         HU.assertEqual "Must be rolled back by appError" v2 (Just "bb")
         v3 <- M.get tx z 3  
         HU.assertEqual "Must be rolled back by exception" v3 Nothing
    

shouldPreserveStateInAppTx :: IO ((FilePath, LmdbEnv), DB LmdbStorage) -> HU.Assertion
shouldPreserveStateInAppTx io = do  
    ((_, env), DB {..}) <- io

    let storage' = LmdbStorage env
    z :: SMap "test-state" LmdbStorage Int String <- SMap storage' <$> createLmdbStore env

    let addedObject = updateMetric @RrdpMetric @_ (#added %~ Map.unionWith (+) (Map.singleton (Just CER) 1))

    (_, ValidationState { validations = Validations validationMap, .. }) 
        <- runValidatorT (newScopes "root") $ 
            timedMetric (Proxy :: Proxy RrdpMetric) $ do                 
                appWarn $ UnspecifiedE "Error0" "text 0"
                void $ DB.rwAppTx storage' $ \tx -> do                             
                    addedObject        
                    appWarn $ UnspecifiedE "Error1" "text 1"
                    inSubVScope "nested-1" $ 
                        appWarn $ UnspecifiedE "Error2" "text 2"
                    -- just to have a transaction
                    liftIO $ M.get tx z 0

                appWarn $ UnspecifiedE "Error4" "text 4"
                addedObject

    HU.assertEqual "Root metric should count 2 objects" 
        (Just $ mempty { added = Map.fromList [(Just CER, Count 2)], deleted = Map.empty })
        (stripTime <$> lookupMetric (newScope "root") (rrdpMetrics topDownMetric))        

    HU.assertEqual "Nested metric should count 1 object" 
        Nothing
        (stripTime <$> lookupMetric (subScope TextFocus "metric-nested-1" (newScope "root"))
                            (rrdpMetrics topDownMetric))        

    HU.assertEqual "Root validations should have 1 warning"     
        (Map.lookup (newScope "root") validationMap)
        (Just $ Set.fromList [
            VWarn (VWarning (UnspecifiedE "Error0" "text 0")),
            VWarn (VWarning (UnspecifiedE "Error1" "text 1")),            
            VWarn (VWarning (UnspecifiedE "Error4" "text 4"))])

    HU.assertEqual "Nested validations should have 1 warning" 
        (Map.lookup (subScope TextFocus "nested-1" (newScope "root")) validationMap)
        (Just $ Set.fromList [VWarn (VWarning (UnspecifiedE "Error2" "text 2"))])



shouldSafePutAndGet :: IO ((FilePath, LmdbEnv), DB LmdbStorage) -> HU.Assertion
shouldSafePutAndGet io = do    
    ((_, env), _) <- io

    let storage' = LmdbStorage env
    z :: DB.SafeMap "test-state" LmdbStorage Text.Text String <- SMap storage' <$> createLmdbStore env

    let short = "short-key" :: Text.Text
    let long = Text.replicate 600 "long-key-part-" :: Text.Text   

    HU.assertBool "The long key is very too long" (BS.length (serialise_ long) > 512)

    rwTx z $ \tx -> do              
        DB.safePut tx z short "one"
        DB.safePut tx z long "two"

    s <- roTx z $ \tx -> DB.safeGet tx z short
    l <- roTx z $ \tx -> DB.safeGet tx z long

    HU.assertEqual "Validations don't match" s  (Just "one")
    HU.assertEqual "Validations don't match" l  (Just "two")

    rwTx z $ \tx -> do              
        DB.safePut tx z short "one-updated"
        DB.safePut tx z long "two-updated"    

    ss <- roTx z $ \tx -> DB.safeGet tx z short
    ll <- roTx z $ \tx -> DB.safeGet tx z long

    HU.assertEqual "Validations don't match" ss  (Just "one-updated")
    HU.assertEqual "Validations don't match" ll  (Just "two-updated")        


stripTime :: HasField "totalTimeMs" metric metric TimeMs TimeMs => metric -> metric
stripTime = #totalTimeMs .~ TimeMs 0

generateSome :: Arbitrary a => IO [a]
generateSome = replicateM 1000 $ QC.generate arbitrary      

withDB :: (IO ((FilePath, LmdbEnv), DB LmdbStorage) -> TestTree) -> TestTree
withDB = withResource (makeLmdbStuff createLmdb) releaseLmdb
  where
    createLmdb lmdbEnv = 
        withLogger (makeLogConfig defaultsLogLevel MainLog) $ \logger -> 
            fst <$> Lmdb.createDatabase lmdbEnv logger Lmdb.DontCheckVersion


ioTestCase :: TestName -> (IO ((FilePath, LmdbEnv), DB LmdbStorage) -> HU.Assertion) -> TestTree
ioTestCase s f = withDB $ \io -> HU.testCase s (f io)

dbTestCase :: TestName -> (IO (DB LmdbStorage) -> HU.Assertion) -> TestTree
dbTestCase s f = ioTestCase s $ f . (snd <$>)

makeLmdbStuff :: (LmdbEnv -> IO b) -> IO ((FilePath, LmdbEnv), b)
makeLmdbStuff mkStore = do 
    dir <- createTempDirectory "/tmp" "lmdb-test"
    e <- Lmdb.mkLmdb dir defaultConfig
    store <- mkStore e
    pure ((dir, e), store)

releaseLmdb :: ((FilePath, LmdbEnv), b) -> IO ()
releaseLmdb ((dir, e), _) = do    
    Lmdb.closeLmdb e
    removeDirectoryRecursive dir

readObjectFromFile :: FilePath -> ValidatorT IO (RpkiURL, RpkiObject)
readObjectFromFile path = do 
    bs <- liftIO $ BS.readFile path
    let Right url = parseRpkiURL $ "rsync://host/" <> Text.pack path
    o <- vHoist $ readObject url bs
    pure (url, o)

replaceAKI :: AKI -> RpkiObject -> RpkiObject
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
    mapCms :: CMS a -> CMS a
    mapCms (CMS so) = CMS $ so & #soContent . #scCertificate . #aki .~ a

newVersion :: MonadIO m => m WorldVersion
newVersion = instantToVersion . unNow <$> thisInstant     