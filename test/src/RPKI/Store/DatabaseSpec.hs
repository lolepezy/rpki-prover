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

import           Control.Lens                     ((.~), (%~), (&), (^.))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Generics.Product.Typed

import           Control.Monad.IO.Class
import qualified Data.ByteString                   as BS
import           Data.Foldable
import qualified Data.List                         as List
import qualified Data.Map.Strict                   as Map
import qualified Data.Set.NonEmpty                 as NESet
import           Data.Maybe
import           Data.Ord
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
import           RPKI.AppState
import           RPKI.Domain
import           RPKI.Logging
import           RPKI.Reporting
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.Store.Base.LMDB
import           RPKI.Store.Base.Map               as M
import qualified RPKI.Store.Base.MultiMap          as MM
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database

import qualified RPKI.Store.MakeLmdb as Lmdb

import           RPKI.Time
import           RPKI.Util

import           RPKI.RepositorySpec

import Data.Generics.Product (HasField)


storeGroup :: TestTree
storeGroup = testGroup "LMDB storage tests"
    [
        objectStoreGroup,
        validationResultStoreGroup,
        repositoryStoreGroup,
        txGroup
    ]

objectStoreGroup :: TestTree
objectStoreGroup = testGroup "Object storage test"
    [                    
        dbTestCase "Should insert and get back" shouldInsertAndGetAllBackFromObjectStore,        
        dbTestCase "Should order manifests accoring to their dates" shouldOrderManifests,
        dbTestCase "Should merge locations" shouldMergeObjectLocations
        -- dbTestCase "Should save, delete and have no garbage left" shouldCreateAndDeleteAllTheMaps
    ]        

validationResultStoreGroup :: TestTree
validationResultStoreGroup = testGroup "Validation result storage test"
    [
        dbTestCase "Should insert and get back" shouldInsertAndGetAllBackFromValidationResultStore,
        dbTestCase "Should insert and get back" shouldGetAndSaveRepositories
    ]

repositoryStoreGroup :: TestTree
repositoryStoreGroup = testGroup "Repository LMDB storage test"
    [
        dbTestCase "Should insert and get a repository" shouldInsertAndGetAllBackFromRepositoryStore
    ]        

txGroup :: TestTree
txGroup = testGroup "App transaction test"
    [
        ioTestCase "Should rollback App transactions properly" shouldRollbackAppTx,        
        ioTestCase "Should preserve state from StateT in transactions" shouldPreserveStateInAppTx
    ]


shouldMergeObjectLocations :: Storage s => IO (DB s) -> HU.Assertion
shouldMergeObjectLocations io = do 
    
    db@DB {..} <- io
    Now now <- thisInstant 

    [url1, url2, url3] :: [RpkiURL] <- take 3 . List.nub <$> replicateM 10 (QC.generate arbitrary)

    ro1 :: RpkiObject <- QC.generate arbitrary    
    ro2 :: RpkiObject <- QC.generate arbitrary        
    extraLocations :: Locations <- QC.generate arbitrary    

    let storeIt obj url = rwTx objectStore $ \tx -> do        
            putObject tx objectStore (toStorableObject obj) (instantToVersion now)
            linkObjectToUrl tx objectStore url (getHash obj)

    let getIt hash = roTx objectStore $ \tx -> getByHash tx objectStore hash    


    storeIt ro1 url1
    storeIt ro1 url2
    storeIt ro1 url3

    storeIt ro2 url3

    Just (Located loc1 _) <- getIt (getHash ro1)
    HU.assertEqual "Wrong locations 1" loc1 (toLocations url1 <> toLocations url2 <> toLocations url3)

    Just (Located loc2 _) <- getIt (getHash ro2)
    HU.assertEqual "Wrong locations 2" loc2 (toLocations url3)    

    verifyUrlCount objectStore "1" 3    

    rwTx objectStore $ \tx ->
        deleteObject tx objectStore (getHash ro1)    

    verifyUrlCount objectStore "2" 3

    -- should only clean up URLs
    cleanObjectCache db (const False)

    verifyUrlCount objectStore "3" 1

    Just (Located loc2 _) <- getIt (getHash ro2)
    HU.assertEqual "Wrong locations 3" loc2 (toLocations url3)
    where 
        verifyUrlCount objectStore s count = do 
            uriToUriKey <- roTx objectStore $ \tx -> M.all tx (uriToUriKey objectStore)
            uriKeyToUri <- roTx objectStore $ \tx -> M.all tx (uriKeyToUri objectStore)
            HU.assertEqual ("Not all URLs one way " <> s) count (length uriToUriKey)
            HU.assertEqual ("Not all URLs backwards " <> s) count (length uriKeyToUri)        


shouldCreateAndDeleteAllTheMaps :: Storage s => IO (DB s) -> HU.Assertion
shouldCreateAndDeleteAllTheMaps io = do 
    
    DB {..} <- io
    Now now <- thisInstant 

    url :: RpkiURL <- QC.generate arbitrary    
    ros :: [RpkiObject] <- generateSome

    rwTx objectStore $ \tx -> 
        for_ ros $ \ro -> 
            putObject tx objectStore (toStorableObject ro) (instantToVersion now)

    -- roTx objectStore $ \tx -> 
    --     forM ros $ \ro -> do 
    --         Just ro' <- getByHash tx objectStore (getHash ro)
    --         HU.assertEqual "Not the same objects" ro ro'
    

    pure ()


shouldInsertAndGetAllBackFromObjectStore :: Storage s => IO (DB s) -> HU.Assertion
shouldInsertAndGetAllBackFromObjectStore io = do  
    DB {..} <- io
    aki1 :: AKI <- QC.generate arbitrary
    aki2 :: AKI <- QC.generate arbitrary
    ros :: [Located RpkiObject] <- removeMftNumberDuplicates <$> generateSome    

    let (firstHalf, secondHalf) = List.splitAt (List.length ros `div` 2) ros

    let ros1 = List.map (& typed @RpkiObject %~ replaceAKI aki1) firstHalf
    let ros2 = List.map (& typed @RpkiObject %~ replaceAKI aki2) secondHalf
    let ros' = ros1 <> ros2 

    Now now <- thisInstant     

    rwTx objectStore $ \tx -> 
        for_ ros' $ \(Located (Locations locations) ro) -> do             
            putObject tx objectStore (toStorableObject ro) (instantToVersion now)
            forM_ locations $ \url -> 
                linkObjectToUrl tx objectStore url (getHash ro)

    allObjects <- roTx objectStore $ \tx -> getAll tx objectStore
    HU.assertEqual 
        "Not the same objects" 
        (List.sortOn (getHash . payload) allObjects) 
        (List.sortOn (getHash . payload) ros')
        
    compareLatestMfts objectStore ros1 aki1    
    compareLatestMfts objectStore ros2 aki2  
    
    let (toDelete, toKeep) = List.splitAt (List.length ros1 `div` 2) ros1

    rwTx objectStore $ \tx -> 
        forM_ toDelete $ \(Located _ ro) -> 
            deleteObject tx objectStore (getHash ro)
    
    compareLatestMfts objectStore ros2 aki2      
    compareLatestMfts objectStore toKeep aki1
    
  where
    removeMftNumberDuplicates = List.nubBy $ \ro1 ro2 ->
            case (ro1, ro2) of
                (Located _ (MftRO mft1), Located _ (MftRO mft2)) -> 
                    getMftTimingMark mft1 == getMftTimingMark mft2
                _ -> False

    compareLatestMfts objectStore ros a = do
        mftLatest <- roTx objectStore $ \tx -> 
                        findLatestMftByAKI tx objectStore a         
        
        let mftLatest' = listToMaybe $ List.sortOn (Down . getMftTimingMark)
                [ mft | Located _ (MftRO mft) <- ros, getAKI mft == Just a ]                                    
        
        HU.assertEqual "Not the same manifests" ((^. #payload) <$> mftLatest) mftLatest'                    


shouldOrderManifests :: Storage s => IO (DB s) -> HU.Assertion
shouldOrderManifests io = do  
    DB {..} <- io
    (url1, Right mft1) <- readObjectFromFile "./test/data/afrinic_mft1.mft"
    (url2, Right mft2) <- readObjectFromFile "./test/data/afrinic_mft2.mft"

    Now now <- thisInstant 
    let worldVersion = instantToVersion now

    rwTx objectStore $ \tx -> do        
            putObject tx objectStore (toStorableObject mft1) worldVersion
            putObject tx objectStore (toStorableObject mft2) worldVersion
            linkObjectToUrl tx objectStore url1 (getHash mft1)
            linkObjectToUrl tx objectStore url2 (getHash mft2)

    -- they have the same AKIs
    let Just aki1 = getAKI mft1
    Just (Located _ mftLatest) <- roTx objectStore $ \tx -> findLatestMftByAKI tx objectStore aki1

    HU.assertEqual "Not the same manifests" (MftRO mftLatest) mft2


shouldInsertAndGetAllBackFromValidationResultStore :: Storage s => IO (DB s) -> HU.Assertion
shouldInsertAndGetAllBackFromValidationResultStore io = do  
    db@DB {..} <- io
    vrs :: Validations <- QC.generate arbitrary      

    world <- getOrCreateWorldVerion =<< newAppState

    rwTx validationsStore $ \tx -> putValidations tx db world vrs
    vrs' <- roTx validationsStore $ \tx -> validationsForVersion tx validationsStore world

    HU.assertEqual "Not the same Validations" (Just vrs) vrs'


shouldInsertAndGetAllBackFromRepositoryStore :: Storage s => IO (DB s) -> HU.Assertion
shouldInsertAndGetAllBackFromRepositoryStore io = do  
    db <- io

    createdPPs <- generateRepositories

    storedPps1 <- roTx db $ \tx -> getPublicationPoints tx db

    let changeSet1 = changeSet storedPps1 createdPPs

    rwTx db $ \tx -> applyChangeSet tx db changeSet1
    storedPps2 <- roTx db $ \tx -> getPublicationPoints tx db

    HU.assertEqual "Not the same publication points" createdPPs storedPps2

    rsyncPPs2 <- QC.generate (QC.sublistOf repositoriesURIs)    
    rrdpMap2  <- rrdpSubMap createdPPs

    let shrunkPPs = List.foldr mergeRsyncPP (PublicationPoints rrdpMap2 newRsyncTree mempty) rsyncPPs2

    let changeSet2 = changeSet storedPps2 shrunkPPs

    rwTx db $ \tx -> applyChangeSet tx db changeSet2
    storedPps3 <- roTx db $ \tx -> getPublicationPoints tx db

    HU.assertEqual "Not the same publication points after shrinking" shrunkPPs storedPps3


shouldGetAndSaveRepositories :: Storage s => IO (DB s) -> HU.Assertion
shouldGetAndSaveRepositories io = do  
    db <- io

    pps1 <- generateRepositories
    rwTx db $ \tx -> savePublicationPoints tx db pps1
    pps1' <- roTx db $ \tx -> getPublicationPoints tx db       

    HU.assertEqual "Not the same publication points first" pps1 pps1'
    
    rsyncPPs2 <- QC.generate (QC.sublistOf repositoriesURIs)    
    rrdpMap2  <- rrdpSubMap pps1

    let pps2 = List.foldr mergeRsyncPP (PublicationPoints rrdpMap2 newRsyncTree mempty) rsyncPPs2

    rwTx db $ \tx -> savePublicationPoints tx db pps2
    pps2' <- roTx db $ \tx -> getPublicationPoints tx db       

    HU.assertEqual "Not the same publication points second" pps2 pps2'
    

generateRepositories :: IO PublicationPoints
generateRepositories = do     
    rrdpMap :: RrdpMap <- QC.generate arbitrary    

    let everSucceeded = Map.fromList [ (RrdpU u, AtLeastOnce) | 
            RrdpRepository { uri = u, status = FetchedAt t } <- Map.elems $ unRrdpMap rrdpMap ]

    let pps = PublicationPoints rrdpMap newRsyncTree (EverSucceededMap everSucceeded)
    pure $ List.foldr mergeRsyncPP pps repositoriesURIs    
    

rrdpSubMap :: PublicationPoints -> IO RrdpMap
rrdpSubMap pps = do 
    let RrdpMap rrdpsM = rrdps pps
    keys <- QC.generate (QC.sublistOf $ Map.keys rrdpsM)
    pure $ RrdpMap $ Map.filterWithKey (\u _ -> u `elem` keys) rrdpsM


shouldRollbackAppTx :: IO ((FilePath, LmdbEnv), DB LmdbStorage) -> HU.Assertion
shouldRollbackAppTx io = do  
    ((_, env), DB {..}) <- io

    let storage' = LmdbStorage env
    z :: SMap "test" LmdbStorage Int String <- SMap storage' <$> createLmdbStore env

    void $ runValidatorT (newScopes "bla") $ rwAppTx storage' $ \tx -> do
        liftIO $ M.put tx z 1 "aa"
        appError $ UnspecifiedE "Test" "Test problem"

    void $ runValidatorT (newScopes "bla") $ rwAppTx storage' $ \tx ->
        liftIO $ M.put tx z 2 "bb"        

    let throwFromTx =
            void $ runValidatorT (newScopes "bla") $ rwAppTx storage' $ \tx -> liftIO $ do
                    M.put tx z 3 "cc"        
                    throwIO RatioZeroDenominator

    Left (SomeException e) <- try throwFromTx    
    HU.assertEqual "Must be the right type of exception" 
            (fromException (toException e)) 
            (Just RatioZeroDenominator)

    void $ runValidatorT (newScopes "bla") $ roAppTx storage' $ \tx -> liftIO $ do         
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

    let addedObject = updateMetric @RrdpMetric @_ (& #added %~ (+1))    

    (_, ValidationState { validations = Validations validationMap, .. }) 
        <- runValidatorT (newScopes "root") $ 
            timedMetric (Proxy :: Proxy RrdpMetric) $ do                 
                appWarn $ UnspecifiedE "Error0" "text 0"
                rwAppTx storage' $ \tx -> do                             
                    addedObject        
                    appWarn $ UnspecifiedE "Error1" "text 1"
                    inSubVScope "nested-1" $ 
                        appWarn $ UnspecifiedE "Error2" "text 2"
                    -- just to have a transaction
                    liftIO $ M.get tx z 0

                appWarn $ UnspecifiedE "Error4" "text 4"
                addedObject

    HU.assertEqual "Root metric should count 2 objects" 
        (Just $ mempty { added = 2, deleted = 0 })
        (stripTime <$> lookupMetric (newScope "root") (rrdpMetrics topDownMetric))        

    HU.assertEqual "Nested metric should count 1 object" 
        Nothing
        (stripTime <$> lookupMetric (subScope "metric-nested-1" (newScope "root"))
                            (rrdpMetrics topDownMetric))        

    HU.assertEqual "Root validations should have 1 warning"     
        (Map.lookup (newScope "root") validationMap)
        (Just $ Set.fromList [
            VWarn (VWarning (UnspecifiedE "Error0" "text 0")),
            VWarn (VWarning (UnspecifiedE "Error1" "text 1")),            
            VWarn (VWarning (UnspecifiedE "Error4" "text 4"))])

    HU.assertEqual "Nested validations should have 1 warning" 
        (Map.lookup (subScope "nested-1" (newScope "root")) validationMap)
        (Just $ Set.fromList [VWarn (VWarning (UnspecifiedE "Error2" "text 2"))])


stripTime :: HasField "totalTimeMs" metric metric TimeMs TimeMs => metric -> metric
stripTime = (& #totalTimeMs .~ TimeMs 0)

generateSome :: Arbitrary a => IO [a]
generateSome = replicateM 1000 $ QC.generate arbitrary      

withDB :: (IO ((FilePath, LmdbEnv), DB LmdbStorage) -> TestTree) -> TestTree
withDB = withResource (makeLmdbStuff createLmdb) releaseLmdb
  where
    createLmdb lmdbEnv = 
        withLogger MainLogger defaultsLogLevel $ \logger -> 
            (Lmdb.createDatabase lmdbEnv logger Lmdb.DontCheckVersion) 


ioTestCase :: TestName -> (IO ((FilePath, LmdbEnv), DB LmdbStorage) -> HU.Assertion) -> TestTree
ioTestCase s f = withDB $ \io -> HU.testCase s (f io)

dbTestCase :: TestName -> (IO (DB LmdbStorage) -> HU.Assertion) -> TestTree
dbTestCase s f = ioTestCase s $ f . (snd <$>)

makeLmdbStuff :: (LmdbEnv -> IO b) -> IO ((FilePath, LmdbEnv), b)
makeLmdbStuff mkStore = do 
    dir <- createTempDirectory "/tmp" "lmdb-test"
    e <- Lmdb.mkLmdb dir 1000 1000 
    store <- mkStore e
    pure ((dir, e), store)

releaseLmdb :: ((FilePath, LmdbEnv), b) -> IO ()
releaseLmdb ((dir, e), _) = do    
    Lmdb.closeLmdb e
    removeDirectoryRecursive dir

readObjectFromFile :: FilePath -> IO (RpkiURL, ParseResult RpkiObject)
readObjectFromFile path = do 
    bs <- BS.readFile path
    let url = let Right u = parseRpkiURL ("rsync://host/" <> Text.pack path) in u
    pure (url, readObject url bs)

replaceAKI :: AKI -> RpkiObject -> RpkiObject
replaceAKI a = \case 
    CerRO c -> CerRO $ c { aki = Just a }
    CrlRO c -> CrlRO $ c { aki = a }
    MftRO c -> MftRO $ c & #cmsPayload %~ mapCms
    RoaRO c -> RoaRO $ c & #cmsPayload %~ mapCms
    GbrRO c -> GbrRO $ c & #cmsPayload %~ mapCms
    RscRO c -> RscRO $ c & #cmsPayload %~ mapCms
    where
        mapCms :: CMS a -> CMS a
        mapCms (CMS so) = CMS $ so & #soContent . #scCertificate . #aki .~ a
