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
import           Data.Maybe
import           Data.Ord
import           Data.Proxy
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
import           RPKI.Reporting
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.Store.Base.LMDB
import           RPKI.Store.Base.Map               as M
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import           RPKI.Store.Repository
import           RPKI.Store.Util
import           RPKI.Time

import           RPKI.RepositorySpec
import qualified Data.Set as Set
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
objectStoreGroup = withDB $ \io -> testGroup "Object storage test"
    [
        HU.testCase "Should insert and get back" (shouldInsertAndGetAllBackFromObjectStore io),        
        HU.testCase "Should order manifests accoring to their dates" (shouldOrderManifests io)
    ]

validationResultStoreGroup :: TestTree
validationResultStoreGroup = withDB $ \io -> testGroup "Validation result storage test"
    [
        HU.testCase "Should insert and get back" (shouldInsertAndGetAllBackFromValidationResultStore io)        
    ]

repositoryStoreGroup :: TestTree
repositoryStoreGroup = withDB $ \io -> testGroup "Repository LMDB storage test"
    [
        HU.testCase "Should insert and get a repository" (shouldInsertAndGetAllBackFromRepositoryStore io)
        -- HU.testCase "Should use repository change set properly" (should_read_create_change_set_and_apply_repository_store io)
    ]

txGroup :: TestTree
txGroup = withDB $ \io -> testGroup "App transaction test"
    [
        HU.testCase "Should rollback App transactions properly" (shouldRollbackAppTx io),        
        HU.testCase "Should preserve state from StateT in transactions" (shouldPreserveStateInAppTx io)        
    ]



shouldInsertAndGetAllBackFromObjectStore :: IO ((FilePath, LmdbEnv), DB LmdbStorage) -> HU.Assertion
shouldInsertAndGetAllBackFromObjectStore io = do  
    (_, DB {..}) <- io
    aki1 :: AKI <- QC.generate arbitrary
    aki2 :: AKI <- QC.generate arbitrary
    ros :: [RpkiObject] <- removeMftNumberDuplicates <$> generateSome

    let (firstHalf, secondHalf) = List.splitAt (List.length ros `div` 2) ros

    let ros1 = List.map (replaceAKI aki1) firstHalf
    let ros2 = List.map (replaceAKI aki2) secondHalf
    let ros' = ros1 <> ros2 

    Now now <- thisInstant 

    rwTx objectStore $ \tx -> 
        for_ ros' $ \ro -> 
            putObject tx objectStore (toStorableObject ro) (instantToVersion now)

    allObjects <- roTx objectStore $ \tx -> getAll tx objectStore
    HU.assertEqual 
        "Not the same objects" 
        (List.sortOn getHash allObjects) 
        (List.sortOn getHash ros')
    
    compareLatestMfts objectStore ros1 aki1
    compareLatestMfts objectStore ros2 aki2  
    
    let (toDelete, toKeep) = List.splitAt (List.length ros1 `div` 2) ros1

    rwTx objectStore $ \tx -> 
        forM_ toDelete $ \ro -> 
            deleteObject tx objectStore (getHash ro)

    compareLatestMfts objectStore toKeep aki1
    compareLatestMfts objectStore ros2 aki2  

    where
        removeMftNumberDuplicates = List.nubBy sameMftNumber
            where 
                sameMftNumber ro1 ro2 = 
                    case (ro1, ro2) of
                        (MftRO mft1, MftRO mft2) -> getMftTimingMark mft1 == getMftTimingMark mft2
                        _ -> False

        compareLatestMfts objectStore ros a = do
            mftLatest <- roTx objectStore $ \tx -> findLatestMftByAKI tx objectStore a         
            
            let mftLatest' = listToMaybe $ List.sortOn (Down . getMftTimingMark)
                    [ mft | MftRO mft <- ros, getAKI mft == Just a ]
                
            HU.assertEqual "Not the same manifests" mftLatest mftLatest'


shouldOrderManifests :: IO ((FilePath, LmdbEnv), DB LmdbStorage) -> HU.Assertion
shouldOrderManifests io = do  
    (_, DB {..}) <- io
    Right mft1 <- readObjectFromFile "./test/data/afrinic_mft1.mft"
    Right mft2 <- readObjectFromFile "./test/data/afrinic_mft2.mft"

    Now now <- thisInstant 
    let worldVersion = instantToVersion now

    rwTx objectStore $ \tx -> do        
            putObject tx objectStore (toStorableObject mft1) worldVersion
            putObject tx objectStore (toStorableObject mft2) worldVersion

    -- they have the same AKIs
    let Just aki1 = getAKI mft1
    Just mftLatest <- roTx objectStore $ \tx -> findLatestMftByAKI tx objectStore aki1

    HU.assertEqual "Not the same manifests" (MftRO mftLatest) mft2


shouldInsertAndGetAllBackFromValidationResultStore :: IO ((FilePath, LmdbEnv), DB LmdbStorage) -> HU.Assertion
shouldInsertAndGetAllBackFromValidationResultStore io = do  
    (_, DB {..}) <- io
    vrs :: Validations <- QC.generate arbitrary      

    world <- getWorldVerionIO =<< newAppState

    rwTx validationsStore $ \tx -> putValidations tx validationsStore world vrs
    vrs' <- roTx validationsStore $ \tx -> validationsForVersion tx validationsStore world

    HU.assertEqual "Not the same Validations" (Just vrs) vrs'


shouldInsertAndGetAllBackFromRepositoryStore :: IO ((FilePath, LmdbEnv), DB LmdbStorage) -> HU.Assertion
shouldInsertAndGetAllBackFromRepositoryStore io = do  
    (_, DB {..}) <- io

    let rsyncPPs = fromRsyncPPs repositoriesURIs 
    rrdpMap :: RrdpMap <- QC.generate arbitrary    

    let lastSuccess = Map.fromList [ (RrdpU u, FetchLastSuccess t) | 
            RrdpRepository { uri = u, status = FetchedAt t } <- Map.elems $ unRrdpMap rrdpMap ]

    let createdPPs = rsyncPPs <> PublicationPoints rrdpMap mempty (LastSuccededMap lastSuccess)

    storedPps1 <- roTx repositoryStore $ \tx -> 
                    getPublicationPoints tx repositoryStore

    let changeSet1 = changeSet storedPps1 createdPPs

    rwTx repositoryStore $ \tx -> 
            applyChangeSet tx repositoryStore changeSet1

    storedPps2 <- roTx repositoryStore $ \tx -> 
                    getPublicationPoints tx repositoryStore

    HU.assertEqual "Not the same publication points" createdPPs storedPps2

    rsyncPPs2 <- fromRsyncPPs <$> QC.generate (QC.sublistOf repositoriesURIs)

    let RrdpMap rrdpsM = rrdpMap    
    keys <- QC.generate (QC.sublistOf $ Map.keys rrdpsM)
    let rrdpMap2 = RrdpMap $ Map.filterWithKey (\u _ -> u `elem` keys) rrdpsM

    let shrunkPPs = rsyncPPs2 <> PublicationPoints rrdpMap2 mempty mempty

    let changeSet2 = changeSet storedPps2 shrunkPPs

    rwTx repositoryStore $ \tx -> 
            applyChangeSet tx repositoryStore changeSet2

    storedPps3 <- roTx repositoryStore $ \tx -> 
                    getPublicationPoints tx repositoryStore    

    HU.assertEqual "Not the same publication points after shrinking" shrunkPPs storedPps3

    
shouldRollbackAppTx :: IO ((FilePath, LmdbEnv), DB LmdbStorage) -> HU.Assertion
shouldRollbackAppTx io = do  
    ((_, env), DB {..}) <- io

    let storage' = LmdbStorage env
    z :: SMap "test" LmdbStorage Int String <- SMap storage' <$> createLmdbStore env

    void $ runValidatorT (newValidatorPath "bla") $ rwAppTx storage' $ \tx -> do
        liftIO $ M.put tx z 1 "aa"
        appError $ UnspecifiedE "Test" "Test problem"

    void $ runValidatorT (newValidatorPath "bla") $ rwAppTx storage' $ \tx ->
        liftIO $ M.put tx z 2 "bb"        

    let throwFromTx =
            void $ runValidatorT (newValidatorPath "bla") $ rwAppTx storage' $ \tx -> liftIO $ do
                    M.put tx z 3 "cc"        
                    throwIO RatioZeroDenominator

    Left (SomeException e) <- try throwFromTx    
    HU.assertEqual "Must be the right type of exception" 
            (fromException (toException e)) 
            (Just RatioZeroDenominator)

    void $ runValidatorT (newValidatorPath "bla") $ roAppTx storage' $ \tx -> liftIO $ do         
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

    let addedObject   = updateMetric @RrdpMetric @_ (& #added %~ (+1))    

    (_, ValidationState { validations = Validations validationMap, .. }) 
        <- runValidatorT (newValidatorPath "root") $ 
            timedMetric (Proxy :: Proxy RrdpMetric) $ do                 
                appWarn $ UnspecifiedE "Error0" "text 0"
                rwAppTx storage' $ \tx -> do                             
                    addedObject        
                    appWarn $ UnspecifiedE "Error1" "text 1"
                    inSubVPath "nested-1" $ 
                        appWarn $ UnspecifiedE "Error2" "text 2"
                    -- just to have a transaction
                    liftIO $ M.get tx z 0
                    inSubMetricPath "metric-nested-1" $ do 
                        timedMetric (Proxy :: Proxy RrdpMetric) $ do                 
                            appWarn $ UnspecifiedE "Error3" "text 3"
                            addedObject

                appWarn $ UnspecifiedE "Error4" "text 4"
                addedObject

    HU.assertEqual "Root metric should count 2 objects" 
        (Just $ RrdpMetric { added = 2, deleted = 0, rrdpSource = RrdpNothing, 
                             downloadTakenMs = TimeTakenMs 0, 
                             saveTakenMs = TimeTakenMs 0, 
                             timeTakenMs = TimeTakenMs 0 })
        (stripTime <$> lookupMetric (newPath "root") (rrdpMetrics topDownMetric))        

    HU.assertEqual "Nested metric should count 1 object" 
        (Just $ RrdpMetric { added = 1, deleted = 0, rrdpSource = RrdpNothing, 
                             downloadTakenMs = TimeTakenMs 0, 
                             saveTakenMs = TimeTakenMs 0, 
                             timeTakenMs = TimeTakenMs 0 })
        (stripTime <$> lookupMetric (newPath "metric-nested-1" <> newPath "root") 
                            (rrdpMetrics topDownMetric))        

    HU.assertEqual "Root validations should have 1 warning"     
        (Map.lookup (newPath "root") validationMap)
        (Just $ Set.fromList [
            VWarn (VWarning (UnspecifiedE "Error0" "text 0")),
            VWarn (VWarning (UnspecifiedE "Error1" "text 1")),
            VWarn (VWarning (UnspecifiedE "Error3" "text 3")),
            VWarn (VWarning (UnspecifiedE "Error4" "text 4"))])

    HU.assertEqual "Nested validations should have 1 warning" 
        (Map.lookup (newPath "nested-1" <> newPath "root") validationMap)
        (Just $ Set.fromList [VWarn (VWarning (UnspecifiedE "Error2" "text 2"))])


stripTime :: HasField "timeTakenMs" metric metric TimeTakenMs TimeTakenMs => metric -> metric
stripTime = (& #timeTakenMs .~ TimeTakenMs 0)

generateSome :: Arbitrary a => IO [a]
generateSome = forM [1 :: Int .. 1000] $ const $ QC.generate arbitrary      

withDB :: (IO ((FilePath, LmdbEnv), DB LmdbStorage) -> TestTree) -> TestTree
withDB = withResource (makeLmdbStuff createDatabase) releaseLmdb


makeLmdbStuff :: (LmdbEnv -> IO b) -> IO ((FilePath, LmdbEnv), b)
makeLmdbStuff mkStore = do 
    dir <- createTempDirectory "/tmp" "lmdb-test"
    e <- mkLmdb dir 1000 1000 
    store <- mkStore e
    pure ((dir, e), store)

releaseLmdb :: ((FilePath, LmdbEnv), b) -> IO ()
releaseLmdb ((dir, e), _) = do    
    closeLmdb e
    removeDirectoryRecursive dir

readObjectFromFile :: FilePath -> IO (ParseResult RpkiObject)
readObjectFromFile path = do 
    bs <- BS.readFile path
    pure $! readObject (RsyncU $ RsyncURL $ URI $ Text.pack path) bs

replaceAKI :: AKI -> RpkiObject -> RpkiObject
replaceAKI a = go 
    where
        go (CerRO c) = CerRO $ c { aki = Just a }
        go (CrlRO c) = CrlRO $ c { aki = a }
        go (MftRO c) = MftRO $ c { cmsPayload = mapCms $ cmsPayload c }
        go (RoaRO c) = RoaRO $ c { cmsPayload = mapCms $ cmsPayload c }
        go (GbrRO c) = GbrRO $ c { cmsPayload = mapCms $ cmsPayload c }

        mapCms :: CMS a -> CMS a
        mapCms (CMS so) = CMS $ so { soContent = sc { scCertificate = ee' } }
            where 
                ee = scCertificate sc
                sc = soContent so
                ee' :: EECerObject = ee { aki = a :: AKI }
