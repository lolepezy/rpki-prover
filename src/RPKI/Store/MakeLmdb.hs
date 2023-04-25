{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}

module RPKI.Store.MakeLmdb where

import Control.Lens
import Control.Monad
import Control.Concurrent.STM

import           Data.String.Interpolate.IsString

import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))

import           Lmdb.Connection
import           Lmdb.Types hiding (Size)

import           RPKI.Store.Base.LMDB
import           RPKI.Config
import           RPKI.Parallel
import           RPKI.Logging
import           RPKI.Time
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import           RPKI.Store.Sequence


data IncompatibleDbCheck = CheckVersion | DontCheckVersion

createDatabase :: LmdbEnv -> AppLogger -> IncompatibleDbCheck -> IO (DB LmdbStorage)
createDatabase e logger checkAction = do 
    sequences <- SMap lmdb <$> createLmdbStore e
    taStore          <- createTAStore
    repositoryStore  <- createRepositoryStore
    objectStore      <- createObjectStore sequences    
    validationsStore <- createValidationsStore
    vrpStore         <- createVRPStore    
    aspaStore        <- createAspaStore
    bgpStore         <- createBgpStore
    versionStore     <- createVersionStore
    metricStore      <- createMetricsStore
    slurmStore       <- createSlurmStore
    jobStore         <- createJobStore        
    metadataStore    <- createMetadataStore
    
    let db = DB {..}
    case checkAction of     
        CheckVersion     -> verifyDBVersion db
        DontCheckVersion -> pure ()
    pure db 
  where
    lmdb = LmdbStorage e        

    verifyDBVersion db =
        rwTx db $ \tx -> do     
            dbVersion <- getDatabaseVersion tx db
            case dbVersion of 
                Nothing -> do
                    logInfo logger [i|Cache version is not set, setting it to #{currentDatabaseVersion}, dropping the cache.|]
                    (_, ms) <- timedMS $ emptyDBMaps tx db
                    logDebug logger  [i|Erasing cache took #{ms}ms.|]
                    saveCurrentDatabaseVersion tx db
                Just version -> 
                    when (version /= currentDatabaseVersion) $ do
                        -- We are seeing incompatible storage. The only option 
                        -- now is to erase all the maps and start from scratch.
                        --
                        -- This is obviously far from optimal, so it would make
                        -- sense to automate that part.
                        logInfo logger [i|Cache version is #{version} and current version is #{currentDatabaseVersion}, dropping the cache.|]    
                        (_, ms) <- timedMS $ emptyDBMaps tx db
                        logDebug logger [i|Erasing cache took #{ms}ms.|]
                        saveCurrentDatabaseVersion tx db

    createObjectStore seqMap = do 
        let keys = Sequence "object-key" seqMap
        objects  <- SMap lmdb <$> createLmdbStore e
        mftByAKI <- SMultiMap lmdb <$> createLmdbMultiStore e
        objectInsertedBy <- SMap lmdb <$> createLmdbStore e
        objectValidatedBy <- SMap lmdb <$> createLmdbStore e        
        hashToKey   <- SMap lmdb <$> createLmdbStore e
        lastValidMft <- SMap lmdb <$> createLmdbStore e

        uriToUriKey <- SMap lmdb <$> createLmdbStore e
        uriKeyToUri <- SMap lmdb <$> createLmdbStore e
        urlKeyToObjectKey  <- SMultiMap lmdb <$> createLmdbMultiStore e
        objectKeyToUrlKeys <- SMap lmdb <$> createLmdbStore e
        certBySKI <- SMap lmdb <$> createLmdbStore e
        objectBriefs <- SMap lmdb <$> createLmdbStore e
        pure RpkiObjectStore {..}
            
    createRepositoryStore = 
        RepositoryStore <$>
            (SMap lmdb <$> createLmdbStore e) <*>
            (SMap lmdb <$> createLmdbStore e) <*>
            (SMap lmdb <$> createLmdbStore e)        
    
    createValidationsStore = ValidationsStore . SMap lmdb <$> createLmdbStore e
    createVRPStore = VRPStore . SMap lmdb <$> createLmdbStore e    
    createAspaStore = AspaStore . SMap lmdb <$> createLmdbStore e    
    createBgpStore = BgpStore . SMap lmdb <$> createLmdbStore e    
    createTAStore = TAStore . SMap lmdb <$> createLmdbStore e    
    createVersionStore = VersionStore . SMap lmdb <$> createLmdbStore e    
    createMetricsStore = MetricStore . SMap lmdb <$> createLmdbStore e    
    createSlurmStore = SlurmStore . SMap lmdb <$> createLmdbStore e    
    createJobStore = JobStore . SMap lmdb <$> createLmdbStore e    
    createMetadataStore = MetadataStore . SMap lmdb <$> createLmdbStore e    
    

mkLmdb :: FilePath -> Config -> IO LmdbEnv
mkLmdb fileName config = do 
    nativeEnv <- initializeReadWriteEnvironment (fromIntegral mapSize) 
                    maxReaders maxDatabases fileName
    LmdbEnv <$> 
        newTVarIO (RWEnv nativeEnv) <*>
        createSemaphoreIO maxBottleNeck
  where    
    mapSize = unSize (config ^. #lmdbSizeMb) * 1024 * 1024
    maxDatabases = 120    
    maxBottleNeck = 64    
    maxReaders = (maxBottleNeck + 1) * maxProcesses
    -- main process + validator + fetchers
    maxProcesses = 2 + fromIntegral (config ^. #parallelism . #fetchParallelism)

closeLmdb :: LmdbEnv -> IO ()
closeLmdb e = closeEnvironment =<< atomically (getNativeEnv e)

closeNativeLmdb :: Environment e -> IO ()
closeNativeLmdb = closeEnvironment
