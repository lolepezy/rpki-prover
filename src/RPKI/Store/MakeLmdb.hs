{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}

module RPKI.Store.MakeLmdb where

import Control.Lens
import Control.Concurrent.STM

import           Data.IORef
import           Data.String.Interpolate.IsString

import           GHC.TypeLits

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
data DbCheckResult = WasIncompatible | WasCompatible | DidntHaveVersion

createDatabase :: LmdbEnv -> AppLogger -> IncompatibleDbCheck -> IO (DB LmdbStorage, DbCheckResult)
createDatabase e logger checkAction = do 

    erasables <- newIORef []
    db :: DB LmdbStorage <- doCreate erasables
    
    case checkAction of     
        CheckVersion -> do 
            dbCheck <- verifyDBVersion db
            pure (db, dbCheck)
        DontCheckVersion -> 
            pure (db, WasCompatible)
    
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
                    pure DidntHaveVersion
                Just version -> 
                    if version /= currentDatabaseVersion then do
                        -- We are seeing incompatible storage. The only option 
                        -- now is to erase all the maps and start from scratch.
                        --
                        -- This is obviously far from optimal, so it would make
                        -- sense to automate that part.
                        logInfo logger [i|Persisted cache version is #{version} and expected version is #{currentDatabaseVersion}, dropping the cache.|]    
                        (_, ms) <- timedMS $ emptyDBMaps tx db
                        logDebug logger [i|Erasing cache took #{ms}ms.|]                        
                        saveCurrentDatabaseVersion tx db
                        pure WasIncompatible
                    else
                        pure WasCompatible

    doCreate :: IORef [EraseWrapper LmdbStorage] -> IO (DB LmdbStorage)
    doCreate erasablesRef = do 
        sequences        <- createMap
        taStore          <- TAStore <$> createMap        
        validationsStore <- ValidationsStore <$> createMap
        vrpStore         <- VRPStore <$> createMap    
        aspaStore        <- AspaStore <$> createMap    
        gbrStore         <- GbrStore <$> createMap 
        bgpStore         <- BgpStore <$> createMap
        versionStore     <- VersionStore <$> createMap
        metricStore      <- MetricStore <$> createMap
        slurmStore       <- SlurmStore <$> createMap
        jobStore         <- JobStore <$> createMap        
        metadataStore    <- MetadataStore <$> createMap          
        repositoryStore  <- createRepositoryStore
        objectStore      <- createObjectStore sequences            

        erasables <- readIORef erasablesRef

        pure DB {..}
      where

        createObjectStore seqMap = do 
            let keys = Sequence "object-key" seqMap
            objects          <- createMap
            mftByAKI         <- createMultiMap
            objectInsertedBy <- createMap        
            hashToKey        <- createMap
            lastValidMfts    <- createMap
            uriToUriKey      <- createMap
            uriKeyToUri      <- createMap
            urlKeyToObjectKey  <- createMultiMap
            objectKeyToUrlKeys <- createMap
            certBySKI          <- createMap
            objectBriefs       <- createMap
            validatedByVersion <- createMap        
            pure RpkiObjectStore {..}
            
        createRepositoryStore = RepositoryStore <$> createMap <*> createMap <*> createMap      
        
        createMap :: forall k v name . (KnownSymbol name) => IO (SMap name LmdbStorage k v)
        createMap = do 
            sm <- SMap lmdb <$> createLmdbStore e
            modifyIORef' erasablesRef (EraseWrapper sm :)
            pure sm

        createMultiMap :: forall k v name . (KnownSymbol name) => IO (SMultiMap name LmdbStorage k v)
        createMultiMap = do 
            sm <- SMultiMap lmdb <$> createLmdbMultiStore e
            modifyIORef' erasablesRef (EraseWrapper sm :)
            pure sm
        

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
