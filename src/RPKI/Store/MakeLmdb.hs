{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}

module RPKI.Store.MakeLmdb where

import Control.Lens
import Control.Concurrent.STM

import           Data.String.Interpolate.IsString

import           GHC.TypeLits

import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))

import           Lmdb.Connection
import           Lmdb.Types hiding (Size)

import           RPKI.Store.Base.LMDB
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Parallel
import           RPKI.Logging
import           RPKI.Time
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import           RPKI.Store.Sequence


data IncompatibleDbCheck = CheckVersion | DontCheckVersion
data DbCheckResult = WasIncompatible | WasCompatible | DidntHaveVersion

createDatabase :: LmdbEnv -> AppLogger -> IncompatibleDbCheck -> IO (DB LmdbStorage, DbCheckResult)
createDatabase env logger checkAction = do 
    
    db <- doCreateDb
    
    case checkAction of     
        CheckVersion -> 
            (db, ) <$> verifyDBVersion db            
        DontCheckVersion -> 
            pure (db, WasCompatible)    
  where    

    verifyDBVersion db =
        rwTx db $ \tx -> do     
            dbVersion <- getDatabaseVersion tx db
            case dbVersion of 
                Nothing -> do
                    logInfo logger [i|Cache version is not set, will set the version to #{currentDatabaseVersion} and clean up the cache.|]                    
                    ms <- eraseCache tx
                    logDebug logger [i|Erased cache in #{ms}ms.|]
                    saveCurrentDatabaseVersion tx db
                    pure DidntHaveVersion
                Just version
                    | version == currentDatabaseVersion -> 
                        pure WasCompatible
                    | otherwise -> do
                        -- We are seeing incompatible storage. The only option 
                        -- now is to erase all the maps and start from scratch.
                        logInfo logger [i|Persisted cache version is #{version} and expected version is #{currentDatabaseVersion}, will drop the cache.|]    
                        -- NOTE: We erase every map in the cache, including metadata, but that's not an problem, 
                        -- since we'll set new DB version here
                        ms <- eraseCache tx
                        logDebug logger [i|Erased cache in #{ms}ms.|]                  
                        saveCurrentDatabaseVersion tx db
                        pure WasIncompatible                        

    eraseCache tx = do 
        nativeEnv <- atomically $ getNativeEnv env
        (_, ms) <- timedMS $ eraseEnv nativeEnv tx        
        pure ms

    doCreateDb = do 
        sequences        <- createMap
        taStore          <- TAStore <$> createMap        
        validationsStore <- ValidationsStore <$> createMap
        vrpStore         <- VRPStore <$> createMap
        splStore         <- SplStore <$> createMap
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
        pure DB {..}
      where

        createObjectStore seqMap = do 
            let keys = Sequence "object-key" seqMap
            objects          <- createMap
            mftByAKI         <- createMultiMap
            objectMetas      <- createMap        
            hashToKey        <- createMap
            uriToUriKey      <- createMap
            uriKeyToUri      <- createMap
            urlKeyToObjectKey  <- createMultiMap
            objectKeyToUrlKeys <- createMap
            certBySKI          <- createMap
            validatedByVersion <- createMap                    
            mftShortcuts       <- MftShortcutStore <$> createMap <*> createMap
            originals          <- createMap
            pure RpkiObjectStore {..}
            
        createRepositoryStore = 
            RepositoryStore <$> createMap <*> createMap <*> createMap
        
        lmdb = LmdbStorage env

        createMap :: forall k v name . (KnownSymbol name) => IO (SMap name LmdbStorage k v)
        createMap = SMap lmdb <$> createLmdbStore env            

        createMultiMap :: forall k v name . (KnownSymbol name) => IO (SMultiMap name LmdbStorage k v)
        createMultiMap = SMultiMap lmdb <$> createLmdbMultiStore env            
        

mkLmdb :: FilePath -> Config -> IO LmdbEnv
mkLmdb fileName config = do 
    nativeEnv <- initializeReadWriteEnvironment (fromIntegral mapSize) 
                    maxReaders maxDatabases fileName
    LmdbEnv <$> 
        newTVarIO (RWEnv nativeEnv) <*>
        newSemaphoreIO maxBottleNeck
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
