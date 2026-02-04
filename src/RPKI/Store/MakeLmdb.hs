{-# LANGUAGE OverloadedStrings #-}

module RPKI.Store.MakeLmdb where

import Control.Lens
import Control.Concurrent.STM

import           Data.String.Interpolate.IsString
import           Data.Foldable                   (for_)

import           GHC.TypeLits

import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))
import           RPKI.Store.Base.SafeMap  (SafeMap (..))

import           Lmdb.Connection

import           RPKI.Store.Base.LMDB
import           RPKI.Config
import           RPKI.AppTypes
import           RPKI.Parallel
import           RPKI.Logging
import           RPKI.Time
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database    
import           RPKI.Store.Sequence


data IncompatibleDbCheck = CheckVersion | DontCheckVersion
data DbCheckResult = WasIncompatible | WasCompatible | DidntHaveVersion

createDatabase :: LmdbEnv -> AppLogger -> Config -> IncompatibleDbCheck -> IO (DB LmdbStorage, DbCheckResult)
createDatabase env logger config checkAction = do 
    
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
        sequences        <- newSMap
        let keys = Sequence "object-key" sequences
        taStore          <- TAStore <$> newSafeMap        
        validationsStore <- ValidationsStore <$> newSMap
        roaStore         <- RoaStore <$> newSMap
        splStore         <- SplStore <$> newSMap
        aspaStore        <- AspaStore <$> newSMap    
        gbrStore         <- GbrStore <$> newSMap 
        bgpStore         <- BgpStore <$> newSMap
        versionStore     <- VersionStore <$> newSMap
        metricStore      <- MetricStore <$> newSMap
        slurmStore       <- SlurmStore <$> newSMap
        jobStore         <- JobStore <$> newSMap        
        metadataStore    <- MetadataStore <$> newSMap          
        repositoryStore  <- createRepositoryStore
        objectStore      <- createObjectStore
        pure DB {..}
      where

        createObjectStore = do             
            objects          <- newSMap
            mftsForKI        <- newSMultiMap
            objectMetas      <- newSMap
            hashToKey        <- newSMap
            uriToUriKey      <- newSafeMap
            uriKeyToUri      <- newSMap
            urlKeyToObjectKey  <- newSMultiMap
            objectKeyToUrlKeys <- newSMap
            certBySKI          <- newSMap
            validatedByVersion <- newSMap                    
            mftShortcuts       <- MftShortcutStore <$> newSMap <*> newSMap
            originals          <- newSMap
            indexStore         <- createIndexStore
            pure RpkiObjectStore {..}
            
        createRepositoryStore = 
            RepositoryStore <$> newSafeMap <*> newSafeMap <*> newSafeMap <*> newSafeMap
        
        createIndexStore = do
            kiMetas  <- newSMap
            cert2mft  <- newSMap
            mftShorts <- newSMap
            expiresAt <- newSMultiMap
            maturesAt <- newSMultiMap
            repository2object <- newSMultiMap
            caShortcuts <- newSMap            
            pure IndexStore {..}

        lmdb = LmdbStorage env 
                (config ^. #storageConfig . #rwTransactionTimeout)

        newSMap :: forall k v name . (KnownSymbol name) => IO (SMap name LmdbStorage k v)
        newSMap = SMap lmdb <$> createLmdbStore lmdb            

        newSMultiMap :: forall k v name . (KnownSymbol name) => IO (SMultiMap name LmdbStorage k v)
        newSMultiMap = SMultiMap lmdb <$> createLmdbMultiStore lmdb            

        newSafeMap = SafeMap <$> newSMap <*> newSMap <*> pure maxLmdbKeyBytes

        maxLmdbKeyBytes = 511
        

mkLmdb :: FilePath -> Config -> IO LmdbEnv
mkLmdb directory config = do 
    nativeEnv <- initializeReadWriteEnvironment (fromIntegral mapSize) 
                    maxReaders maxDatabases directory
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
closeLmdb e = do
    env <- atomically $ getNativeEnvAndDisable `orElse` pure Nothing        
    for_ env closeEnvironment
 where
    getNativeEnvAndDisable = do 
        nativeEnv <- getNativeEnv e
        disableNativeEnv e
        pure $ Just nativeEnv
