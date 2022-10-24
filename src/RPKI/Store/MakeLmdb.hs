{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module RPKI.Store.MakeLmdb where

import Control.Concurrent.STM

import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))

import           Lmdb.Connection
import           Lmdb.Types hiding (Size)

import           RPKI.Store.Base.LMDB
import           RPKI.Config
import           RPKI.Parallel
import           RPKI.Store.Database
import           RPKI.Store.Sequence


createDatabase :: LmdbEnv -> IO (DB LmdbStorage)
createDatabase e = do 
    sequences <- SMap lmdb <$> createLmdbStore e
    taStore          <- createTAStore
    repositoryStore  <- createRepositoryStore
    objectStore      <- createObjectStore sequences
    validationsStore <- createValidationsStore
    vrpStore         <- createVRPStore
    versionStore     <- createVersionStore
    metricStore      <- createMetricsStore
    slurmStore       <- createSlurmStore
    jobStore         <- createJobStore        
    pure DB {..}    
  where
    lmdb = LmdbStorage e        
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
        pure RpkiObjectStore {..}
            
    createRepositoryStore = 
        RepositoryStore <$>
            (SMap lmdb <$> createLmdbStore e) <*>
            (SMap lmdb <$> createLmdbStore e) <*>
            (SMap lmdb <$> createLmdbStore e)        

    createValidationsStore = ValidationsStore . SMap lmdb <$> createLmdbStore e
    createVRPStore = VRPStore . SMap lmdb <$> createLmdbStore e    
    createTAStore = TAStore . SMap lmdb <$> createLmdbStore e    
    createVersionStore = VersionStore . SMap lmdb <$> createLmdbStore e    
    createMetricsStore = MetricStore . SMap lmdb <$> createLmdbStore e    
    createSlurmStore = SlurmStore . SMap lmdb <$> createLmdbStore e    
    createJobStore = JobStore . SMap lmdb <$> createLmdbStore e    
    
mkLmdb :: FilePath -> Size -> Int -> IO LmdbEnv
mkLmdb fileName maxSizeMb maxReaders = do 
    nativeEnv <- newNativeLmdb maxSizeMb
    LmdbEnv <$> 
        newTVarIO (RWEnv nativeEnv) <*>
        createSemaphoreIO maxReaders    
  where    
    newNativeLmdb (Size maxSizeMb) =
        initializeReadWriteEnvironment (fromIntegral mapSize) maxReaders maxDatabases fileName        
      where
        mapSize = maxSizeMb * 1024 * 1024
        maxDatabases = 120

closeLmdb :: LmdbEnv -> IO ()
closeLmdb e = closeEnvironment =<< atomically (getNativeEnv e)

closeNativeLmdb :: Environment e -> IO ()
closeNativeLmdb = closeEnvironment
