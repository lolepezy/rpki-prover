{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module RPKI.Store.MakeLmdb where

import Control.Concurrent.STM
import Data.Text (Text)

import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))

import           Lmdb.Connection
import           Lmdb.Types hiding (Size)

import           RPKI.Store.Base.LMDB
import           RPKI.Config
import           RPKI.Store.Database
import           RPKI.Store.Repository
import           RPKI.Store.Sequence



createObjectStore :: LmdbEnv -> SequenceMap LmdbStorage -> IO (RpkiObjectStore LmdbStorage)
createObjectStore e seqMap = do 
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
        pure RpkiObjectStore {..}
    where 
        lmdb = LmdbStorage e        

createRepositoryStore :: LmdbEnv -> IO (RepositoryStore LmdbStorage)
createRepositoryStore e = 
    RepositoryStore <$>
        (SMap lmdb <$> createLmdbStore e) <*>
        (SMap lmdb <$> createLmdbStore e) <*>
        (SMap lmdb <$> createLmdbStore e)
    where 
        lmdb = LmdbStorage e

createResultStore :: LmdbEnv -> IO (ValidationsStore LmdbStorage)
createResultStore e = 
    ValidationsStore <$> (SMap lmdb <$> createLmdbStore e)
    where 
        lmdb = LmdbStorage e

createVRPStore :: LmdbEnv -> IO (VRPStore LmdbStorage)
createVRPStore e = VRPStore . SMap (LmdbStorage e) <$> createLmdbStore e    

createTAStore :: LmdbEnv -> IO (TAStore LmdbStorage)
createTAStore e = TAStore . SMap (LmdbStorage e) <$> createLmdbStore e    

createVersionStore :: LmdbEnv -> IO (VersionStore LmdbStorage)
createVersionStore e = VersionStore . SMap (LmdbStorage e) <$> createLmdbStore e    

createMetricsStore :: LmdbEnv -> IO (MetricStore LmdbStorage)
createMetricsStore e = MetricStore . SMap (LmdbStorage e) <$> createLmdbStore e    

createSlurmStore :: LmdbEnv -> IO (SlurmStore LmdbStorage)
createSlurmStore e = SlurmStore . SMap (LmdbStorage e) <$> createLmdbStore e    

createSequenceStore :: LmdbEnv -> Text -> IO (Sequence LmdbStorage)
createSequenceStore e seqName = Sequence seqName . SMap (LmdbStorage e) <$> createLmdbStore e    


mkLmdb :: FilePath -> Size -> Int -> IO LmdbEnv
mkLmdb fileName maxSizeMb maxReaders = do 
    nativeEnv <- newNativeLmdb fileName maxSizeMb maxReaders
    LmdbEnv <$> 
        newTVarIO (RWEnv nativeEnv) <*>
        createSemaphore maxReaders    

newNativeLmdb :: FilePath -> Size -> Int -> IO (Environment 'ReadWrite)
newNativeLmdb fileName (Size maxSizeMb) maxReaders = 
    initializeReadWriteEnvironment (fromIntegral mapSize) maxReaders maxDatabases fileName        
    where
        mapSize = maxSizeMb * 1024 * 1024
        maxDatabases = 120

closeLmdb :: LmdbEnv -> IO ()
closeLmdb e = closeEnvironment =<< atomically (getNativeEnv e)

closeNativeLmdb :: Environment e -> IO ()
closeNativeLmdb = closeEnvironment


createDatabase :: LmdbEnv -> IO (DB LmdbStorage)
createDatabase e = do 
    seqMap <- SMap (LmdbStorage e) <$> createLmdbStore e
    DB <$>
        createTAStore e <*>
        createRepositoryStore e <*>
        createObjectStore e seqMap <*>
        createResultStore e <*>
        createVRPStore e <*>
        createVersionStore e <*>
        createMetricsStore e <*>
        createSlurmStore e <*>
        pure seqMap

