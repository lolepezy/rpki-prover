{-# LANGUAGE OverloadedStrings #-}
module RPKI.Store.Util where


import Data.Text (Text)

import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))

import           Lmdb.Connection
import           Lmdb.Types hiding (Size)
import           RPKI.Store.Base.LMDB

import           Data.Int                 (Int64)
import           RPKI.Store.Database
import           RPKI.Store.Repository
import           RPKI.Store.Sequence
import Control.Concurrent.STM
import RPKI.Config



createObjectStore :: LmdbEnv -> SequenceMap LmdbStorage -> IO (RpkiObjectStore LmdbStorage)
createObjectStore e seqMap =
    RpkiObjectStore 
        (Sequence "object-key" seqMap) <$>
        (SMap lmdb <$> createLmdbStore e) <*>        
        (SMap lmdb <$> createLmdbStore e) <*>
        (SMap lmdb <$> createLmdbStore e) <*>
        (SMultiMap lmdb <$> createLmdbMultiStore e) <*>
        (SMap lmdb <$> createLmdbStore e)
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

createMetricsStore :: LmdbEnv -> IO (MetricsStore LmdbStorage)
createMetricsStore e = MetricsStore . SMap (LmdbStorage e) <$> createLmdbStore e    

createSequenceStore :: LmdbEnv -> Text -> IO (Sequence LmdbStorage)
createSequenceStore e seqName = Sequence seqName . SMap (LmdbStorage e) <$> createLmdbStore e    


mkLmdb :: FilePath -> Size -> Int -> IO LmdbEnv
mkLmdb fileName size maxReaders = do 
    nativeEnv <- newNativeLmdb fileName size maxReaders
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
        pure seqMap

