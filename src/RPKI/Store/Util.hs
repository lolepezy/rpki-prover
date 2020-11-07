{-# LANGUAGE OverloadedStrings #-}
module RPKI.Store.Util where


import Data.Text (Text)

import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))

import           Lmdb.Connection
import           Lmdb.Types
import           RPKI.Store.Base.LMDB

import           Data.Int                 (Int64)
import           RPKI.Store.Database
import           RPKI.Store.Repository
import           RPKI.Store.Sequence
import Control.Concurrent.STM



createObjectStore :: LmdbEnv -> SequenceMap LmdbStorage -> IO (RpkiObjectStore LmdbStorage)
createObjectStore e seqMap =
    RpkiObjectStore <$>
        pure (Sequence "object-key" seqMap) <*>
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
        (SMap lmdb <$> createLmdbStore e) <*>
        (SMultiMap lmdb <$> createLmdbMultiStore e)
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

createSequenceStore :: LmdbEnv -> Text -> IO (Sequence LmdbStorage)
createSequenceStore e seqName = Sequence seqName . SMap (LmdbStorage e) <$> createLmdbStore e    


mkLmdb :: FilePath -> Int64 -> Int -> IO LmdbEnv
mkLmdb fileName maxSizeMb maxReaders = do 
    nativeEnv <- initializeReadWriteEnvironment (fromIntegral mapSize) maxReaders maxDatabases fileName    
    LmdbEnv <$> 
        newTVarIO (RWEnv nativeEnv) <*>
        createSemaphore maxReaders
    where
        mapSize = maxSizeMb * 1024 * 1024
        maxDatabases = 120

closeLmdb :: Environment e -> IO ()
closeLmdb = closeEnvironment


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
        pure seqMap

