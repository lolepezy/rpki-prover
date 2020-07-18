{-# LANGUAGE OverloadedStrings #-}
module RPKI.Store.Util where


import Data.Text (Text)

import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))

import           Lmdb.Connection
import           Lmdb.Types
import           RPKI.Store.Base.LMDB

import           RPKI.Store.Database
import           RPKI.Store.Repository
import           RPKI.Store.Sequence


createObjectStore :: LmdbEnv -> IO (RpkiObjectStore LmdbStorage)
createObjectStore e =
    RpkiObjectStore <$>
        createSequenceStore e "object-key" <*>
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
        (SMultiMap lmdb <$> createLmdbMultiStore e)
    where 
        lmdb = LmdbStorage e

createResultStore :: LmdbEnv -> IO (VResultStore LmdbStorage)
createResultStore e = 
    VResultStore <$> 
        createSequenceStore e "vresult-key" <*>
        (SMap lmdb <$> createLmdbStore e) <*>
        (SMultiMap lmdb <$> createLmdbMultiStore e)
    where 
        lmdb = LmdbStorage e

createVRPStore :: LmdbEnv -> IO (VRPStore LmdbStorage)
createVRPStore e = VRPStore . SMultiMap (LmdbStorage e) <$> createLmdbMultiStore e    

createTAStore :: LmdbEnv -> IO (TAStore LmdbStorage)
createTAStore e = TAStore . SMap (LmdbStorage e) <$> createLmdbStore e    

createVersionStore :: LmdbEnv -> IO (VersionStore LmdbStorage)
createVersionStore e = VersionStore . SMap (LmdbStorage e) <$> createLmdbStore e    

createSequenceStore :: LmdbEnv -> Text -> IO (Sequence LmdbStorage)
createSequenceStore e seqName = Sequence seqName . SMap (LmdbStorage e) <$> createLmdbStore e    


mkLmdb :: FilePath -> Int -> Int -> IO LmdbEnv
mkLmdb fileName maxSizeMb maxReaders = 
    LmdbEnv <$> 
        initializeReadWriteEnvironment mapSize maxReaders maxDatabases fileName <*>
        createSemaphore maxReaders
    where
        -- TODO Make it configurable?
        mapSize = maxSizeMb * 1024 * 1024
        maxDatabases = 120

closeLmdb :: Environment e -> IO ()
closeLmdb = closeEnvironment


createDatabase :: LmdbEnv -> IO (DB LmdbStorage)
createDatabase e = DB <$>
    createTAStore e <*>
    createRepositoryStore e <*>
    createObjectStore e <*>
    createResultStore e <*>
    createVRPStore e <*>
    createVersionStore e