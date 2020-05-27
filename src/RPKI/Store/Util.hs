{-# LANGUAGE OverloadedStrings #-}
module RPKI.Store.Util where


import Data.Text (Text)

import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))

import           Lmdb.Connection
import           Lmdb.Types
import           RPKI.Store.Base.LMDB

import           RPKI.Store.Stores
import           RPKI.Store.Repository
import           RPKI.Store.Sequence


createObjectStore :: LmdbEnv -> IO (RpkiObjectStore LmdbStorage)
createObjectStore e =
    RpkiObjectStore <$>
        (SMap lmdb <$> create e) <*>
        (SMultiMap lmdb <$> createMulti e) <*>
        (SMultiMap lmdb <$> createMulti e)
    where 
        lmdb = LmdbStorage e        

createRepositoryStore :: LmdbEnv -> IO (RepositoryStore LmdbStorage)
createRepositoryStore e = 
    RepositoryStore <$>
        (SMap lmdb <$> create e) <*>
        (SMap lmdb <$> create e) <*>
        (SMultiMap lmdb <$> createMulti e)
    where 
        lmdb = LmdbStorage e

createResultStore :: LmdbEnv -> IO (VResultStore LmdbStorage)
createResultStore e = 
    VResultStore <$> 
        createSequenceStore e "vresult-key" <*>
        (SMap lmdb <$> create e) <*>
        (SMultiMap lmdb <$> createMulti e)
    where 
        lmdb = LmdbStorage e

createVRPStore :: LmdbEnv -> IO (VRPStore LmdbStorage)
createVRPStore e = VRPStore . SMultiMap (LmdbStorage e) <$> createMulti e    

createTAStore :: LmdbEnv -> IO (TAStore LmdbStorage)
createTAStore e = TAStore . SMap (LmdbStorage e) <$> create e    

createSequenceStore :: LmdbEnv -> Text -> IO (Sequence LmdbStorage)
createSequenceStore e seqName = Sequence seqName . SMap (LmdbStorage e) <$> create e    

mkLmdb :: FilePath -> Int -> IO LmdbEnv
mkLmdb fileName maxReaders = 
    LmdbEnv <$> 
        initializeReadWriteEnvironment mapSize maxReaders maxDatabases fileName <*>
        createSemaphore maxReaders
    where
        -- TODO Make it configurable
        mapSize = 64*1024*1024*1024
        maxDatabases = 120

closeLmdb :: Environment e -> IO ()
closeLmdb = closeEnvironment


createDatabase :: LmdbEnv -> IO (DB LmdbStorage)
createDatabase e = DB <$>
    createTAStore e <*>
    createRepositoryStore e <*>
    createObjectStore e <*>
    createResultStore e <*>
    createVRPStore e