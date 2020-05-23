module RPKI.Store.Util where

import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))

import           Lmdb.Connection
import           Lmdb.Types
import           RPKI.Store.Base.LMDB

import           RPKI.Store.Stores
import           RPKI.Store.Repository
import           RPKI.Store.Sequence

createObjectStore :: LmdbEnv -> IO (RpkiObjectStore LmdbStorage)
createObjectStore e = do
    let lmdb = LmdbStorage e
    objMap <- create e
    akiIndex <- createMulti e
    mftAkiIndex <- createMulti e

    return $ RpkiObjectStore {
        objects = SMap lmdb objMap,
        byAKI = SMultiMap lmdb akiIndex,
        mftByAKI = SMultiMap lmdb mftAkiIndex
    }

createRepositoryStore :: LmdbEnv -> IO (RepositoryStore LmdbStorage)
createRepositoryStore e = do
    let lmdb = LmdbStorage e
    rMap1 <- create e
    rMap2 <- create e
    perTaMap <- createMulti e
    pure $ RepositoryStore {
        rrdpS = SMap lmdb rMap1,
        rsyncS = SMap lmdb rMap2,
        perTA = SMultiMap lmdb perTaMap
    }

createResultStore :: LmdbEnv -> IO (VResultStore LmdbStorage)
createResultStore e = do
    let lmdb = LmdbStorage e
    rMap <- createMulti e    
    pure $ VResultStore {
        results = SMultiMap lmdb rMap
    }

createTAStore :: LmdbEnv -> IO (TAStore LmdbStorage)
createTAStore e = do
    let lmdb = LmdbStorage e
    rMap <- create e
    pure $ TAStore (SMap lmdb rMap)

createSequenceStore :: LmdbEnv -> IO (SequenceStore LmdbStorage)
createSequenceStore e = do
    let lmdb = LmdbStorage e
    rMap <- create e
    pure $ SequenceStore {
        sequences = SMap lmdb rMap
    }

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
    createResultStore e