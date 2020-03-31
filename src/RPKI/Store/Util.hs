module RPKI.Store.Util where

import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))

import           Lmdb.Connection
import           Lmdb.Types
import           RPKI.Store.Base.LMDB

import           RPKI.Store.Stores
import           RPKI.Store.Sequence

createObjectStore :: Env -> IO (RpkiObjectStore LmdbStorage)
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

createRepositoryStore :: Env -> IO (RepositoryStore LmdbStorage)
createRepositoryStore e = do
    let lmdb = LmdbStorage e
    rMap <- create e
    perTaMap <- createMulti e
    pure $ RepositoryStore {
        repositories = SMap lmdb rMap,
        repositoriesPerTA = SMultiMap lmdb perTaMap
    }

createResultStore :: Env -> IO (VResultStore LmdbStorage)
createResultStore e = do
    let lmdb = LmdbStorage e
    rMap <- create e    
    pure $ VResultStore {
        results = SMap lmdb rMap
    }

createTAStore :: Env -> IO (TAStore LmdbStorage)
createTAStore e = do
    let lmdb = LmdbStorage e
    rMap <- create e
    pure $ TAStore (SMap lmdb rMap)

createSequenceStore :: Env -> IO (SequenceStore LmdbStorage)
createSequenceStore e = do
    let lmdb = LmdbStorage e
    rMap <- create e
    pure $ SequenceStore {
        sequences = SMap lmdb rMap
    }

mkLmdb :: FilePath -> IO Env
mkLmdb fileName = initializeReadWriteEnvironment mapSize readerNum maxDatabases fileName
    where
        -- TODO Make it configurable
        mapSize = 8*1024*1024*1024
        readerNum = 120
        maxDatabases = 120

closeLmdb :: Environment e -> IO ()
closeLmdb = closeEnvironment


createDatabase :: Env -> IO (DB LmdbStorage)
createDatabase e = DB <$>
    createTAStore e <*>
    createRepositoryStore e <*>
    createObjectStore e <*>
    createResultStore e