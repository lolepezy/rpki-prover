{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RPKI.Store.Base.LMDB where

import Control.Monad (forever)
import Control.Concurrent.STM
import Control.Exception

import qualified Data.ByteString as BS
import Data.Coerce (coerce)

import RPKI.Store.Base.Storable
import RPKI.Store.Base.Storage

import Data.IORef

import GHC.TypeLits
import Data.Proxy as P

import Lmdb.Connection
import Lmdb.Codec (byteString)
import qualified Lmdb.Map as LMap
import qualified Lmdb.Multimap as LMMap
import qualified Lmdb.Types as Lmdb

import Pipes

type Env = Lmdb.Environment 'Lmdb.ReadWrite
type DBMap = Lmdb.Database BS.ByteString BS.ByteString


data LmdbStore (name :: Symbol) = LmdbStore { 
    env :: Env,
    db  :: DBMap,
    txSem :: Semaphore
}

data LmdbMultiStore (name :: Symbol) = LmdbMultiStore { 
    env :: Env,
    db :: Lmdb.MultiDatabase BS.ByteString BS.ByteString
}

type family LmdbTxMode (m :: TxMode) :: Lmdb.Mode where
    LmdbTxMode 'RO = 'Lmdb.ReadOnly
    LmdbTxMode 'RW = 'Lmdb.ReadWrite

toROTx :: Lmdb.Transaction (m :: Lmdb.Mode) -> Lmdb.Transaction 'Lmdb.ReadOnly
toROTx = coerce

class WithLmdb lmdb where
    getEnv :: lmdb -> Env

newtype LmdbStorage = LmdbStorage { unEnv :: Env }

instance WithLmdb LmdbStorage where
    getEnv = unEnv


instance WithTx LmdbStorage where    
    data Tx LmdbStorage (m :: TxMode) = LmdbTx (Lmdb.Transaction (LmdbTxMode m))

    -- TODO Limit the amount of simultaneous transactions to 126
    readOnlyTx lmdb f = withROTransaction (getEnv lmdb) (f . LmdbTx)
    readWriteTx lmdb f = withTransaction (getEnv lmdb) (f . LmdbTx)

-- | Basic storage implemented using LMDB
instance Storage LmdbStorage where    
    type SMapImpl LmdbStorage = LmdbStore
    type SMultiMapImpl LmdbStorage = LmdbMultiStore

    put (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) (SValue (Storable bs)) = 
        LMap.repsert' tx db ks bs

    delete (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) = 
        LMap.delete' tx db ks

    get (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) =
        (SValue . Storable <$>) <$> LMap.lookup' (toROTx tx) db ks 

    fold (LmdbTx tx) LmdbStore {..} f a0 =
        foldGeneric tx db f a0 withCursor LMap.firstForward        

    putMu (LmdbTx tx) LmdbMultiStore {..} (SKey (Storable ks)) (SValue (Storable bs)) = 
        withMultiCursor tx db $ \c -> LMMap.insert c ks bs

    deleteMu (LmdbTx tx) LmdbMultiStore {..} (SKey (Storable ks)) (SValue (Storable vs)) =   
        LMMap.deleteKV tx db ks vs

    deleteAllMu (LmdbTx tx) LmdbMultiStore {..} (SKey (Storable ks)) = 
        withMultiCursor tx db $ \c -> do
            LMMap.lookupFirstValue c ks >>= \case
                Nothing -> pure ()
                Just _  -> LMMap.deleteValues c

    foldMuForKey (LmdbTx tx) LmdbMultiStore {..} key@(SKey (Storable ks)) f a0 =
        withMultiCursor tx db $ \c -> do
            z <- newIORef a0
            runEffect $ LMMap.lookupValues c ks >-> do
                forever $ do
                    v <- await
                    lift $ do 
                        a <- readIORef z
                        a' <- f a key (SValue $ Storable v)
                        writeIORef z $! a'
            readIORef z

    foldMu (LmdbTx tx) LmdbMultiStore {..} f a0 =
        foldGeneric tx db f a0 withMultiCursor LMMap.firstForward

-- TODO Add some nice type signature here
foldGeneric tx db f a0 withC makeProducer =
    withC tx db $ \c -> do
        z <- newIORef a0
        runEffect $ makeProducer c >-> do
            forever $ do
                Lmdb.KeyValue k v <- await
                lift $ do 
                    a <- readIORef z
                    a' <- f a (SKey $ Storable k) (SValue $ Storable v)
                    writeIORef z $! a'
        readIORef z


create :: forall name . KnownSymbol name => 
            Env -> IO (LmdbStore name)
create env = do
    let name' = symbolVal (P.Proxy :: P.Proxy name)
    db <- withTransaction env $ \tx -> openDatabase tx (Just name') dbSettings     
    -- TODO Bump it up and make it the same for LMDB and the semaphore
    LmdbStore env db <$> createSemaphore 126
    where
        dbSettings = makeSettings 
            (Lmdb.SortNative Lmdb.NativeSortLexographic) 
            byteString byteString

createMulti :: forall name . KnownSymbol name =>  
                Env -> IO (LmdbMultiStore name)
createMulti env = do
    let name' = symbolVal (P.Proxy :: P.Proxy name)
    db <- withTransaction env $ \tx -> openMultiDatabase tx (Just name') dbSettings 
    pure $ LmdbMultiStore env db
    where
        dbSettings :: Lmdb.MultiDatabaseSettings BS.ByteString BS.ByteString
        dbSettings = makeMultiSettings 
            (Lmdb.SortNative Lmdb.NativeSortLexographic) 
            (Lmdb.SortNative Lmdb.NativeSortLexographic) 
            byteString byteString


-- Auxialliry stuff for limiting the amount of parallel LMDB transactions    
data Semaphore = Semaphore Int (TVar Int)

createSemaphore :: Int -> IO Semaphore
createSemaphore n = Semaphore n <$> atomically (newTVar 0)

withSemaphore :: Semaphore -> IO a -> IO a
withSemaphore (Semaphore maxCounter current) f = 
    bracket increment decrement (const f)
    where 
        increment = atomically $ do 
            c <- readTVar current
            if c >= maxCounter 
                then retry
                else writeTVar current (c + 1)
        decrement _ = atomically $ modifyTVar current $ \c -> c - 1