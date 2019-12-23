{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RPKI.Store.Base.LMDB where

import qualified Data.ByteString as B
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
type DB = Lmdb.Database B.ByteString B.ByteString


data LmdbStore (name :: Symbol) = LmdbStore { 
    env :: Env,
    db  :: DB
}

data LmdbMultiStore (name :: Symbol) = LmdbMultiStore { 
    env :: Env,
    db :: Lmdb.MultiDatabase B.ByteString B.ByteString
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
    readOnlyTx lmdb f = withTransaction (getEnv lmdb) (f . LmdbTx . readonly)
    readWriteTx lmdb f = withTransaction (getEnv lmdb) (f . LmdbTx)

-- | Basic storage implemented using LMDB
instance Storage LmdbStorage where    
    type SMapImpl LmdbStorage = LmdbStore
    type SMultiMapImpl LmdbStorage = LmdbMultiStore

    put (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) (SValue (Storable bs)) = 
        LMap.insert' tx db ks bs

    delete (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) = 
        LMap.delete' tx db ks

    get (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) =
        (SValue . Storable <$>) <$> LMap.lookup' (toROTx tx) db ks 

    fold (LmdbTx tx) LmdbStore {..} f a0 =
        withCursor tx db $ \c -> do
            z <- newIORef a0
            runEffect $ LMap.firstForward c >-> do
                Lmdb.KeyValue k v <- await
                lift $ do
                    a <- readIORef z
                    a' <- f a (SKey $ Storable k) (SValue $ Storable v)
                    writeIORef z $! a'
            readIORef z

    putMu (LmdbTx tx) LmdbMultiStore {..} (SKey (Storable ks)) (SValue (Storable bs)) = 
        pure ()
        -- LMMap.insert' tx db ks bs

    deleteMu (LmdbTx tx) LmdbMultiStore {..} (SKey (Storable ks)) (SValue (Storable vs)) =         
        pure ()
        -- LMMap.delete' tx db ks

    deleteAllMu (LmdbTx tx) LmdbMultiStore {..} (SKey (Storable ks)) = 
        -- LMMap.delete' tx db ks        
        pure ()    

    foldMu (LmdbTx tx) LmdbMultiStore {..} key@(SKey (Storable ks)) f a0 =
        withMultiCursor tx db $ \c -> do
            z <- newIORef a0
            runEffect $ LMMap.lookupValues c ks >-> do
                v <- await
                lift $ do 
                    a <- readIORef z
                    a' <- f a key (SValue $ Storable v)
                    writeIORef z $! a'
            readIORef z


create :: forall name . KnownSymbol name => 
            Env -> IO (LmdbStore name)
create env = do
    let name' = symbolVal (P.Proxy :: P.Proxy name)
    db <- withTransaction env $ \tx -> openDatabase tx (Just name') dbSettings 
    pure $ LmdbStore env db
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
        dbSettings :: Lmdb.MultiDatabaseSettings B.ByteString B.ByteString
        dbSettings = makeMultiSettings 
            (Lmdb.SortNative Lmdb.NativeSortLexographic) 
            (Lmdb.SortNative Lmdb.NativeSortLexographic) 
            byteString byteString
    
    