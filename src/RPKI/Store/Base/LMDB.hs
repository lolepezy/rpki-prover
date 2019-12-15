{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}

module RPKI.Store.Base.LMDB where

import qualified Data.ByteString as B
import Data.Coerce (coerce)

import RPKI.Store.Base.Storable
import RPKI.Store.Base.Storage
import RPKI.Store.Base.MultiStorage

import Lmdb.Connection
import Lmdb.Codec (byteString)
import qualified Lmdb.Map as LMap
import qualified Lmdb.Multimap as LMMap
import qualified Lmdb.Types as Lmdb

import Pipes

data LmdbStore = LmdbStore { 
    env :: Lmdb.Environment 'Lmdb.ReadWrite,
    db :: Lmdb.Database B.ByteString B.ByteString
}

data LmdbMultiStore = LmdbMultiStore { 
    env :: Lmdb.Environment 'Lmdb.ReadWrite,
    db :: Lmdb.MultiDatabase B.ByteString B.ByteString
}

type family LmdbTxMode (m :: TxMode) :: Lmdb.Mode where
    LmdbTxMode 'RO = 'Lmdb.ReadOnly
    LmdbTxMode 'RW = 'Lmdb.ReadWrite

toROTx :: Lmdb.Transaction (m :: Lmdb.Mode) -> Lmdb.Transaction 'Lmdb.ReadOnly
toROTx = coerce

class WithLmdb lmdb where
    getEnv :: lmdb -> Lmdb.Environment 'Lmdb.ReadWrite
    
instance WithLmdb lmdb => WithTx lmdb where    
    data Tx lmdb (m :: TxMode) = LmdbTx (Lmdb.Transaction (LmdbTxMode m))
    readOnlyTx lmdb f = withTransaction (getEnv lmdb) (f . LmdbTx . readonly)
    readWriteTx lmdb f = withTransaction (getEnv lmdb) (f . LmdbTx)

instance WithLmdb LmdbStore where
    getEnv (LmdbStore {..}) = env

instance WithLmdb LmdbMultiStore where
    getEnv (LmdbMultiStore {..}) = env


-- | Basic storage implemented using LMDB
instance Storage LmdbStore where    
    put (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) (SValue (Storable bs)) = 
        LMap.insert' tx db ks bs

    delete (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) = 
        LMap.delete' tx db ks

    get (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) =
        (SValue . Storable <$>) <$> LMap.lookup' (toROTx tx) db ks 

    iterateOver (LmdbTx tx) LmdbStore {..} f =
        void $ withCursor tx db $ \c -> 
            runEffect $ LMap.firstForward c >-> do
                Lmdb.KeyValue k v <- await
                lift $ f (SKey $ Storable k) (SValue $ Storable v)


-- | Basic storage implemented using LMDB
instance MultiStorage LmdbMultiStore where    
    put (LmdbTx tx) LmdbMultiStore {..} (SKey (Storable ks)) (SValue (Storable bs)) = 
        pure ()
        -- LMMap.insert' tx db ks bs

    delete (LmdbTx tx) LmdbMultiStore {..} (SKey (Storable ks)) (SValue (Storable vs)) =         
        pure ()
        -- LMMap.delete' tx db ks

    deleteAll (LmdbTx tx) LmdbMultiStore {..} (SKey (Storable ks)) = 
        -- LMMap.delete' tx db ks        
        pure ()    

    iterateOver (LmdbTx tx) LmdbMultiStore {..} key@(SKey (Storable ks)) f =
        void $ withMultiCursor tx db $ \c -> 
            runEffect $ LMMap.lookupValues c ks >-> do
                v <- await
                lift $ f key (SValue $ Storable v)



create :: Lmdb.Environment 'Lmdb.ReadWrite -> String -> IO LmdbStore
create env name = do
    db <- withTransaction env $ \tx -> openDatabase tx (Just name) dbSettings 
    pure $ LmdbStore env db
    where
        dbSettings = makeSettings 
            (Lmdb.SortNative Lmdb.NativeSortLexographic) 
            byteString byteString

createMulti :: Lmdb.Environment 'Lmdb.ReadWrite -> String -> IO LmdbMultiStore
createMulti env name = do
    db <- withTransaction env $ \tx -> openMultiDatabase tx (Just name) dbSettings 
    pure $ LmdbMultiStore env db
    where
        dbSettings :: Lmdb.MultiDatabaseSettings B.ByteString B.ByteString
        dbSettings = makeMultiSettings 
            (Lmdb.SortNative Lmdb.NativeSortLexographic) 
            (Lmdb.SortNative Lmdb.NativeSortLexographic) 
            byteString byteString
    
    