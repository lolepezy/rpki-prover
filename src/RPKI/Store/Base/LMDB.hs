{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}

module RPKI.Store.Base.LMDB where

import qualified Data.ByteString as B
import Data.Coerce (coerce)

import RPKI.Store.Base.Storage

import Lmdb.Connection
import Lmdb.Codec (byteString)
import qualified Lmdb.Map as LmdbMap
import qualified Lmdb.Types as Lmdb


data LmdbStore = LmdbStore { 
    env :: Lmdb.Environment 'Lmdb.ReadWrite,
    db :: Lmdb.Database B.ByteString B.ByteString
}

type family LmdbTxMode (m :: TxMode) :: Lmdb.Mode where
    LmdbTxMode 'RO = 'Lmdb.ReadOnly
    LmdbTxMode 'RW = 'Lmdb.ReadWrite

toROTx :: Lmdb.Transaction (m :: Lmdb.Mode) -> Lmdb.Transaction 'Lmdb.ReadOnly
toROTx = coerce

-- | Basic storage implemented using LMDB
instance Storage LmdbStore where    
    data Tx LmdbStore (m :: TxMode) = LmdbTx (Lmdb.Transaction (LmdbTxMode m))
    roTx LmdbStore {..} f = withTransaction env (f . LmdbTx . readonly)
    rwTx LmdbStore {..} f = withTransaction env (f . LmdbTx)

    put (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) (SValue (Storable bs)) = 
        LmdbMap.insert' tx db ks bs

    delete (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) = 
        LmdbMap.delete' tx db ks

    get (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) =
        (SValue . Storable <$>) <$> LmdbMap.lookup' (toROTx tx) db ks 


create :: Lmdb.Environment 'Lmdb.ReadWrite -> String -> IO LmdbStore
create env name = do
    db <- withTransaction env $ \tx -> openDatabase tx (Just name) dbSettings 
    pure $ LmdbStore env db
    where
        dbSettings = makeSettings (Lmdb.SortNative Lmdb.NativeSortLexographic) byteString byteString              