{-# LANGUAGE DataKinds #-}
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

instance WithTx LmdbStore where    
    data Tx LmdbStore (m :: TxMode) = LmdbTx (Lmdb.Transaction (LmdbTxMode m))
    roTx LmdbStore {..} f = withTransaction env (f . LmdbTx . readonly)
    rwTx LmdbStore {..} f = withTransaction env (f . LmdbTx)

-- | Basic storage implemented using LMDB
instance Storage LmdbStore where    
    put (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) (SValue (Storable bs)) = 
        LMap.insert' tx db ks bs

    delete (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) = 
        LMap.delete' tx db ks

    get (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) =
        (SValue . Storable <$>) <$> LMap.lookup' (toROTx tx) db ks 


-- | Basic storage implemented using LMDB
instance MultiStorage LmdbStore where    
    put (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) (SValue (Storable bs)) = 
        pure ()
        -- LMMap.insert' tx db ks bs

    delete (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) =         
        pure ()
        -- LMMap.delete' tx db ks

    deleteAll (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) (SValue (Storable vs)) = 
        LMap.delete' tx db ks        

    -- TODO 
    get (LmdbTx tx) LmdbStore {..} (SKey (Storable ks)) =
        -- (SValue . Storable <$>) <$> LMMap.lookup' (toROTx tx) db ks 
        pure []


create :: Lmdb.Environment 'Lmdb.ReadWrite -> String -> IO LmdbStore
create env name = do
    db <- withTransaction env $ \tx -> openDatabase tx (Just name) dbSettings 
    pure $ LmdbStore env db

-- createMulti :: Lmdb.Environment 'Lmdb.ReadWrite -> String -> IO LmdbMultiStore
-- createMulti env name = do
--     db <- withTransaction env $ \tx -> openMultiDatabase tx (Just name) dbSettings 
--     pure $ LmdbMultiStore $ env db
    
        
dbSettings = makeSettings (Lmdb.SortNative Lmdb.NativeSortLexographic) byteString byteString