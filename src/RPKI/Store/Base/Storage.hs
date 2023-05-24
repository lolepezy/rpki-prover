{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module RPKI.Store.Base.Storage where

import Data.Kind
import GHC.TypeLits
import RPKI.Store.Base.Storable
import Control.Exception (SomeException)
import RPKI.Reporting
import RPKI.Util (fmtEx)

data TxMode = RO | RW

class WithTx s where
    data Tx s (m :: TxMode)
    readOnlyTx :: s -> (Tx s 'RO -> IO a) -> IO a
    readWriteTx :: s -> (Tx s 'RW -> IO a) -> IO a

class WithTx s => Storage s where
    type SMapImpl s :: Symbol -> Type
    type SMultiMapImpl s :: Symbol -> Type
    get :: Tx s m -> SMapImpl s name -> SKey -> IO (Maybe SValue)    
    foldS :: Tx s m -> SMapImpl s name -> (a -> SKey -> SValue -> IO a) -> a -> IO a
    put :: Tx s 'RW -> SMapImpl s name -> SKey -> SValue -> IO ()
    delete :: Tx s 'RW -> SMapImpl s name -> SKey -> IO ()
    clear :: Tx s 'RW -> SMapImpl s name -> IO ()

    putMu :: Tx s 'RW -> SMultiMapImpl s name -> SKey -> SValue -> IO ()
    foldMuForKey :: Tx s m -> SMultiMapImpl s name -> SKey -> (a -> SKey -> SValue -> IO a) -> a -> IO a
    foldMu :: Tx s m -> SMultiMapImpl s name -> (a -> SKey -> SValue -> IO a) -> a -> IO a
    deleteMu :: Tx s 'RW -> SMultiMapImpl s name -> SKey -> SValue -> IO ()
    deleteAllMu :: Tx s 'RW -> SMultiMapImpl s name -> SKey -> IO ()    
    clearMu :: Tx s 'RW -> SMultiMapImpl s name -> IO ()


class Storage s => WithStorage s ws where
    storage :: ws -> s

instance Storage s => WithStorage s s where
    storage = id

roTx :: WithStorage s ws => ws -> (Tx s 'RO -> IO a) -> IO a
roTx = readOnlyTx . storage

rwTx :: WithStorage s ws => ws -> (Tx s 'RW -> IO a) -> IO a
rwTx = readWriteTx . storage

storageError :: SomeException -> AppError
storageError = StorageE . StorageError . fmtEx    

