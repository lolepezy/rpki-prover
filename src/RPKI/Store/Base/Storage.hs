{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module RPKI.Store.Base.Storage where

import GHC.TypeLits
import RPKI.Store.Base.Storable

data TxMode = RO | RW

class WithTx s where
    data Tx s (m :: TxMode)
    readOnlyTx :: s -> (Tx s 'RO -> IO a) -> IO a
    readWriteTx :: s -> (Tx s 'RW -> IO a) -> IO a

class WithTx s => Storage s where        
    type SMap' s :: Symbol -> *
    type MSMap' s :: Symbol -> *
    get :: Tx s m -> SMap' s name -> SKey -> IO (Maybe SValue)    
    fold :: Tx s m -> SMap' s name -> (a -> SKey -> SValue -> IO a) -> a -> IO a
    put :: Tx s 'RW -> SMap' s name -> SKey -> SValue -> IO ()
    delete :: Tx s 'RW -> SMap' s name -> SKey -> IO ()

    putMu :: Tx s 'RW -> MSMap' s name -> SKey -> SValue -> IO ()
    foldMu :: Tx s m -> MSMap' s name -> SKey -> (a -> SKey -> SValue -> IO a) -> a -> IO a
    deleteMu :: Tx s 'RW -> MSMap' s name -> SKey -> SValue -> IO ()
    deleteAllMu :: Tx s 'RW -> MSMap' s name -> SKey -> IO ()

class Storage s => WithStorage s ws where
    storage :: ws -> s

roTx :: WithStorage s ws => ws -> (Tx s 'RO -> IO a) -> IO a
roTx s = readOnlyTx (storage s)

rwTx :: WithStorage s ws => ws -> (Tx s 'RW -> IO a) -> IO a
rwTx s = readWriteTx (storage s)
