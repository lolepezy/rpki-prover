{-# LANGUAGE MultiParamTypeClasses #-}

module RPKI.Store.Base.Storage where
import RPKI.Store.Base.Storable

data TxMode = RO | RW

class WithTx s where
    data Tx s (m :: TxMode)
    roTx :: s -> (Tx s 'RO -> IO a) -> IO a
    rwTx :: s -> (Tx s 'RW -> IO a) -> IO a

class WithTx s => Storage s where        
    get :: Tx s m -> s -> SKey -> IO (Maybe SValue)    
    put :: Tx s 'RW -> s -> SKey -> SValue -> IO ()
    delete :: Tx s 'RW -> s -> SKey -> IO ()

class Storage s => WithStorage s ws where
    storage :: ws -> s
