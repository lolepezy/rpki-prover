{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module RPKI.Store.Base.Storage where
import RPKI.Store.Base.Storable

data TxMode = RO | RW

class WithTx s where
    data Tx s (m :: TxMode)
    readOnlyTx :: s -> (Tx s 'RO -> IO a) -> IO a
    readWriteTx :: s -> (Tx s 'RW -> IO a) -> IO a

class WithTx s => Storage s where        
    get :: Tx s m -> s -> SKey -> IO (Maybe SValue)    
    iterateOver :: Tx s m -> s -> (SKey -> SValue -> IO r) -> IO [r]
    put :: Tx s 'RW -> s -> SKey -> SValue -> IO ()
    delete :: Tx s 'RW -> s -> SKey -> IO ()

class Storage s => WithStorage s ws where
    storage :: ws -> s

roTx :: WithStorage s ws => ws -> (Tx s 'RO -> IO a) -> IO a
roTx s = readOnlyTx (storage s)

rwTx :: WithStorage s ws => ws -> (Tx s 'RW -> IO a) -> IO a
rwTx s = readWriteTx (storage s)