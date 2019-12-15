{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingVia #-}

module RPKI.Store.Base.MultiStorage where

import RPKI.Store.Base.Storage
import RPKI.Store.Base.Storable

class WithTx s => MultiStorage s where        
    get :: Tx s m -> s -> SKey -> IO [v]
    put :: Tx s 'RW -> s -> SKey -> SValue -> IO ()
    iterateOver :: Tx s m -> s -> SKey -> (SKey -> SValue -> IO r) -> IO [r]
    delete :: Tx s 'RW -> s -> SKey -> SValue -> IO ()
    deleteAll :: Tx s 'RW -> s -> SKey -> IO ()

class MultiStorage s => WithStorage s ws where
    storage :: ws -> s
