{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module RPKI.Store.Base.Map where

import           Codec.Serialise

import           RPKI.Store.Base.Storage
import           RPKI.Store.Base.Storable
import qualified RPKI.Store.Base.Storage as S

type Index s v = (s, v -> [SKey])

-- TODO Add proper support for indexes
data SMap s k v where
    SIxMap :: Storage s => s -> [Index s v] -> SMap s k v

instance Storage s => WithStorage s (SMap s k v) where
    storage (SIxMap s _) = s
 
put :: (Serialise k, Serialise v, Storage s) =>
        Tx s 'RW -> SMap s k v -> k -> v -> IO ()
put tx (SIxMap s _) k v = S.put tx s (storableKey k) (storableValue v)    

get :: (Serialise k, Serialise v, Storage s) =>
        Tx s m -> SMap s k v -> k -> IO (Maybe v)
get tx (SIxMap s _) k = do
    msv <- S.get tx s (storableKey k)
    pure $ fromStorable . (\(SValue z) -> z) <$> msv

delete :: (Serialise k, Serialise v, Storage s) =>
         Tx s 'RW -> SMap s k v -> k -> IO ()
delete tx (SIxMap s _) k = S.delete tx s (storableKey k)

