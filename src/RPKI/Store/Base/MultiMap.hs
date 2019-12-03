{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module RPKI.Store.Base.MultiMap where

import           Codec.Serialise

import           RPKI.Store.Base.Storage
import           RPKI.Store.Base.Storable
import qualified RPKI.Store.Base.Storage as S


-- TODO Implement it!
data SMultiMap s k v where
    SMMap :: Storage s => s -> SMultiMap s k v

instance Storage s => WithStorage s (SMultiMap s k v) where
    storage (SMMap s) = s
 
put :: (Serialise k, Serialise v, Storage s) =>
        Tx s 'RW -> SMultiMap s k v -> k -> v -> IO ()
put tx (SMMap s) k v = S.put tx s (storableKey k) (storableValue v)    

get :: (Serialise k, Serialise v, Storage s) =>
        Tx s m -> SMultiMap s k v -> k -> IO [v]
get tx (SMMap s) k = do
    msv <- S.get tx s (storableKey k)
    pure []
    -- pure $ fromStorable . (\(SValue z) -> z) <$> msv

delete :: (Serialise k, Serialise v, Storage s) =>
         Tx s 'RW -> SMultiMap s k v -> k -> IO ()
delete tx (SMMap s) k = S.delete tx s (storableKey k)

-- This is wrong
deleteAll :: (Serialise k, Serialise v, Storage s) =>
         Tx s 'RW -> SMultiMap s k v -> k -> IO ()
deleteAll tx (SMMap s) k = S.delete tx s (storableKey k)

