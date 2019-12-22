{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module RPKI.Store.Base.Map where

import           Codec.Serialise

import GHC.TypeLits

import           RPKI.Store.Base.Storage as S
import           RPKI.Store.Base.Storable

data SMap (name :: Symbol) s k v where
    SIxMap :: Storage s => s -> SMap' s name -> SMap name s k v

instance Storage s => WithStorage s (SMap name s k v) where
    storage (SIxMap s _) = s
 
put :: (Serialise k, Serialise v) =>
        Tx s 'RW -> SMap name s k v -> k -> v -> IO ()
put tx (SIxMap _ s) k v = S.put tx s (storableKey k) (storableValue v)    

get :: (Serialise k, Serialise v) =>
        Tx s m -> SMap name s k v -> k -> IO (Maybe v)
get tx (SIxMap _ s) k = do
    msv <- S.get tx s (storableKey k)
    pure $ fromStorable . (\(SValue z) -> z) <$> msv

delete :: (Serialise k, Serialise v) =>
         Tx s 'RW -> SMap name s k v -> k -> IO ()
delete tx (SIxMap _ s) k = S.delete tx s (storableKey k)

fold :: (Serialise k, Serialise v) =>
        Tx s m -> SMap name s k v -> (a -> k -> v -> IO a) -> a -> IO a
fold tx (SIxMap _ s) f a = S.fold tx s f' a
    where
        f' z (SKey sk) (SValue sv) = f z (fromStorable sk) (fromStorable sv)