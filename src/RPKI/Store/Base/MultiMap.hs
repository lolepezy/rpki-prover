{-# LANGUAGE FlexibleInstances #-}

module RPKI.Store.Base.MultiMap where

import Codec.Serialise

import GHC.TypeLits

import RPKI.Store.Base.Storable
import RPKI.Store.Base.Storage  as S


data SMultiMap (name :: Symbol) s k v where
    SMultiMap :: Storage s => s -> SMultiMapImpl s name -> SMultiMap name s k v

instance Storage s => WithStorage s (SMultiMap name s k v) where
    storage (SMultiMap s _) = s

put :: (Serialise k, Serialise v) =>
        Tx s 'RW -> SMultiMap name s k v -> k -> v -> IO ()
put tx (SMultiMap _ s) k v = S.putMu tx s (storableKey k) (storableValue v)    

delete :: (Serialise k, Serialise v) =>
            Tx s 'RW -> SMultiMap name s k v -> k -> v -> IO ()
delete tx (SMultiMap _ s) k v = S.deleteMu tx s (storableKey k) (storableValue v)

deleteAll :: (Serialise k, Serialise v) =>
            Tx s 'RW -> SMultiMap name s k v -> k -> IO ()
deleteAll tx (SMultiMap _ s) k = S.deleteAllMu tx s (storableKey k)

fold :: (Serialise k, Serialise v) =>
        Tx s m -> SMultiMap name s k v -> (a -> k -> v -> IO a) -> a -> IO a
fold tx (SMultiMap _ s) f a = S.foldMu tx s f' a
    where
        f' z (SKey sk) (SValue sv) = f z (fromStorable sk) (fromStorable sv)

foldS :: (Serialise k, Serialise v) =>
        Tx s m -> SMultiMap name s k v -> k -> (a -> k -> v -> IO a) -> a -> IO a
foldS tx (SMultiMap _ s) k f a = S.foldMuForKey tx s (storableKey k) f' a
  where
    f' z (SKey sk) (SValue sv) = f z (fromStorable sk) (fromStorable sv)

allForKey :: (Serialise k, Serialise v) =>
            Tx s m -> SMultiMap name s k v -> k -> IO [v]
allForKey tx (SMultiMap _ s) k = reverse <$> S.foldMuForKey tx s (storableKey k) f []
  where
    f z _ (SValue sv) = pure $! let !q = fromStorable sv in q : z

all :: (Serialise k, Serialise v) =>
        Tx s m -> SMultiMap name s k v -> IO [(k, v)]
all tx (SMultiMap _ s) = reverse <$> S.foldMu tx s f []
  where
    f z (SKey sk) (SValue sv) = pure $! (fromStorable sk, fromStorable sv) : z

stats :: (Serialise k, Serialise v) =>
        Tx s m -> SMultiMap name s k v -> IO SStats
stats tx (SMultiMap _ s) = S.foldMu tx s f (SStats 0 0 0 0)
  where
    f stat skey svalue = pure $! incrementStats stat skey svalue

erase :: (Serialise k, Serialise v) =>
        Tx s 'RW -> SMultiMap name s k v -> IO ()
erase tx (SMultiMap _ s) = S.clearMu tx s
