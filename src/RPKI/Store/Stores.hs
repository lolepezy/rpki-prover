{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveAnyClass       #-}

module RPKI.Store.Stores where

import Codec.Serialise

import GHC.Generics

import RPKI.Domain
import RPKI.TAL
import RPKI.Store.Base.Map (SMap(..))
import RPKI.Store.Base.MultiMap (SMultiMap(..))

import qualified RPKI.Store.Base.Map as M
import qualified RPKI.Store.Base.MultiMap as MM
import RPKI.Store.Base.Storage
import RPKI.Store.Base.Storable


data RpkiObjectStore s = RpkiObjectStore {
  objects  :: SMap "objects" s Hash SValue,
  byAKI    :: SMultiMap "byAKI" s AKI Hash,
  mftByAKI :: SMultiMap "mftByAKI" s AKI Hash
}

instance Storage s => WithStorage s (RpkiObjectStore s) where
  storage = storage . objects


getByHash :: Storage s => Tx s m -> RpkiObjectStore s -> Hash -> IO (Maybe RpkiObject)
getByHash tx store h = (fromSValue <$>) <$> M.get tx (objects store) h

putObject :: Storage s => Tx s 'RW -> RpkiObjectStore s -> StorableObject RpkiObject -> IO ()
putObject tx store (StorableObject ro sv) = do
  let h = getHash ro
  M.put tx (objects store) h sv  
  ifJust (getAKI ro) $ \aki' -> do 
    MM.put tx (byAKI store) aki' h
    case ro of
      MftRO _ -> MM.put tx (mftByAKI store) aki' h
      _       -> pure ()


deleteObject :: Storage s => Tx s 'RW -> RpkiObjectStore s -> Hash -> IO ()
deleteObject tx store h = do
  ro' <- getByHash tx store h
  ifJust ro' $ \ro -> do 
    M.delete tx (objects store) h
    ifJust (getAKI ro) $ \aki' -> do 
      MM.delete tx (byAKI store) aki' h
      case ro of
        MftRO _ -> MM.delete tx (mftByAKI store) aki' h
        _       -> pure ()      
findLatestMftByAKI :: Storage s => Tx s m -> RpkiObjectStore s -> AKI -> IO (Maybe MftObject)
findLatestMftByAKI tx store aki' = 
    MM.fold tx (mftByAKI store) aki' f Nothing
    where
      f latest _ h = latestMft <$> getByHash tx store h
        where
          latestMft = \case
            Just (MftRO mft) -> case latest of
              Nothing -> Just mft
              Just lat | getMftNumber mft > getMftNumber lat -> Just mft
              Just lat | otherwise                           -> Just lat
            _ -> Nothing

findMftsByAKI :: Storage s => Tx s m -> RpkiObjectStore s -> AKI -> IO [MftObject]
findMftsByAKI tx store aki' = 
    MM.fold tx (mftByAKI store) aki' f []
    where
      f mfts _ h = accumulate <$> getByHash tx store h
        where 
          accumulate = \case          
            Just (MftRO mft) -> mft : mfts
            _ -> mfts            

-- This is for testing purposes mostly
getAll :: Storage s => Tx s m -> RpkiObjectStore s -> IO [RpkiObject]
getAll tx store = reverse <$> M.fold tx (objects store) f []
  where
    f ros _ v = pure $ fromSValue v : ros

newtype TAStore s = TAStore (SMap "trust-anchors" s TaName StoredTA)

instance Storage s => WithStorage s (TAStore s) where
  storage (TAStore s) = storage s

data StoredTA = StoredTA {
  tal        :: TAL,
  taCert     :: CerObject
} deriving (Show, Eq, Generic, Serialise)

putTA :: Storage s => Tx s 'RW -> TAStore s -> StoredTA -> IO ()
putTA tx (TAStore s) ta = M.put tx s (getTaName $ tal ta) ta

getTA :: Storage s => Tx s m -> TAStore s -> TaName -> IO (Maybe StoredTA)
getTA tx (TAStore s) name = M.get tx s name

ifJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
ifJust a f = maybe (pure ()) f a