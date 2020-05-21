{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module RPKI.Store.Stores where

import Control.Monad.IO.Class

import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))
import           RPKI.TAL

import qualified RPKI.Store.Base.Map      as M
import qualified RPKI.Store.Base.MultiMap as MM
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage

import           RPKI.Store.Data
import           RPKI.Store.Repository


data RpkiObjectStore s = RpkiObjectStore {
    objects  :: SMap "objects" s Hash SValue,
    byAKI    :: SMultiMap "byAKI" s AKI Hash,
    mftByAKI :: SMultiMap "mftByAKI" s AKI (Hash, Int)
}

instance Storage s => WithStorage s (RpkiObjectStore s) where
    storage = storage . objects

getByHash :: (MonadIO m, Storage s) => 
            Tx s mode -> RpkiObjectStore s -> Hash -> m (Maybe RpkiObject)
getByHash tx store h = liftIO $ (fromSValue <$>) <$> M.get tx (objects store) h

putObject :: (MonadIO m, Storage s) => 
            Tx s 'RW -> RpkiObjectStore s -> StorableObject RpkiObject -> m ()
putObject tx store (StorableObject ro sv) = liftIO $ do
    let h = getHash ro
    M.put tx (objects store) h sv  
    ifJust (getAKI ro) $ \aki' -> do 
        MM.put tx (byAKI store) aki' h
        case ro of
            MftRO mft -> MM.put tx (mftByAKI store) aki' (h, getMftNumber mft)
            _         -> pure ()


deleteObject :: (MonadIO m, Storage s) => Tx s 'RW -> RpkiObjectStore s -> Hash -> m ()
deleteObject tx store h = liftIO $ do
    ro' <- getByHash tx store h
    ifJust ro' $ \ro -> do 
        M.delete tx (objects store) h
        ifJust (getAKI ro) $ \aki' -> do 
            MM.delete tx (byAKI store) aki' h
            case ro of
                MftRO mft -> MM.delete tx (mftByAKI store) aki' (h, getMftNumber mft)
                _         -> pure ()      

findLatestMftByAKI :: (MonadIO m, Storage s) => 
                    Tx s mode -> RpkiObjectStore s -> AKI -> m (Maybe MftObject)
findLatestMftByAKI tx store aki' = liftIO $ 
    MM.foldS tx (mftByAKI store) aki' f Nothing >>= \case
        Nothing        -> pure Nothing
        Just (hash, _) -> 
            getByHash tx store hash >>= \case
                Just (MftRO mft) -> pure $ Just mft
                _                -> pure Nothing
    where
        f latest _ (hash, mftNum) = 
            pure $ case latest of 
            Nothing -> Just (hash, mftNum)
            Just (_, latestNum) 
                | mftNum > latestNum -> Just (hash, mftNum)
                | otherwise          -> latest

findMftsByAKI :: (MonadIO m, Storage s) => 
                Tx s mode -> RpkiObjectStore s -> AKI -> m [MftObject]
findMftsByAKI tx store aki' = liftIO $ 
    MM.foldS tx (mftByAKI store) aki' f []
    where
        f mfts _ (h, _) = accumulate <$> getByHash tx store h
            where 
                accumulate = \case          
                    Just (MftRO mft) -> mft : mfts
                    _ -> mfts            

-- This is for testing purposes mostly
getAll :: (MonadIO m, Storage s) => Tx s mode -> RpkiObjectStore s -> m [RpkiObject]
getAll tx store = liftIO $ reverse <$> M.fold tx (objects store) f []
    where
        f ros _ v = pure $! fromSValue v : ros


-- | TA Store 
newtype TAStore s = TAStore (SMap "trust-anchors" s TaName STA)

instance Storage s => WithStorage s (TAStore s) where
    storage (TAStore s) = storage s


putTA :: (MonadIO m, Storage s) => Tx s 'RW -> TAStore s -> STA -> m ()
putTA tx (TAStore s) ta = liftIO $ M.put tx s (getTaName $ tal ta) ta

getTA :: (MonadIO m, Storage s) => Tx s mode -> TAStore s -> TaName -> m (Maybe STA)
getTA tx (TAStore s) name = liftIO $ M.get tx s name

ifJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
ifJust a f = maybe (pure ()) f a


-- | Validation result store
newtype VResultStore s = VResultStore {
    results  :: SMap "results" s VContext VResult
}

instance Storage s => WithStorage s (VResultStore s) where
    storage (VResultStore s) = storage s

putVResult :: (MonadIO m, Storage s) => 
            Tx s 'RW -> VResultStore s -> VResult -> m ()
putVResult tx (VResultStore s) vr = liftIO $ M.put tx s (path vr) vr

allVResults :: (MonadIO m, Storage s) => 
                Tx s mode -> VResultStore s -> m [(VContext, VResult)]
allVResults tx (VResultStore s) = liftIO $ reverse <$> M.fold tx s f []
    where
        f ros vc vr = pure $! (vc, vr) : ros

data DB s = DB {
    taStore :: TAStore s, 
    repositoryStore :: RepositoryStore s, 
    objectStore :: RpkiObjectStore s,
    resultStore :: VResultStore s
}

instance Storage s => WithStorage s (DB s) where
    storage DB {..} = storage taStore
