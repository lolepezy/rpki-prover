{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RPKI.Store.Stores where

import           Codec.Serialise

import           GHC.Generics

import qualified Data.List.NonEmpty       as NE

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


data RpkiObjectStore s = RpkiObjectStore {
    objects  :: SMap "objects" s Hash SValue,
    byAKI    :: SMultiMap "byAKI" s AKI Hash,
    mftByAKI :: SMultiMap "mftByAKI" s AKI (Hash, Int)
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
            MftRO mft -> MM.put tx (mftByAKI store) aki' (h, getMftNumber mft)
            _         -> pure ()


deleteObject :: Storage s => Tx s 'RW -> RpkiObjectStore s -> Hash -> IO ()
deleteObject tx store h = do
    ro' <- getByHash tx store h
    ifJust ro' $ \ro -> do 
        M.delete tx (objects store) h
        ifJust (getAKI ro) $ \aki' -> do 
            MM.delete tx (byAKI store) aki' h
            case ro of
                MftRO mft -> MM.delete tx (mftByAKI store) aki' (h, getMftNumber mft)
                _         -> pure ()      

findLatestMftByAKI :: Storage s => Tx s m -> RpkiObjectStore s -> AKI -> IO (Maybe MftObject)
findLatestMftByAKI tx store aki' = 
    MM.fold tx (mftByAKI store) aki' f Nothing >>= \case
        Nothing        -> pure Nothing
        Just (hash, _) -> do
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

findMftsByAKI :: Storage s => Tx s m -> RpkiObjectStore s -> AKI -> IO [MftObject]
findMftsByAKI tx store aki' = 
    MM.fold tx (mftByAKI store) aki' f []
    where
        f mfts _ (h, _) = accumulate <$> getByHash tx store h
            where 
                accumulate = \case          
                    Just (MftRO mft) -> mft : mfts
                    _ -> mfts            

-- This is for testing purposes mostly
getAll :: Storage s => Tx s m -> RpkiObjectStore s -> IO [RpkiObject]
getAll tx store = reverse <$> M.fold tx (objects store) f []
    where
        f ros _ v = pure $ fromSValue v : ros


-- | TA Store 
newtype TAStore s = TAStore (SMap "trust-anchors" s TaName STA)

instance Storage s => WithStorage s (TAStore s) where
    storage (TAStore s) = storage s


putTA :: Storage s => Tx s 'RW -> TAStore s -> STA -> IO ()
putTA tx (TAStore s) ta = M.put tx s (getTaName $ tal ta) ta

getTA :: Storage s => Tx s m -> TAStore s -> TaName -> IO (Maybe STA)
getTA tx (TAStore s) name = M.get tx s name

ifJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
ifJust a f = maybe (pure ()) f a


-- | Validation result store
data VResultStore s = VResultStore {
    results  :: SMap "results" s VContext VResult
}

instance Storage s => WithStorage s (VResultStore s) where
    storage (VResultStore s) = storage s

putVResult :: Storage s => Tx s 'RW -> VResultStore s -> VResult -> IO ()
putVResult tx (VResultStore s) vr = M.put tx s (path vr) vr


data RepositoryStore s = RepositoryStore {
    repositories      :: SMap "repositories" s URI SRepository,
    repositoriesPerTA :: SMultiMap "repositoriesPerTA" s TaName URI
}

instance Storage s => WithStorage s (RepositoryStore s) where
    storage (RepositoryStore s _) = storage s


newRepository :: Repository -> SRepository
newRepository r = SRepository r NEW 

putRepository :: Storage s => Tx s 'RW -> RepositoryStore s -> SRepository -> TaName -> IO ()
putRepository tx s r taName = do 
    let repoUri = repositoryURI $ repo r
    M.put tx (repositories s) repoUri r
    MM.put tx (repositoriesPerTA s) taName repoUri

getRepositoriesForTA :: Storage s => Tx s m -> RepositoryStore s -> TaName -> IO [SRepository]
getRepositoriesForTA tx s taName = MM.fold tx (repositoriesPerTA s) taName f []
    where
        f ros _ uri =   
            M.get tx (repositories s) uri >>= \case
                Just rs -> pure $! rs : ros
                Nothing -> pure ros


data DB s = DB {
    taStore :: TAStore s, 
    repositoryStore :: RepositoryStore s, 
    objectStore :: RpkiObjectStore s,
    resultStore :: VResultStore s
}
