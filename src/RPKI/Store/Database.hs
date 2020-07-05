{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module RPKI.Store.Database where

import           Codec.Serialise
import           Control.Monad.IO.Class
import           Data.IORef.Lifted

import           Control.Lens
import           Data.Generics.Product.Typed

import           Data.Int
import           Data.List                   (length)

import           GHC.Generics

import           RPKI.Domain
import           RPKI.Store.Base.Map         (SMap (..))
import           RPKI.Store.Base.MultiMap    (SMultiMap (..))
import           RPKI.TAL
import           RPKI.Version

import qualified RPKI.Store.Base.Map         as M
import qualified RPKI.Store.Base.MultiMap    as MM
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import           RPKI.Store.Sequence

import           RPKI.Parallel
import           RPKI.Util (increment)

import           RPKI.Store.Data
import           RPKI.Store.Repository

import           Control.Concurrent.STM      (atomically)
import           Control.Monad               (forM_, void, when)



data ROMeta = ROMeta {
        validatedBy :: WorldVersion
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Serialise)

-- | RPKI objects store
data RpkiObjectStore s = RpkiObjectStore {
    objects     :: SMap "objects" s Hash SValue,
    byAKI       :: SMultiMap "byAKI" s AKI Hash,
    mftByAKI    :: SMultiMap "mftByAKI" s AKI (Hash, Int),
    objectMetas :: SMap "object-meta" s Hash ROMeta
} deriving stock (Generic)

instance Storage s => WithStorage s (RpkiObjectStore s) where
    storage = storage . objects


-- | TA Store 
newtype TAStore s = TAStore (SMap "trust-anchors" s TaName StorableTA)

instance Storage s => WithStorage s (TAStore s) where
    storage (TAStore s) = storage s


newtype ArtificialKey = ArtificialKey Int64
    deriving (Show, Eq, Generic, Serialise)

-- | Validation result store
data VResultStore s = VResultStore {
    keys    :: Sequence s,
    results :: SMap "validation-results" s ArtificialKey VResult,
    whToKey :: SMultiMap "wh-to-key" s WorldVersion ArtificialKey
}

instance Storage s => WithStorage s (VResultStore s) where
    storage (VResultStore s _ _) = storage s


-- | VRP store
newtype VRPStore s = VRPStore {
    vrps :: SMultiMap "vrps" s WorldVersion Roa
}

instance Storage s => WithStorage s (VRPStore s) where
    storage (VRPStore s) = storage s


-- Version store
newtype VersionStore s = VersionStore {
    vrps :: SMap "versions" s WorldVersion VersionState
}

instance Storage s => WithStorage s (VersionStore s) where
    storage (VersionStore s) = storage s




getByHash :: (MonadIO m, Storage s) => 
            Tx s mode -> RpkiObjectStore s -> Hash -> m (Maybe RpkiObject)
getByHash tx store h = (fromSValue <$>) <$> liftIO (M.get tx (objects store) h)

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

hashExists :: (MonadIO m, Storage s) => 
            Tx s mode -> RpkiObjectStore s -> Hash -> m Bool
hashExists tx store h = liftIO $ M.exists tx (objects store) h


deleteObject :: (MonadIO m, Storage s) => Tx s 'RW -> RpkiObjectStore s -> Hash -> m ()
deleteObject tx store@RpkiObjectStore {..} h = liftIO $ do
    ro' <- getByHash tx store h
    ifJust ro' $ \ro -> do         
        M.delete tx objects h
        ifJust (getAKI ro) $ \aki' -> do 
            MM.delete tx byAKI aki' h
            case ro of
                MftRO mft -> MM.delete tx mftByAKI aki' (h, getMftNumber mft)
                _         -> pure ()      
        M.delete tx objectMetas h

findLatestMftByAKI :: (MonadIO m, Storage s) => 
                    Tx s mode -> RpkiObjectStore s -> AKI -> m (Maybe MftObject)
findLatestMftByAKI tx store@RpkiObjectStore {..} aki' = liftIO $ 
    MM.foldS tx mftByAKI aki' f Nothing >>= \case
        Nothing        -> pure Nothing
        Just (hash, _) -> do 
            o <- getByHash tx store hash
            pure $ case o of 
                Just (MftRO mft) -> Just mft
                _                -> Nothing
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
                accumulate (Just (MftRO mft)) = mft : mfts
                accumulate _                  = mfts            


markValidated :: (MonadIO m, Storage s) => 
                Tx s 'RW -> RpkiObjectStore s -> Hash -> WorldVersion -> m ()
markValidated tx RpkiObjectStore {..} hash wv = liftIO $ do
    meta <- M.get tx objectMetas hash 
    -- TODO If ROMeta stores more stuff, logic must be different
    let meta' = maybe (ROMeta wv) (& typed @WorldVersion .~ wv) meta
    M.put tx objectMetas hash meta'

-- This is for testing purposes mostly
getAll :: (MonadIO m, Storage s) => Tx s mode -> RpkiObjectStore s -> m [RpkiObject]
getAll tx store = map (fromSValue . snd) <$> liftIO (M.all tx (objects store))


-- TA store functions

putTA :: (MonadIO m, Storage s) => Tx s 'RW -> TAStore s -> StorableTA -> m ()
putTA tx (TAStore s) ta = liftIO $ M.put tx s (getTaName $ tal ta) ta

getTA :: (MonadIO m, Storage s) => Tx s mode -> TAStore s -> TaName -> m (Maybe StorableTA)
getTA tx (TAStore s) name = liftIO $ M.get tx s name

ifJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
ifJust a f = maybe (pure ()) f a


putVResult :: (MonadIO m, Storage s) => 
            Tx s 'RW -> VResultStore s -> WorldVersion -> VResult -> m ()
putVResult tx VResultStore {..} wv vr = liftIO $ do 
    SequenceValue k <- nextValue tx keys     
    MM.put tx whToKey wv (ArtificialKey k)
    M.put tx results (ArtificialKey k) vr

allVResults :: (MonadIO m, Storage s) => 
                Tx s mode -> VResultStore s -> WorldVersion -> m [VResult]
allVResults tx VResultStore {..} wv = liftIO $ MM.foldS tx whToKey wv f []
    where
        f vrs _ artificialKey = 
            maybe vrs (: vrs) <$> M.get tx results artificialKey            



putVRP :: (MonadIO m, Storage s) => 
        Tx s 'RW -> VRPStore s -> WorldVersion -> Roa -> m ()
putVRP tx (VRPStore s) wv vrp = liftIO $ MM.put tx s wv vrp

allVRPs :: (MonadIO m, Storage s) => 
            Tx s mode -> VRPStore s -> WorldVersion -> m [Roa]
allVRPs tx (VRPStore s) wv = liftIO $ MM.allForKey tx s wv


putVersion :: (MonadIO m, Storage s) => 
        Tx s 'RW -> VersionStore s -> WorldVersion -> VersionState -> m ()
putVersion tx (VersionStore s) wv vrp = liftIO $ M.put tx s wv vrp

allVersions :: (MonadIO m, Storage s) => 
            Tx s mode -> VersionStore s -> m [(WorldVersion, VersionState)]
allVersions tx (VersionStore s) = liftIO $ M.all tx s


-- More complicated operations

-- Delete all the objects from the objectStore if they were 
-- validated longer than certain time ago.
cleanObjectCache :: (MonadIO m, Storage s) => 
                    RpkiObjectStore s -> 
                    (WorldVersion -> Bool) -> -- ^ function that determines if an object is too old to be in cache
                    m (Int, Int)
cleanObjectCache objectStore@RpkiObjectStore {..} tooOld = liftIO $ do
    kept    <- newIORef (0 :: Int)
    deleted <- newIORef (0 :: Int)
    
    let readOldObjects queue = do
            roTx objectStore $ \tx -> do
                M.traverse tx objectMetas $ \hash ROMeta {..} -> do
                    if tooOld validatedBy
                        then increment deleted >> atomically (writeCQueue queue hash)
                        else increment kept
                    when (tooOld validatedBy) $ atomically $ writeCQueue queue hash

        -- Don't lock the DB for potentially too long, use big but finite chunks
    let deleteObjects queue = do
            readQueueChunked queue 50_000 $ \quuElems -> do
                rwTx objectStore $ \tx -> do 
                    putStrLn $ "quuElems size = " <> show (length quuElems)
                    forM_ quuElems $ deleteObject tx objectStore

    void $ bracketChanClosable 
            100_000
            readOldObjects
            deleteObjects
            (const $ pure ())    
    
    (,) <$> readIORef deleted <*> readIORef kept


-- All of the stores of the application in one place
data DB s = DB {
    taStore         :: TAStore s, 
    repositoryStore :: RepositoryStore s, 
    objectStore     :: RpkiObjectStore s,
    resultStore     :: VResultStore s,
    vrpStore        :: VRPStore s,
    versionStore    :: VersionStore s
} deriving stock (Generic)

instance Storage s => WithStorage s (DB s) where
    storage DB {..} = storage taStore
