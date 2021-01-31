{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE OverloadedStrings     #-}

module RPKI.Store.Database where

import           Codec.Serialise
import           Control.Concurrent.STM   (atomically)
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.IO.Class
import           Control.Monad.Reader     (ask)
import qualified Control.Monad.State as St
import           Control.Monad.Trans.Control

import           Data.Int
import           Data.IORef.Lifted

import qualified Data.List                as List
import           Data.List.NonEmpty       (NonEmpty)
import           Data.Maybe               (fromMaybe)

import           GHC.Generics

import           RPKI.AppState
import           RPKI.Config              (Size)
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))
import           RPKI.TAL

import qualified RPKI.Store.Base.Map      as M
import qualified RPKI.Store.Base.MultiMap as MM
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import           RPKI.Store.Sequence

import           RPKI.Parallel
import           RPKI.Time                (Instant)
import           RPKI.Util                (fmtEx, increment, ifJust)

import           RPKI.AppMonad
import           RPKI.Repository
import           RPKI.Store.Repository



data StorableTA = StorableTA {
    tal                 :: TAL,
    taCert              :: CerObject,
    fetchStatus         :: FetchStatus,
    initialRepositories :: NonEmpty Repository
} deriving (Show, Eq, Generic, Serialise)

data ROMeta = ROMeta {
        insertedBy :: WorldVersion,
        validatedBy :: Maybe WorldVersion
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Serialise)

newtype MftMonotonousNumber = MftMonotonousNumber Int64 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Serialise)

data MftTimingMark = MftTimingMark Instant Instant 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Serialise)

-- | RPKI objects store
data RpkiObjectStore s = RpkiObjectStore {
    keys        :: Sequence s,
    objects     :: SMap "objects" s ArtificialKey SValue,
    hashToKey   :: SMap "hash-to-key" s Hash ArtificialKey,
    uriToKey    :: SMap "uri-to-key" s URI ArtificialKey,
    mftByAKI    :: SMultiMap "mft-by-aki" s AKI (ArtificialKey, MftTimingMark),
    objectMetas :: SMap "object-meta" s ArtificialKey ROMeta
} deriving stock (Generic)


instance Storage s => WithStorage s (RpkiObjectStore s) where
    storage = storage . objects


-- | TA Store 
newtype TAStore s = TAStore (SMap "trust-anchors" s TaName StorableTA)

instance Storage s => WithStorage s (TAStore s) where
    storage (TAStore s) = storage s


newtype ArtificialKey = ArtificialKey Int64
    deriving (Show, Eq, Generic, Serialise)

newtype ValidationsStore s = ValidationsStore {
    results :: SMap "validations" s WorldVersion Validations    
}

instance Storage s => WithStorage s (ValidationsStore s) where
    storage (ValidationsStore s) = storage s

newtype MetricsStore s = MetricsStore {
    metrics :: SMap "metrics" s WorldVersion AppMetric    
}

instance Storage s => WithStorage s (MetricsStore s) where
    storage (MetricsStore s) = storage s


-- | VRP store
newtype VRPStore s = VRPStore {    
    vrps :: SMap "vrps" s WorldVersion [Vrp]
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
getByHash tx store h = (snd <$>) <$> getByHash_ tx store h    

getByHash_ :: (MonadIO m, Storage s) => 
              Tx s mode -> RpkiObjectStore s -> Hash -> m (Maybe (ArtificialKey, RpkiObject))
getByHash_ tx RpkiObjectStore {..} h = liftIO $
    M.get tx hashToKey h >>= \case
        Nothing -> pure Nothing 
        Just k  -> 
            M.get tx objects k >>= \case 
                Nothing -> pure Nothing 
                Just sv -> pure $ Just (k, fromSValue sv)
            
getByUri :: (MonadIO m, Storage s) => 
            Tx s mode -> RpkiObjectStore s -> URI -> m (Maybe RpkiObject)
getByUri tx RpkiObjectStore {..} uri = liftIO $ do 
    M.get tx uriToKey uri >>= \case 
        Nothing -> pure Nothing 
        Just k  -> 
            M.get tx objects k >>= \case 
                Nothing -> pure Nothing 
                Just sv -> pure $ Just $ fromSValue sv


putObject :: (MonadIO m, Storage s) => 
            Tx s 'RW -> RpkiObjectStore s -> StorableObject RpkiObject -> WorldVersion -> m ()
putObject tx RpkiObjectStore {..} (StorableObject ro sv) wv = liftIO $ do
    let h = getHash ro
    exists <- M.exists tx hashToKey h
    -- check if this object is already there, don't insert it twice
    unless exists $ do     
        SequenceValue k <- nextValue tx keys
        let key = ArtificialKey k
        M.put tx hashToKey h key
        M.put tx objects key sv   
        forM_ (getLocations ro) $ \location -> 
            M.put tx uriToKey (getURL location) key
        M.put tx objectMetas key (ROMeta wv Nothing)  
        ifJust (getAKI ro) $ \aki' ->
            case ro of
                MftRO mft -> MM.put tx mftByAKI aki' (key, getMftTimingMark mft)
                _         -> pure ()

hashExists :: (MonadIO m, Storage s) => 
            Tx s mode -> RpkiObjectStore s -> Hash -> m Bool
hashExists tx store h = liftIO $ M.exists tx (hashToKey store) h


deleteObject :: (MonadIO m, Storage s) => Tx s 'RW -> RpkiObjectStore s -> Hash -> m ()
deleteObject tx store@RpkiObjectStore {..} h = liftIO $ do
    p <- getByHash_ tx store h
    ifJust p $ \(k, ro) -> do         
        M.delete tx objects k
        M.delete tx objectMetas k
        M.delete tx hashToKey h
        ifJust (getAKI ro) $ \aki' ->
            case ro of
                MftRO mft -> MM.delete tx mftByAKI aki' (k, getMftTimingMark mft)
                _         -> pure ()        


findLatestMftByAKI :: (MonadIO m, Storage s) => 
                    Tx s mode -> RpkiObjectStore s -> AKI -> m (Maybe MftObject)
findLatestMftByAKI tx RpkiObjectStore {..} aki' = liftIO $ 
    MM.foldS tx mftByAKI aki' chooseMaxNum Nothing >>= \case
        Nothing     -> pure Nothing
        Just (k, _) -> do 
            o <- (fromSValue <$>) <$> M.get tx objects k
            pure $! case o of 
                Just (MftRO mft) -> Just mft
                _                -> Nothing
    where
        chooseMaxNum latest _ (hash, orderingNum) = 
            pure $! case latest of 
                Nothing                       -> Just (hash, orderingNum)
                Just (_, latestNum) 
                    | orderingNum > latestNum -> Just (hash, orderingNum)
                    | otherwise               -> latest

findMftsByAKI :: (MonadIO m, Storage s) => 
                Tx s mode -> RpkiObjectStore s -> AKI -> m [MftObject]
findMftsByAKI tx RpkiObjectStore {..} aki' = liftIO $ 
    MM.foldS tx mftByAKI aki' f []
    where
        f mfts _ (k, _) = do 
            o <- (fromSValue <$>) <$> M.get tx objects k
            pure $! accumulate o            
            where 
                accumulate (Just (MftRO mft)) = mft : mfts
                accumulate _                  = mfts            
    

markValidated :: (MonadIO m, Storage s) => 
                Tx s 'RW -> RpkiObjectStore s -> Hash -> WorldVersion -> m ()
markValidated tx RpkiObjectStore {..} hash wv = liftIO $ do
    k <- M.get tx hashToKey hash
    ifJust k $ \key -> do 
        z <- M.get tx objectMetas key
        ifJust z $ \meta -> let 
            m = meta { validatedBy = Just wv }
            in M.put tx objectMetas key m                        

-- This is for testing purposes mostly
getAll :: (MonadIO m, Storage s) => Tx s mode -> RpkiObjectStore s -> m [RpkiObject]
getAll tx store = map (fromSValue . snd) <$> 
    liftIO (M.all tx (objects store))


-- | Get something from the manifest that would allow us to judge 
-- which MFT is newer/older.
getMftTimingMark :: MftObject -> MftTimingMark
getMftTimingMark mft = let 
    m = getCMSContent $ cmsPayload mft 
    in MftTimingMark (thisTime m) (nextTime m)


-- TA store functions

putTA :: (MonadIO m, Storage s) => Tx s 'RW -> TAStore s -> StorableTA -> m ()
putTA tx (TAStore s) ta = liftIO $ M.put tx s (getTaName $ tal ta) ta

getTA :: (MonadIO m, Storage s) => Tx s mode -> TAStore s -> TaName -> m (Maybe StorableTA)
getTA tx (TAStore s) name = liftIO $ M.get tx s name

putValidations :: (MonadIO m, Storage s) => 
            Tx s 'RW -> ValidationsStore s -> WorldVersion -> Validations -> m ()
putValidations tx ValidationsStore {..} wv validations = liftIO $ M.put tx results wv validations

validationsForVersion :: (MonadIO m, Storage s) => 
                        Tx s mode -> ValidationsStore s -> WorldVersion -> m (Maybe Validations)
validationsForVersion tx ValidationsStore {..} wv = liftIO $ M.get tx results wv

deleteValidations :: (MonadIO m, Storage s) => 
                Tx s 'RW -> DB s -> WorldVersion -> m ()
deleteValidations tx DB { validationsStore = ValidationsStore {..} } wv = 
    liftIO $ M.delete tx results wv
                 

getVrps :: (MonadIO m, Storage s) => 
            Tx s mode -> DB s -> WorldVersion -> m [Vrp]
getVrps tx DB { vrpStore = VRPStore vrpMap } wv = 
    liftIO $ fromMaybe [] <$> M.get tx vrpMap wv    

deleteVRPs :: (MonadIO m, Storage s) => 
            Tx s 'RW -> DB s -> WorldVersion -> m ()
deleteVRPs tx DB { vrpStore = VRPStore vrpMap } wv = liftIO $ M.delete tx vrpMap wv

putVrps :: (MonadIO m, Storage s) => 
            Tx s 'RW -> DB s -> [Vrp] -> WorldVersion -> m ()
putVrps tx DB { vrpStore = VRPStore vrpMap } vrps worldVersion = 
    liftIO $ M.put tx vrpMap worldVersion vrps    

putVersion :: (MonadIO m, Storage s) => 
        Tx s 'RW -> DB s -> WorldVersion -> VersionState -> m ()
putVersion tx DB { versionStore = VersionStore s } wv versionState = 
    liftIO $ M.put tx s wv versionState

allVersions :: (MonadIO m, Storage s) => 
            Tx s mode -> DB s -> m [(WorldVersion, VersionState)]
allVersions tx DB { versionStore = VersionStore s } = 
    liftIO $ M.all tx s

deleteVersion :: (MonadIO m, Storage s) => 
        Tx s 'RW -> DB s -> WorldVersion -> m ()
deleteVersion tx DB { versionStore = VersionStore s } wv = 
    liftIO $ M.delete tx s wv


completeWorldVersion :: Storage s => 
                        Tx s 'RW -> DB s -> WorldVersion -> IO ()
completeWorldVersion tx database worldVersion =    
    putVersion tx database worldVersion CompletedVersion


putMetrics :: (MonadIO m, Storage s) => 
            Tx s 'RW -> MetricsStore s -> WorldVersion -> AppMetric -> m ()
putMetrics tx MetricsStore {..} wv appMetric = 
    liftIO $ M.put tx metrics wv appMetric

metricsForVersion :: (MonadIO m, Storage s) => 
                    Tx s mode -> MetricsStore s -> WorldVersion -> m (Maybe AppMetric)
metricsForVersion tx MetricsStore {..} wv = 
    liftIO $ M.get tx metrics wv    


-- More complicated operations

-- Delete all the objects from the objectStore if they were 
-- visited longer than certain time ago.
cleanObjectCache :: Storage s => 
                    DB s -> 
                    (WorldVersion -> Bool) -> -- ^ function that decides if the object is too old to stay in cache
                    IO (Int, Int)
cleanObjectCache DB {..} tooOld = do
    kept    <- newIORef (0 :: Int)
    deleted <- newIORef (0 :: Int)
    
    let queueToDeletOrKeep worldVersion queue hash = 
            if tooOld worldVersion
                then increment deleted >> atomically (writeCQueue queue hash)
                else increment kept

    let readOldObjects queue =
            roTx objectStore $ \tx ->
                M.traverse tx (hashToKey objectStore) $ \hash key -> do
                    r <- M.get tx (objectMetas objectStore) key
                    ifJust r $ \ROMeta {..} -> do
                        let cutoffVersion = fromMaybe insertedBy validatedBy
                        queueToDeletOrKeep cutoffVersion queue hash                        

    -- Don't lock the DB for potentially too long, use big but finite chunks
    let deleteObjects queue =
            readQueueChunked queue 50_000 $ \quuElems ->
                rwTx objectStore $ \tx ->
                    forM_ quuElems $ deleteObject tx objectStore

    mapException (AppException . storageError) 
        $ voidRun "cleanObjectCache" 
        $ bracketChanClosable 
                50_000
                (liftIO . readOldObjects)
                (liftIO . deleteObjects)
                (const $ pure ())    
                    
    (,) <$> readIORef deleted <*> readIORef kept


-- Clean up older versions and delete everything associated with the version, i,e.
-- VRPs and validation results.
deleteOldVersions :: (MonadIO m, Storage s) => 
                    DB s -> 
                    (WorldVersion -> Bool) -> -- ^ function that determines if an object is too old to be in cache
                    m Int
deleteOldVersions database tooOld = 
    mapException (AppException . storageError) <$> liftIO $ do

    versions <- roTx database $ \tx -> allVersions tx database    
    let toDelete = 
            case [ version | (version, CompletedVersion) <- versions ] of
                []       -> 
                    case versions of 
                        []  -> []
                        [_] -> [] -- don't delete the last one
                        _ -> filter (tooOld . fst) versions            

                finished -> 
                    -- delete all too old except for the last finished one    
                    let lastFinished = List.maximum finished 
                    in filter (( /= lastFinished) . fst) $ filter (tooOld . fst) versions
    
    rwTx database $ \tx -> 
        forM_ toDelete $ \(worldVersion, _) -> do
            deleteValidations tx database worldVersion
            deleteVRPs tx database worldVersion
            deleteVersion tx database worldVersion
    
    pure $! List.length toDelete


-- | Find the latest completed world version 
-- 
getLastCompletedVersion :: (Storage s) => 
                        DB s -> Tx s 'RO -> IO (Maybe WorldVersion)
getLastCompletedVersion database tx = do 
        vs <- allVersions tx database
        pure $! case [ v | (v, CompletedVersion) <- vs ] of         
            []  -> Nothing
            vs' -> Just $ maximum vs'


data RpkiObjectStats = RpkiObjectStats {
    objectsStats    :: SStats,
    mftByAKIStats   :: SStats,
    objectMetaStats :: SStats,
    hashToKeyStats  :: SStats
} deriving stock (Show, Eq, Generic)

data VResultStats = VResultStats {     
    resultsStats :: SStats    
} deriving  (Show, Eq, Generic)

data RepositoryStats = RepositoryStats {
    rrdpStats  :: SStats,
    rsyncStats :: SStats,
    lastSStats :: SStats    
} deriving stock (Show, Eq, Generic)

data DBStats = DBStats {
    taStats         :: SStats,
    repositoryStats :: RepositoryStats,
    rpkiObjectStats :: RpkiObjectStats,    
    vResultStats    :: VResultStats,    
    vrpStats        :: SStats,    
    versionStats    :: SStats,
    sequenceStats   :: SStats
} deriving stock (Show, Eq, Generic)


-- Compute database stats
dbStats :: (MonadIO m, Storage s) => 
        DB s -> m DBStats
dbStats db@DB {..} = liftIO $ roTx db $ \tx ->    
    DBStats <$>
        (let TAStore sm = taStore in M.stats tx sm) <*>
        repositoryStats tx <*>
        rpkiObjectStats tx <*>
        vResultStats tx <*>
        (let VRPStore sm = vrpStore in M.stats tx sm) <*>
        (let VersionStore sm = versionStore in M.stats tx sm) <*>
        M.stats tx sequences
    where
        rpkiObjectStats tx = 
            let RpkiObjectStore {..} = objectStore
            in RpkiObjectStats <$>
                M.stats tx objects <*>
                MM.stats tx mftByAKI <*>
                M.stats tx objectMetas <*>
                M.stats tx hashToKey

        repositoryStats tx = 
            let RepositoryStore {..} = repositoryStore
            in RepositoryStats <$>
                M.stats tx rrdpS <*>
                M.stats tx rsyncS <*>
                M.stats tx lastS

        vResultStats tx = 
            let ValidationsStore results = validationsStore
            in VResultStats <$> M.stats tx results
   
                       
-- | Return total amount of bytes taken by the data in the DB
-- 
totalSpace :: DBStats -> Size
totalSpace DBStats {..} = 
    let fullStat = taStats 
                <> (let RepositoryStats{..} = repositoryStats 
                    in rrdpStats <> rsyncStats <> lastSStats) 
                <> (let RpkiObjectStats {..} = rpkiObjectStats
                    in objectsStats <> mftByAKIStats <> objectMetaStats <> hashToKeyStats)
                <> resultsStats vResultStats 
                <> vrpStats 
                <> versionStats 
                <> sequenceStats
        in statKeyBytes fullStat + statValueBytes fullStat



-- All of the stores of the application in one place
data DB s = DB {
    taStore          :: TAStore s, 
    repositoryStore  :: RepositoryStore s, 
    objectStore      :: RpkiObjectStore s,
    validationsStore :: ValidationsStore s,    
    vrpStore         :: VRPStore s,
    versionStore     :: VersionStore s,
    metricStore      :: MetricsStore s,
    sequences        :: SequenceMap s
} deriving stock (Generic)

instance Storage s => WithStorage s (DB s) where
    storage DB {..} = storage taStore


storageError :: SomeException -> AppError
storageError = StorageE . StorageError . fmtEx    



-- Utilities to have storage transaction in ValidatorT monad.

roAppTx :: (Storage s, WithStorage s ws) => 
            ws -> (Tx s 'RO -> ValidatorT IO a) -> ValidatorT IO a 
roAppTx s f = appTx s f roTx    

rwAppTx :: (Storage s, WithStorage s ws) => 
            ws -> (Tx s 'RW -> ValidatorT IO a) -> ValidatorT IO a
rwAppTx s f = appTx s f rwTx


appTx :: (Storage s, WithStorage s ws) => 
        ws 
        -> (Tx s mode -> ValidatorT IO a) 
        -> (ws 
            -> (Tx s mode -> IO (Either AppError a, ValidationState))
            -> IO (Either AppError a, ValidationState)) 
        -> ValidatorT IO a
appTx s f txF = do
    env <- ask        
    embedValidatorT $ transaction env `catch` 
                    (\(TxRollbackException e vs) -> pure (Left e, vs))
  where
    transaction env = txF s $ \tx -> do 
        z@(r, vs) <- runValidatorT env (f tx)
        case r of
            -- abort transaction on ExceptT error
            Left e  -> throwIO $ TxRollbackException e vs
            Right _ -> pure z
         

roAppTxEx :: (Storage s, WithStorage s ws, Exception exc) => 
            ws -> 
            (exc -> AppError) -> 
            (Tx s 'RO -> ValidatorT IO a) -> 
            ValidatorT IO a 
roAppTxEx ws err f = appTxEx ws err f roTx

rwAppTxEx :: (Storage s, WithStorage s ws, Exception exc) => 
            ws -> (exc -> AppError) -> 
            (Tx s 'RW -> ValidatorT IO a) -> ValidatorT IO a
rwAppTxEx s err f = appTxEx s err f rwTx


appTxEx :: (Storage s, WithStorage s ws, Exception exc) => 
            ws -> (exc -> AppError) -> 
            (Tx s mode -> ValidatorT IO a) -> 
            (s -> (Tx s mode -> IO (Either AppError a, ValidationState))
               -> IO (Either AppError a, ValidationState)) -> 
            ValidatorT IO a
appTxEx ws err f txF = do
    env <- ask
    embedValidatorT $ transaction env `catches` [
            Handler $ \(TxRollbackException e vs) -> pure (Left e, vs),
            Handler $ \e -> pure (Left (err e), mempty)
        ]       
    where
        transaction env = txF (storage ws) $ \tx -> do 
            z@(r, vs) <- runValidatorT env (f tx)
            case r of
                -- abort transaction on ExceptT error
                Left e  -> throwIO $ TxRollbackException e vs
                Right _ -> pure z


data TxRollbackException = TxRollbackException AppError ValidationState
    deriving stock (Show, Eq, Ord, Generic)

instance Exception TxRollbackException

