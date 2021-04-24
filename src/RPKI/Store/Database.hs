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
import           Control.Monad.Trans.Maybe
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Reader     (ask)
import           Data.Int
import           Data.IORef.Lifted

import qualified Data.List                as List
import           Data.List.NonEmpty       (NonEmpty)
import qualified Data.List.NonEmpty       as NonEmpty
import qualified Data.Set.NonEmpty        as NESet
import           Data.Maybe               (fromMaybe, catMaybes)
import qualified Data.Set                 as Set

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
import           RPKI.Util                (increment, ifJust, ifJustM)

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

data MftTimingMark = MftTimingMark Instant Instant 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Serialise)

newtype UrlKey = UrlKey ArtificialKey
    deriving (Show, Eq, Ord, Generic, Serialise)

newtype ObjectKey = ObjectKey ArtificialKey
    deriving (Show, Eq, Ord, Generic, Serialise)

newtype ArtificialKey = ArtificialKey Int64
    deriving (Show, Eq, Ord, Generic, Serialise)


-- | RPKI objects store

-- Important: 
-- Locations of an object (URLs where the object was downloaded from) are stored
-- in a separate pair of maps urlKeyToObjectKey and objectKeyToUrlKeys. 
-- That's why all the fiddling with locations in putObject, getLocatedByKey 
-- and deleteObject.
-- 
-- Also, since URLs are relatives long, there's a separate mapping between 
-- URLs and artificial UrlKeys.
-- 
data RpkiObjectStore s = RpkiObjectStore {
    keys           :: Sequence s,
    objects        :: SMap "objects" s ObjectKey SValue,
    hashToKey      :: SMap "hash-to-key" s Hash ObjectKey,    
    mftByAKI       :: SMultiMap "mft-by-aki" s AKI (ObjectKey, MftTimingMark),
    lastValidMft   :: SMap "last-valid-mft" s AKI ObjectKey,    

    objectInsertedBy  :: SMap "object-inserted-by" s ObjectKey WorldVersion,
    objectValidatedBy :: SMap "object-validated-by" s ObjectKey WorldVersion,

    -- Object URL mapping
    uriToUriKey    :: SMap "uri-to-uri-key" s RpkiURL UrlKey,
    uriKeyToUri    :: SMap "uri-key-to-uri" s UrlKey RpkiURL,

    urlKeyToObjectKey  :: SMultiMap "uri-to-object-key" s UrlKey ObjectKey,
    objectKeyToUrlKeys :: SMap "object-key-to-uri" s ObjectKey [UrlKey]
} deriving stock (Generic)


instance Storage s => WithStorage s (RpkiObjectStore s) where
    storage = storage . objects


-- | TA Store 
newtype TAStore s = TAStore (SMap "trust-anchors" s TaName StorableTA)

instance Storage s => WithStorage s (TAStore s) where
    storage (TAStore s) = storage s

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
            Tx s mode -> RpkiObjectStore s -> Hash -> m (Maybe (Located RpkiObject))
getByHash tx store h = (snd <$>) <$> getByHash_ tx store h    

getByHash_ :: (MonadIO m, Storage s) => 
              Tx s mode -> RpkiObjectStore s -> Hash -> m (Maybe (ObjectKey, Located RpkiObject))
getByHash_ tx store@RpkiObjectStore {..} h = liftIO $ runMaybeT $ do 
    objectKey <- MaybeT $ M.get tx hashToKey h
    z         <- MaybeT $ getLocatedByKey tx store objectKey
    pure (objectKey, z)
            
getByUri :: (MonadIO m, Storage s) => 
            Tx s mode -> RpkiObjectStore s -> RpkiURL -> m [Located RpkiObject]
getByUri tx store@RpkiObjectStore {..} uri = liftIO $ do 
    M.get tx uriToUriKey uri >>= \case 
        Nothing -> pure []
        Just uriKey ->         
            fmap catMaybes $ 
                MM.allForKey tx urlKeyToObjectKey uriKey >>=
                mapM (getLocatedByKey tx store)


getObjectByKey :: (MonadIO m, Storage s) => 
                Tx s mode -> RpkiObjectStore s -> ObjectKey -> m (Maybe RpkiObject)
getObjectByKey tx RpkiObjectStore {..} k = liftIO $    
    (fromSValue <$>) <$> M.get tx objects k             

getLocatedByKey :: (MonadIO m, Storage s) => 
                Tx s mode -> RpkiObjectStore s -> ObjectKey -> m (Maybe (Located RpkiObject))
getLocatedByKey tx store@RpkiObjectStore {..} k = liftIO $ runMaybeT $ do     
    object    <- MaybeT $ getObjectByKey tx store k
    uriKeys   <- MaybeT $ M.get tx objectKeyToUrlKeys k
    locations <- MaybeT 
                    $ (toNESet . catMaybes <$>)
                    $ mapM (M.get tx uriKeyToUri) uriKeys             
    pure $ Located (Locations locations) object


putObject :: (MonadIO m, Storage s) => 
            Tx s 'RW 
            -> RpkiObjectStore s 
            -> StorableObject RpkiObject
            -> WorldVersion -> m ()
putObject tx objectStore@RpkiObjectStore {..} StorableObject {..} wv = liftIO $ do
    let h = getHash object
    exists <- M.exists tx hashToKey h    
    unless exists $ do          
        SequenceValue k <- nextValue tx keys
        let objectKey = ObjectKey $ ArtificialKey k
        M.put tx hashToKey h objectKey
        M.put tx objects objectKey storable                           
        M.put tx objectInsertedBy objectKey wv
        ifJust (getAKI object) $ \aki' ->
            case object of
                MftRO mft -> MM.put tx mftByAKI aki' (objectKey, getMftTimingMark mft)
                _         -> pure ()


linkObjectToUrl :: (MonadIO m, Storage s) => 
                Tx s 'RW 
                -> RpkiObjectStore s 
                -> RpkiURL
                -> Hash
                -> m ()
linkObjectToUrl tx objectStore@RpkiObjectStore {..} rpkiURL hash = liftIO $ do    
    ifJustM (M.get tx hashToKey hash) $ \objectKey -> do        
        z <- M.get tx uriToUriKey rpkiURL 
        urlKey <- maybe (saveUrl rpkiURL) pure z                
        
        M.get tx objectKeyToUrlKeys objectKey >>= \case 
            Nothing -> do 
                M.put tx objectKeyToUrlKeys objectKey [urlKey]
                MM.put tx urlKeyToObjectKey urlKey objectKey
            Just existingUrlKeys -> 
                when (urlKey `notElem` existingUrlKeys) $ do 
                    M.put tx objectKeyToUrlKeys objectKey (urlKey : existingUrlKeys)
                    MM.put tx urlKeyToObjectKey urlKey objectKey
  where
    saveUrl url = do 
        SequenceValue k <- nextValue tx keys
        let urlKey = UrlKey $ ArtificialKey k
        M.put tx uriToUriKey url urlKey
        M.put tx uriKeyToUri urlKey url            
        pure urlKey


hashExists :: (MonadIO m, Storage s) => 
            Tx s mode -> RpkiObjectStore s -> Hash -> m Bool
hashExists tx store h = liftIO $ M.exists tx (hashToKey store) h


deleteObject :: (MonadIO m, Storage s) => Tx s 'RW -> RpkiObjectStore s -> Hash -> m ()
deleteObject tx store@RpkiObjectStore {..} h = liftIO $
    ifJustM (M.get tx hashToKey h) $ \objectKey -> do               
        ifJustM (getObjectByKey tx store objectKey) $ \ro -> do 
            M.delete tx objects objectKey
            M.delete tx objectInsertedBy objectKey        
            M.delete tx objectValidatedBy objectKey        
            M.delete tx hashToKey h        
            ifJustM (M.get tx objectKeyToUrlKeys objectKey) $ \urlKeys -> do 
                M.delete tx objectKeyToUrlKeys objectKey            
                forM_ urlKeys $ \urlKey ->
                    MM.delete tx urlKeyToObjectKey urlKey objectKey                
            
                ifJust (getAKI ro) $ \aki' -> do 
                    M.delete tx lastValidMft aki'
                    case ro of
                        MftRO mft -> MM.delete tx mftByAKI aki' (objectKey, getMftTimingMark mft)
                        _         -> pure ()        


findLatestMftByAKI :: (MonadIO m, Storage s) => 
                    Tx s mode -> RpkiObjectStore s -> AKI -> m (Maybe (Located MftObject))
findLatestMftByAKI tx store@RpkiObjectStore {..} aki' = liftIO $ 
    MM.foldS tx mftByAKI aki' chooseLatest Nothing >>= \case
        Nothing     -> pure Nothing
        Just (k, _) -> do 
            o <- getLocatedByKey tx store k
            pure $! case o of 
                Just z@(Located loc (MftRO mft)) -> Just $ Located loc mft
                _                                -> Nothing
  where
    chooseLatest latest _ (k, timingMark) = do 
        pure $! case latest of 
            Nothing                       -> Just (k, timingMark)
            Just (_, latestMark) 
                | timingMark > latestMark -> Just (k, timingMark)
                | otherwise               -> latest    


markValidated :: (MonadIO m, Storage s) => 
                Tx s 'RW -> RpkiObjectStore s -> Hash -> WorldVersion -> m ()
markValidated tx RpkiObjectStore {..} hash wv = liftIO $    
    ifJustM (M.get tx hashToKey hash) $ \key -> 
        M.put tx objectValidatedBy key wv                                


-- This is for testing purposes mostly
getAll :: (MonadIO m, Storage s) => Tx s mode -> RpkiObjectStore s -> m [Located RpkiObject]
getAll tx store = liftIO $ do 
    keys <- M.keys tx (objects store)
    objs <- forM keys $ \k -> getLocatedByKey tx store k
    pure $ catMaybes objs


-- | Get something from the manifest that would allow us to judge 
-- which MFT is newer/older.
getMftTimingMark :: MftObject -> MftTimingMark
getMftTimingMark mft = let 
    m = getCMSContent $ cmsPayload mft 
    in MftTimingMark (thisTime m) (nextTime m)


markLatestValidMft :: (MonadIO m, Storage s) => 
                    Tx s 'RW -> RpkiObjectStore s -> AKI -> Hash -> m ()
markLatestValidMft tx RpkiObjectStore {..} aki hash = liftIO $ do 
    k <- M.get tx hashToKey hash
    ifJust k $ M.put tx lastValidMft aki


getLatestValidMftByAKI :: (MonadIO m, Storage s) => 
                        Tx s mode -> RpkiObjectStore s -> AKI -> m (Maybe (Located MftObject))
getLatestValidMftByAKI tx store@RpkiObjectStore {..} aki = liftIO $ do
    M.get tx lastValidMft aki >>= \case 
        Nothing -> pure Nothing 
        Just k -> do 
            o <- getLocatedByKey tx store k
            pure $! case o of 
                Just z@(Located loc (MftRO mft)) -> Just $ Located loc mft
                _                                -> Nothing


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

data CleanUpResult = CleanUpResult {
        deletedObjects :: Int,
        keptObjects :: Int,
        deletedURLs :: Int
    }
    deriving (Show, Eq, Ord, Generic)

-- Delete all the objects from the objectStore if they were 
-- visited longer than certain time ago.
cleanObjectCache :: Storage s => 
                    DB s -> 
                    (WorldVersion -> Bool) -> -- ^ function that decides if the object is too old to stay in cache
                    IO CleanUpResult
cleanObjectCache DB {..} tooOld = do
    kept        <- newIORef (0 :: Int)
    deleted     <- newIORef (0 :: Int)
    deletedURLs <- newIORef (0 :: Int)
    
    let readOldObjects queue =
            roTx objectStore $ \tx ->
                M.traverse tx (hashToKey objectStore) $ \hash key -> do
                    z <- M.get tx (objectValidatedBy objectStore) key >>= \case 
                            Just validatedBy -> pure $ Just validatedBy
                            Nothing          -> M.get tx (objectInsertedBy objectStore) key                                                                        
                    ifJust z $ \cutoffVersion -> 
                        if tooOld cutoffVersion
                            then increment deleted >> atomically (writeCQueue queue hash)
                            else increment kept

    -- Don't lock the DB for potentially too long, use big but finite chunks
    let chunkSize = 2000
    let deleteObjects queue =
            readQueueChunked queue chunkSize $ \quuElems ->
                rwTx objectStore $ \tx ->
                    forM_ quuElems $ deleteObject tx objectStore

    mapException (AppException . storageError) 
        $ voidRun "cleanObjectCache" 
        $ bracketChanClosable 
                (2 * chunkSize)
                (liftIO . readOldObjects)
                (liftIO . deleteObjects)
                (const $ pure ())    
                    
    -- clean up URLs that don't have any objects referring to them
    mapException (AppException . storageError) 
        $ voidRun "cleanOrphanURLs" $ do 
            referencedUrlKeys <- liftIO 
                $ roTx objectStore 
                $ \tx ->                 
                    Set.fromList <$>
                        M.fold tx (objectKeyToUrlKeys objectStore) 
                            (\allUrlKey _ urlKeys -> pure $! urlKeys <> allUrlKey)
                            mempty

            let readUrls queue = 
                    roTx objectStore $ \tx ->
                        M.traverse tx (uriKeyToUri objectStore) $ \urlKey url ->
                            when (urlKey `Set.notMember` referencedUrlKeys) $
                                atomically (writeCQueue queue (urlKey, url))

            let deleteUrls queue = 
                    readQueueChunked queue chunkSize $ \quuElems ->
                        rwTx objectStore $ \tx ->
                            forM_ quuElems $ \(urlKey, url) -> do 
                                M.delete tx (uriKeyToUri objectStore) urlKey
                                M.delete tx (uriToUriKey objectStore) url
                                increment deletedURLs

            bracketChanClosable 
                (2 * chunkSize)
                (liftIO . readUrls)
                (liftIO . deleteUrls)
                (const $ pure ())            

    CleanUpResult <$> 
        readIORef deleted <*> 
        readIORef kept <*> 
        readIORef deletedURLs


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
    objectsStats       :: SStats,
    mftByAKIStats      :: SStats,    
    hashToKeyStats     :: SStats,
    lastValidMftStats  :: SStats,
    uriToUriKeyStat    :: SStats,
    uriKeyToUriStat    :: SStats,
    uriKeyToObjectKeyStat  :: SStats,
    objectKeyToUrlKeysStat :: SStats,
    objectInsertedByStats  :: SStats,
    objectValidatedByStats  :: SStats    
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
data TotalDBStats = TotalDBStats {
    dbStats         :: DBStats,
    total :: SStats    
} deriving stock (Show, Eq, Generic)


getTotalDbStats :: (MonadIO m, Storage s) => 
                    DB s -> m TotalDBStats
getTotalDbStats db = do 
    dbStats <- getDbStats db
    let total = totalStats dbStats
    pure TotalDBStats {..}

-- Compute database stats
getDbStats :: (MonadIO m, Storage s) => 
              DB s -> m DBStats
getDbStats db@DB {..} = liftIO $ roTx db $ \tx ->    
    DBStats <$>
        (let TAStore sm = taStore in M.stats tx sm) <*>
        repositoryStats tx <*>
        rpkiObjectStats tx <*>
        vResultStats tx <*>
        (let VRPStore sm = vrpStore in M.stats tx sm) <*>
        (let VersionStore sm = versionStore in M.stats tx sm) <*>
        M.stats tx sequences
    where
        rpkiObjectStats tx = do 
            let RpkiObjectStore {..} = objectStore
            objectsStats  <- M.stats tx objects
            mftByAKIStats <- MM.stats tx mftByAKI                                    
            hashToKeyStats  <- M.stats tx hashToKey
            lastValidMftStats <- M.stats tx lastValidMft

            uriToUriKeyStat <- M.stats tx uriToUriKey
            uriKeyToUriStat <- M.stats tx uriKeyToUri
            uriKeyToObjectKeyStat <- MM.stats tx urlKeyToObjectKey
            objectKeyToUrlKeysStat <- M.stats tx objectKeyToUrlKeys

            objectInsertedByStats <- M.stats tx objectInsertedBy
            objectValidatedByStats <- M.stats tx objectValidatedBy            
            pure RpkiObjectStats {..}

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
totalSpace stats = 
    let SStats {..} = totalStats stats
    in statKeyBytes + statValueBytes


totalStats :: DBStats -> SStats
totalStats DBStats {..} = 
       taStats 
    <> (let RepositoryStats{..} = repositoryStats 
        in rrdpStats <> rsyncStats <> lastSStats) 
    <> (let RpkiObjectStats {..} = rpkiObjectStats
        in objectsStats <> mftByAKIStats 
        <> objectInsertedByStats <> objectValidatedByStats 
        <> hashToKeyStats <> lastValidMftStats)
    <> resultsStats vResultStats 
    <> vrpStats 
    <> versionStats 
    <> sequenceStats


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

