{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLabels      #-}

module RPKI.Store.Database where

import           Control.Concurrent.STM   (atomically)
import           Control.Exception.Lifted
import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Reader     (ask)
import           Data.IORef.Lifted
import           Data.Foldable            (for_)
import           Data.Generics.Product.Typed

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as C8
import qualified Data.List                as List
import           Data.Maybe               (catMaybes)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Map.Strict          as Map
import qualified Data.Hashable            as H
import           Data.Text.Encoding       (encodeUtf8)
import           Data.Typeable

import           GHC.Generics
import           Text.Read

import           RPKI.Config              (Size)
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))
import           RPKI.TAL
import           RPKI.RRDP.Types
import           RPKI.SLURM.Types
import           RPKI.Repository

import qualified RPKI.Store.Base.Map      as M
import qualified RPKI.Store.Base.MultiMap as MM
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import           RPKI.Store.Sequence
import           RPKI.Store.Types

import           RPKI.Parallel
import           RPKI.Util                (increment, ifJustM)

import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Time

-- This one is to be changed manually whenever 
-- any of the serialisable/serialized types become incompatible.
-- 
-- It is brittle and inconvenient, but so far seems to be 
-- the only realistic option.
currentDatabaseVersion :: Integer
currentDatabaseVersion = 1

-- All of the stores of the application in one place
data DB s = DB {
    taStore          :: TAStore s, 
    repositoryStore  :: RepositoryStore s, 
    objectStore      :: RpkiObjectStore s,
    validationsStore :: ValidationsStore s,    
    vrpStore         :: VRPStore s,
    versionStore     :: VersionStore s,
    metricStore      :: MetricStore s,
    slurmStore       :: SlurmStore s,
    jobStore         :: JobStore s,
    sequences        :: SequenceMap s,
    metadataStore    :: MetadataStore s
} deriving stock (Generic)

instance Storage s => WithStorage s (DB s) where
    storage DB {..} = storage taStore


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
        certBySKI      :: SMap "cert-by-ski" s SKI ObjectKey,    

        objectInsertedBy  :: SMap "object-inserted-by" s ObjectKey WorldVersion,
        objectValidatedBy :: SMap "object-validated-by" s ObjectKey WorldVersion,

        -- Object URL mapping
        uriToUriKey    :: SMap "uri-to-uri-key" s SafeUrlAsKey UrlKey,
        uriKeyToUri    :: SMap "uri-key-to-uri" s UrlKey RpkiURL,

        urlKeyToObjectKey  :: SMultiMap "uri-to-object-key" s UrlKey ObjectKey,
        objectKeyToUrlKeys :: SMap "object-key-to-uri" s ObjectKey [UrlKey]
    } 
    deriving stock (Generic, Typeable)


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

newtype MetricStore s = MetricStore {
    metrics :: SMap "metrics" s WorldVersion RawMetric    
}

instance Storage s => WithStorage s (MetricStore s) where
    storage (MetricStore s) = storage s


-- | VRP store
newtype VRPStore s = VRPStore {    
    vrps :: SMap "vrps" s WorldVersion Vrps
}

instance Storage s => WithStorage s (VRPStore s) where
    storage (VRPStore s) = storage s


-- Version store
newtype VersionStore s = VersionStore {
    versions :: SMap "versions" s WorldVersion VersionState
}

instance Storage s => WithStorage s (VersionStore s) where
    storage (VersionStore s) = storage s


newtype SlurmStore s = SlurmStore {
    slurms :: SMap "slurms" s WorldVersion Slurm
}

data RepositoryStore s = RepositoryStore {
    rrdpS  :: SMap "rrdp-repositories" s RrdpURL RrdpRepository,
    rsyncS :: SMap "rsync-repositories" s RsyncHost RsyncNodeNormal,
    lastS  :: SMap "last-fetch-success" s RpkiURL FetchEverSucceeded
}

instance Storage s => WithStorage s (RepositoryStore s) where
    storage (RepositoryStore s _ _) = storage s

newtype JobStore s = JobStore {
    jobs :: SMap "jobs" s Text Instant
}

newtype MetadataStore s = MetadataStore {
    metadata :: SMap "metadata" s Text Text
}


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
getByUri tx store@RpkiObjectStore {..} uri = liftIO $
    M.get tx uriToUriKey (makeSafeUrl uri) >>= \case 
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
putObject tx RpkiObjectStore {..} StorableObject {..} wv = liftIO $ do
    let h = getHash object
    exists <- M.exists tx hashToKey h    
    unless exists $ do          
        SequenceValue k <- nextValue tx keys
        let objectKey = ObjectKey $ ArtificialKey k
        M.put tx hashToKey h objectKey
        M.put tx objects objectKey storable                           
        M.put tx objectInsertedBy objectKey wv
        case object of
            CerRO c -> 
                M.put tx certBySKI (getSKI c) objectKey
            MftRO mft -> 
                for_ (getAKI object) $ \aki' ->
                    MM.put tx mftByAKI aki' (objectKey, getMftTimingMark mft)
            _ -> pure ()        


linkObjectToUrl :: (MonadIO m, Storage s) => 
                Tx s 'RW 
                -> RpkiObjectStore s 
                -> RpkiURL
                -> Hash
                -> m ()
linkObjectToUrl tx RpkiObjectStore {..} rpkiURL hash = liftIO $ do    
    let safeUrl = makeSafeUrl rpkiURL
    ifJustM (M.get tx hashToKey hash) $ \objectKey -> do        
        z <- M.get tx uriToUriKey safeUrl 
        urlKey <- maybe (saveUrl safeUrl) pure z                
        
        M.get tx objectKeyToUrlKeys objectKey >>= \case 
            Nothing -> do 
                M.put tx objectKeyToUrlKeys objectKey [urlKey]
                MM.put tx urlKeyToObjectKey urlKey objectKey
            Just existingUrlKeys -> 
                when (urlKey `notElem` existingUrlKeys) $ do 
                    M.put tx objectKeyToUrlKeys objectKey (urlKey : existingUrlKeys)
                    MM.put tx urlKeyToObjectKey urlKey objectKey
  where
    saveUrl safeUrl = do 
        SequenceValue k <- nextValue tx keys
        let urlKey = UrlKey $ ArtificialKey k
        M.put tx uriToUriKey safeUrl urlKey
        M.put tx uriKeyToUri urlKey rpkiURL            
        pure urlKey


hashExists :: (MonadIO m, Storage s) => 
            Tx s mode -> RpkiObjectStore s -> Hash -> m Bool
hashExists tx store h = liftIO $ M.exists tx (hashToKey store) h


deleteObject :: (MonadIO m, Storage s) => Tx s 'RW -> RpkiObjectStore s -> Hash -> m ()
deleteObject tx store@RpkiObjectStore {..} h = liftIO $
    ifJustM (M.get tx hashToKey h) $ \objectKey ->             
        ifJustM (getObjectByKey tx store objectKey) $ \ro -> do 
            M.delete tx objects objectKey
            M.delete tx objectInsertedBy objectKey        
            M.delete tx objectValidatedBy objectKey        
            M.delete tx hashToKey h        
            case ro of 
                CerRO c -> M.delete tx certBySKI (getSKI c)
                _       -> pure ()
            ifJustM (M.get tx objectKeyToUrlKeys objectKey) $ \urlKeys -> do 
                M.delete tx objectKeyToUrlKeys objectKey            
                forM_ urlKeys $ \urlKey ->
                    MM.delete tx urlKeyToObjectKey urlKey objectKey                
            
            for_ (getAKI ro) $ \aki' -> do 
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
                Just (Located loc (MftRO mft)) -> Just $ Located loc mft
                _                              -> Nothing
  where
    chooseLatest latest _ (k, timingMark) = 
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
    for_ k $ M.put tx lastValidMft aki


getLatestValidMftByAKI :: (MonadIO m, Storage s) => 
                        Tx s mode -> RpkiObjectStore s -> AKI -> m (Maybe (Located MftObject))
getLatestValidMftByAKI tx store@RpkiObjectStore {..} aki = liftIO $
    M.get tx lastValidMft aki >>= \case 
        Nothing -> pure Nothing 
        Just k -> do 
            o <- getLocatedByKey tx store k
            pure $! case o of 
                Just (Located loc (MftRO mft)) -> Just $ Located loc mft
                _                              -> Nothing


getBySKI :: (MonadIO m, Storage s) => Tx s mode -> DB s -> SKI -> m (Maybe (Located CerObject))
getBySKI tx DB { objectStore = store@RpkiObjectStore {..} } ski = liftIO $ runMaybeT $ do 
    objectKey <- MaybeT $ M.get tx certBySKI ski
    located   <- MaybeT $ getLocatedByKey tx store objectKey
    pure $ located & #payload %~ (\(CerRO c) -> c)

-- TA store functions

putTA :: (MonadIO m, Storage s) => Tx s 'RW -> TAStore s -> StorableTA -> m ()
putTA tx (TAStore s) ta = liftIO $ M.put tx s (getTaName $ tal ta) ta

getTA :: (MonadIO m, Storage s) => Tx s mode -> TAStore s -> TaName -> m (Maybe StorableTA)
getTA tx (TAStore s) name = liftIO $ M.get tx s name

getTAs :: (MonadIO m, Storage s) => Tx s mode -> TAStore s -> m [(TaName, StorableTA)]
getTAs tx (TAStore s) = liftIO $ M.all tx s

putValidations :: (MonadIO m, Storage s) => 
            Tx s 'RW -> DB s -> WorldVersion -> Validations -> m ()
putValidations tx DB { validationsStore = ValidationsStore s } wv validations = liftIO $ M.put tx s wv validations

validationsForVersion :: (MonadIO m, Storage s) => 
                        Tx s mode -> ValidationsStore s -> WorldVersion -> m (Maybe Validations)
validationsForVersion tx ValidationsStore {..} wv = liftIO $ M.get tx results wv

deleteValidations :: (MonadIO m, Storage s) => 
                Tx s 'RW -> DB s -> WorldVersion -> m ()
deleteValidations tx DB { validationsStore = ValidationsStore {..} } wv = 
    liftIO $ M.delete tx results wv
                 

getVrps :: (MonadIO m, Storage s) => 
            Tx s mode -> DB s -> WorldVersion -> m (Maybe Vrps)
getVrps tx DB { vrpStore = VRPStore vrpMap } wv = 
    liftIO $ M.get tx vrpMap wv    

deleteVRPs :: (MonadIO m, Storage s) => 
            Tx s 'RW -> DB s -> WorldVersion -> m ()
deleteVRPs tx DB { vrpStore = VRPStore vrpMap } wv = liftIO $ M.delete tx vrpMap wv

putVrps :: (MonadIO m, Storage s) => 
            Tx s 'RW -> DB s -> Vrps -> WorldVersion -> m ()
putVrps tx DB { vrpStore = VRPStore vrpMap } vrps worldVersion = 
    liftIO $ M.put tx vrpMap worldVersion vrps    

putVersion :: (MonadIO m, Storage s) => 
        Tx s 'RW -> DB s -> WorldVersion -> VersionState -> m ()
putVersion tx DB { versionStore = VersionStore s } wv versionState = 
    liftIO $ M.put tx s wv versionState

allVersions :: (MonadIO m, Storage s) => 
            Tx s mode -> DB s -> m [(WorldVersion, VersionState)]
allVersions tx DB { versionStore = VersionStore s } = liftIO $ M.all tx s

deleteVersion :: (MonadIO m, Storage s) => 
        Tx s 'RW -> DB s -> WorldVersion -> m ()
deleteVersion tx DB { versionStore = VersionStore s } wv = liftIO $ M.delete tx s wv


completeWorldVersion :: Storage s => 
                        Tx s 'RW -> DB s -> WorldVersion -> IO ()
completeWorldVersion tx database worldVersion =    
    putVersion tx database worldVersion $ VersionState "stub"


putMetrics :: (MonadIO m, Storage s) => 
            Tx s 'RW -> DB s -> WorldVersion -> RawMetric -> m ()
putMetrics tx DB { metricStore = MetricStore s } wv appMetric = 
    liftIO $ M.put tx s wv appMetric

metricsForVersion :: (MonadIO m, Storage s) => 
                    Tx s mode -> MetricStore s -> WorldVersion -> m (Maybe RawMetric)
metricsForVersion tx MetricStore {..} wv = liftIO $ M.get tx metrics wv    

deleteMetrics :: (MonadIO m, Storage s) => 
        Tx s 'RW -> DB s -> WorldVersion -> m ()
deleteMetrics tx DB { metricStore = MetricStore s } wv = liftIO $ M.delete tx s wv

putSlurm :: (MonadIO m, Storage s) => 
            Tx s 'RW -> DB s -> WorldVersion -> Slurm -> m ()
putSlurm tx DB { slurmStore = SlurmStore s } wv slurm = liftIO $ M.put tx s wv slurm

slurmForVersion :: (MonadIO m, Storage s) => 
                    Tx s mode -> DB s -> WorldVersion -> m (Maybe Slurm)
slurmForVersion tx DB { slurmStore = SlurmStore s } wv = liftIO $ M.get tx s wv    

deleteSlurms :: (MonadIO m, Storage s) => 
        Tx s 'RW -> DB s -> WorldVersion -> m ()
deleteSlurms tx DB { slurmStore = SlurmStore s } wv = liftIO $ M.delete tx s wv


updateRrdpMeta :: (MonadIO m, Storage s) =>
                Tx s 'RW -> RepositoryStore s -> (SessionId, RrdpSerial) -> RrdpURL -> m ()
updateRrdpMeta tx RepositoryStore {..} meta url = liftIO $ do
    z <- M.get tx rrdpS url
    for_ z $ \r -> M.put tx rrdpS url (r { rrdpMeta = Just meta })


applyChangeSet :: (MonadIO m, Storage s) =>
                Tx s 'RW ->
                DB s ->
                ChangeSet ->
                m ()
applyChangeSet tx DB { repositoryStore = RepositoryStore {..}} 
                  (ChangeSet rrdpChanges rsyncChanges lastSucceded) = liftIO $ do
    -- Do the Remove first and only then Put
    let (rrdpPuts, rrdpRemoves) = separate rrdpChanges

    for_ rrdpRemoves $ \RrdpRepository{..} -> M.delete tx rrdpS uri
    for_ rrdpPuts $ \r@RrdpRepository{..}  -> M.put tx rrdpS uri r

    let (rsyncPuts, rsyncRemoves) = separate rsyncChanges

    for_ rsyncRemoves $ \(uri', _) -> M.delete tx rsyncS uri'
    for_ rsyncPuts $ uncurry (M.put tx rsyncS)

    let (lastSPuts, lastSRemoves) = separate lastSucceded
    for_ lastSRemoves $ \(uri', _) -> M.delete tx lastS uri'
    for_ lastSPuts $ uncurry (M.put tx lastS)
  where
    separate = foldr f ([], [])
      where
        f (Put r)    (ps, rs) = (r : ps, rs)
        f (Remove r) (ps, rs) = (ps, r : rs)

getPublicationPoints :: (MonadIO m, Storage s) =>
                        Tx s mode -> DB s -> m PublicationPoints
getPublicationPoints tx DB { repositoryStore = RepositoryStore {..}} = liftIO $ do
    rrdps <- M.all tx rrdpS
    rsyns <- M.all tx rsyncS
    lasts <- M.all tx lastS
    pure $ PublicationPoints
            (RrdpMap $ Map.fromList rrdps)
            (RsyncTree $ Map.fromList rsyns)
            (EverSucceededMap $ Map.fromList lasts)

savePublicationPoints :: (MonadIO m, Storage s) =>
                        Tx s 'RW -> DB s -> PublicationPoints -> m ()
savePublicationPoints tx db newPPs' = do
    ppsInDb <- getPublicationPoints tx db
    let changes = changeSet ppsInDb newPPs'
    applyChangeSet tx db changes


setJobCompletionTime :: (MonadIO m, Storage s) => 
            Tx s 'RW -> DB s -> Text -> Instant -> m ()
setJobCompletionTime tx DB { jobStore = JobStore s } job t = liftIO $ M.put tx s job t

allJobs :: (MonadIO m, Storage s) => 
            Tx s mode -> DB s -> m [(Text, Instant)]
allJobs tx DB { jobStore = JobStore s } = liftIO $ M.all tx s


databaseVersionKey :: Text
databaseVersionKey = "database-version"

getDatabaseVersion :: (MonadIO m, Storage s) => 
                    Tx s mode -> DB s -> m (Maybe Integer)
getDatabaseVersion tx DB { metadataStore = MetadataStore s } = 
    liftIO $ runMaybeT $ do 
        v <- MaybeT $ M.get tx s databaseVersionKey
        MaybeT $ pure $ readMaybe $ Text.unpack v        

saveCurrentDatabaseVersion :: (MonadIO m, Storage s) => 
                            Tx s 'RW -> DB s -> m ()
saveCurrentDatabaseVersion tx DB { metadataStore = MetadataStore s } = 
    liftIO $ M.put tx s databaseVersionKey (Text.pack $ show currentDatabaseVersion)

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
                    for_ z $ \cutoffVersion -> 
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
                                M.delete tx (uriToUriKey objectStore) (makeSafeUrl url)
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
    
        let toDelete versions = 
                case versions of            
                    []       -> []                
                    finished -> 
                        -- delete all too old except for the last finished one    
                        let lastFinished = List.maximum finished 
                        in filter ( /= lastFinished) $ filter tooOld versions

        rwTx database $ \tx -> do 
            versions <- map fst <$> allVersions tx database    
            let toDel = toDelete versions
            forM_ toDel $ \worldVersion -> do
                deleteVersion tx database worldVersion
                deleteValidations tx database worldVersion
                deleteVRPs tx database worldVersion            
                deleteMetrics tx database worldVersion
                deleteSlurms tx database worldVersion
        
            pure $! List.length toDel


-- | Find the latest completed world version 
-- 
getLastCompletedVersion :: (Storage s) => 
                        DB s -> Tx s 'RO -> IO (Maybe WorldVersion)
getLastCompletedVersion database tx = do 
        vs <- map fst <$> allVersions tx database
        pure $! case vs of         
            []  -> Nothing
            vs' -> Just $ maximum vs'


getLatestVRPs :: Storage s => DB s -> IO (Maybe Vrps)
getLatestVRPs db = 
    roTx db $ \tx ->        
        runMaybeT $ do 
            version <- MaybeT $ getLastCompletedVersion db tx
            MaybeT $ getVrps tx db version


getTotalDbStats :: (MonadIO m, Storage s) => 
                    DB s -> m TotalDBStats
getTotalDbStats db = do 
    dbStats <- getDbStats db
    let total = totalStats dbStats
    pure TotalDBStats {..}

-- Compute database stats
getDbStats :: (MonadIO m, Storage s) => 
              DB s -> m DBStats
getDbStats db@DB {..} = liftIO $ roTx db $ \tx -> do 
    taStats         <- let TAStore sm = taStore in M.stats tx sm
    repositoryStats <- repositoryStats' tx
    rpkiObjectStats <- rpkiObjectStats' tx
    vResultStats    <- vResultStats' tx
    vrpStats        <- let VRPStore sm = vrpStore in M.stats tx sm
    metricsStats    <- let MetricStore sm = metricStore in M.stats tx sm
    versionStats    <- let VersionStore sm = versionStore in M.stats tx sm
    sequenceStats   <- M.stats tx sequences
    slurmStats      <- let SlurmStore sm = slurmStore in M.stats tx sm
    pure DBStats {..}
  where
    rpkiObjectStats' tx = do 
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

    repositoryStats' tx = 
        let RepositoryStore {..} = repositoryStore
        in RepositoryStats <$>
            M.stats tx rrdpS <*>
            M.stats tx rsyncS <*>
            M.stats tx lastS

    vResultStats' tx = 
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

-- TODO This is a terribly slow implementation, use
-- drop-based implemntation.
emptyDBMaps :: (MonadIO m, Storage s) => 
                Tx s 'RW -> DB s -> m ()
emptyDBMaps tx DB {..} = liftIO $ do     
    emptyRepositoryStore repositoryStore    
    emptyObjectStore objectStore    
    M.erase tx $ results validationsStore
    M.erase tx $ vrps vrpStore
    M.erase tx $ versions versionStore
    M.erase tx $ metrics metricStore
    M.erase tx $ slurms slurmStore
    M.erase tx $ jobs jobStore    
  where
    emptyObjectStore RpkiObjectStore {..} = do   
        M.erase tx objects
        M.erase tx hashToKey
        MM.erase tx mftByAKI
        M.erase tx lastValidMft
        M.erase tx certBySKI
        M.erase tx objectInsertedBy
        M.erase tx objectValidatedBy
        M.erase tx uriToUriKey
        M.erase tx uriKeyToUri
        MM.erase tx urlKeyToObjectKey
        M.erase tx objectKeyToUrlKeys

    emptyRepositoryStore RepositoryStore {..} = do   
        M.erase tx rrdpS
        M.erase tx rsyncS
        M.erase tx lastS



-- Utilities to have storage transaction in ValidatorT monad.

roAppTx :: (Storage s, WithStorage s ws) => 
            ws -> (Tx s 'RO -> ValidatorT IO a) -> ValidatorT IO a 
roAppTx ws f = appTx ws f roTx    

rwAppTx :: (Storage s, WithStorage s ws) => 
            ws -> (Tx s 'RW -> ValidatorT IO a) -> ValidatorT IO a
rwAppTx ws f = appTx ws f rwTx


appTx :: (Storage s, WithStorage s ws) => 
        ws 
        -> (Tx s mode -> ValidatorT IO a) 
        -> (ws 
            -> (Tx s mode -> IO (Either AppError a, ValidationState))
            -> IO (Either AppError a, ValidationState)) 
        -> ValidatorT IO a
appTx ws f txF = do
    env <- ask        
    embedValidatorT $ transaction env `catch` 
                    (\(TxRollbackException e vs) -> pure (Left e, vs))
  where
    transaction env = txF ws $ \tx -> do 
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


-- Utils of different sorts

data TxRollbackException = TxRollbackException AppError ValidationState
    deriving stock (Show, Eq, Ord, Generic)

instance Exception TxRollbackException

-- | URLs can potentially be much larger than 512 bytes that are allowed as LMDB
-- keys. So we 
--     - calculate hash 
--     - truncate the URL so that that truncated version + hash fit into 512 bytes.
--     - use "truncated URL + hash" as a key
makeSafeUrl :: RpkiURL -> SafeUrlAsKey
makeSafeUrl u = 
    let         
        maxLmdbKeyBytes = 512
        URI urlText = getURL u        
        hashBytes = show $ H.hash urlText
        -- We only keep the first "512 - hash bytes" of the URL
        maxTxtUrlBytes = maxLmdbKeyBytes - length hashBytes

        bsUrlFull = encodeUtf8 urlText
        bsUrlKey = BS.concat [
                        BS.take maxTxtUrlBytes bsUrlFull,
                        C8.pack hashBytes
                   ]
    in SafeUrlAsKey $ toShortBS bsUrlKey