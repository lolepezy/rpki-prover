{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE DeriveAnyClass        #-}

module RPKI.Store.Database where

import           Control.Exception.Lifted
import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Reader     (ask)
import           Data.Foldable            (for_)

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as C8
import qualified Data.List                as List
import           Data.Maybe               (catMaybes, fromMaybe)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import qualified Data.Hashable            as H
import           Data.Text.Encoding       (encodeUtf8)
import           Data.Generics.Product.Types
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
import           RPKI.Store.Base.Serialisation
import           RPKI.Store.Sequence
import           RPKI.Store.Types

import           RPKI.Util                (ifJustM)

import           RPKI.AppMonad
import           RPKI.AppState
import           RPKI.AppTypes
import           RPKI.RTR.Types
import           RPKI.Time

-- This one is to be changed manually whenever 
-- any of the serialisable/serialized types become incompatible.
-- 
-- It is brittle and inconvenient, but so far seems to be 
-- the only realistic option.
currentDatabaseVersion :: Integer
currentDatabaseVersion = 19


databaseVersionKey :: Text
databaseVersionKey = "database-version"

lastValidMftKey :: Text
lastValidMftKey = "last-valid-mft"

data EraseWrapper s where 
    EraseWrapper :: forall t s . (Storage s, CanErase s t) => t -> EraseWrapper s

-- All of the stores of the application in one place
data DB s = DB {
    taStore          :: TAStore s, 
    repositoryStore  :: RepositoryStore s, 
    objectStore      :: RpkiObjectStore s,    
    validationsStore :: ValidationsStore s,    
    vrpStore         :: VRPStore s,
    aspaStore        :: AspaStore s,
    gbrStore         :: GbrStore s,
    bgpStore         :: BgpStore s,
    versionStore     :: VersionStore s,
    metricStore      :: MetricStore s,
    slurmStore       :: SlurmStore s,
    jobStore         :: JobStore s,
    sequences        :: SequenceMap s,
    metadataStore    :: MetadataStore s,
    erasables        :: [EraseWrapper s]
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
        objects        :: SMap "objects" s ObjectKey (Compressed (StorableObject RpkiObject)),
        hashToKey      :: SMap "hash-to-key" s Hash ObjectKey,    
        mftByAKI       :: SMultiMap "mft-by-aki" s AKI (ObjectKey, MftTimingMark),
        lastValidMfts  :: SMap "last-valid-mfts" s Text (Compressed (Map AKI ObjectKey)),    
        certBySKI      :: SMap "cert-by-ski" s SKI ObjectKey,    

        objectInsertedBy   :: SMap "object-inserted-by" s ObjectKey WorldVersion,
        validatedByVersion :: SMap "validated-by-version" s WorldVersion (Compressed (Set.Set ObjectKey)),

        -- Object URL mapping
        uriToUriKey    :: SMap "uri-to-uri-key" s SafeUrlAsKey UrlKey,
        uriKeyToUri    :: SMap "uri-key-to-uri" s UrlKey RpkiURL,

        urlKeyToObjectKey  :: SMultiMap "uri-to-object-key" s UrlKey ObjectKey,
        objectKeyToUrlKeys :: SMap "object-key-to-uri" s ObjectKey [UrlKey],

        objectBriefs       :: SMap "object-briefs" s ObjectKey (Compressed EEBrief)
    } 
    deriving stock (Generic)


instance Storage s => WithStorage s (RpkiObjectStore s) where
    storage = storage . objects


-- | TA Store 
newtype TAStore s = TAStore { 
        tas :: SMap "trust-anchors" s TaName StorableTA
    }
    deriving stock (Generic)

instance Storage s => WithStorage s (TAStore s) where
    storage (TAStore s) = storage s

newtype ValidationsStore s = ValidationsStore { 
        results :: SMap "validations" s WorldVersion (Compressed Validations) 
    }
    deriving stock (Generic)

instance Storage s => WithStorage s (ValidationsStore s) where
    storage (ValidationsStore s) = storage s

newtype MetricStore s = MetricStore {
        metrics :: SMap "metrics" s WorldVersion (Compressed RawMetric)
    }    
    deriving stock (Generic)

instance Storage s => WithStorage s (MetricStore s) where
    storage (MetricStore s) = storage s


-- | VRP store
newtype VRPStore s = VRPStore {    
        vrps :: SMap "vrps" s WorldVersion (Compressed Vrps)
    }
    deriving stock (Generic)

-- | ASPA store
newtype AspaStore s = AspaStore {    
        aspas :: SMap "aspas" s WorldVersion (Compressed (Set.Set Aspa))
    } 
    deriving stock (Generic)

-- | GBR store
newtype GbrStore s = GbrStore {    
        gbrs :: SMap "gbrs" s WorldVersion (Compressed (Set.Set (Hash, Gbr)))
    } 
    deriving stock (Generic)

-- | BGP certificate store
newtype BgpStore s = BgpStore {    
        bgps :: SMap "bgps" s WorldVersion (Compressed (Set.Set BGPSecPayload))
    }
    deriving stock (Generic)

instance Storage s => WithStorage s (VRPStore s) where
    storage (VRPStore s) = storage s


-- Version store
newtype VersionStore s = VersionStore {
        versions :: SMap "versions" s WorldVersion VersionKind
    }
    deriving stock (Generic)

instance Storage s => WithStorage s (VersionStore s) where
    storage (VersionStore s) = storage s


newtype SlurmStore s = SlurmStore {
        slurms :: SMap "slurms" s WorldVersion (Compressed Slurm)
    }
    deriving stock (Generic)

data RepositoryStore s = RepositoryStore {
        rrdpS  :: SMap "rrdp-repositories" s RrdpURL RrdpRepository,
        rsyncS :: SMap "rsync-repositories" s RsyncHost RsyncNodeNormal,
        lastS  :: SMap "last-fetch-success" s RpkiURL FetchEverSucceeded
    }
    deriving stock (Generic)

instance Storage s => WithStorage s (RepositoryStore s) where
    storage (RepositoryStore s _ _) = storage s

newtype JobStore s = JobStore {
        jobs :: SMap "jobs" s Text Instant
    }
    deriving stock (Generic)

newtype MetadataStore s = MetadataStore {
        metadata :: SMap "metadata" s Text Text
    }
    deriving stock (Generic)


getByHash :: (MonadIO m, Storage s) => 
            Tx s mode -> DB s -> Hash -> m (Maybe (Located RpkiObject))
getByHash tx db h = ((^. #object) <$>) <$> getKeyedByHash tx db h    

getKeyedByHash :: (MonadIO m, Storage s) => 
              Tx s mode -> DB s -> Hash -> m (Maybe (Keyed (Located RpkiObject)))
getKeyedByHash tx db@DB { objectStore = RpkiObjectStore {..} } h = liftIO $ runMaybeT $ do 
    objectKey <- MaybeT $ M.get tx hashToKey h
    z         <- MaybeT $ getLocatedByKey tx db objectKey
    pure $ Keyed z objectKey
            
getByUri :: (MonadIO m, Storage s) => 
            Tx s mode -> DB s -> RpkiURL -> m [Located RpkiObject]
getByUri tx db@DB { objectStore = RpkiObjectStore {..} } uri = liftIO $
    M.get tx uriToUriKey (makeSafeUrl uri) >>= \case 
        Nothing -> pure []
        Just uriKey ->         
            fmap catMaybes $ 
                MM.allForKey tx urlKeyToObjectKey uriKey >>=
                mapM (getLocatedByKey tx db)


getObjectByKey :: (MonadIO m, Storage s) => 
                Tx s mode -> DB s -> ObjectKey -> m (Maybe RpkiObject)
getObjectByKey tx DB { objectStore = RpkiObjectStore {..} } k = liftIO $
    fmap (\(Compressed StorableObject{..}) -> object) <$> M.get tx objects k    

getLocatedByKey :: (MonadIO m, Storage s) => 
                Tx s mode -> DB s -> ObjectKey -> m (Maybe (Located RpkiObject))
getLocatedByKey tx db k = liftIO $ runMaybeT $ do     
    object    <- MaybeT $ getObjectByKey tx db k    
    locations <- MaybeT $ getLocationsByKey tx db k                    
    pure $ Located locations object


getLocationsByKey :: (MonadIO m, Storage s) => 
                Tx s mode -> DB s -> ObjectKey -> m (Maybe Locations)
getLocationsByKey tx DB { objectStore = RpkiObjectStore {..} } k = liftIO $ runMaybeT $ do         
    uriKeys   <- MaybeT $ M.get tx objectKeyToUrlKeys k
    locations <- MaybeT 
                    $ (toNESet . catMaybes <$>)
                    $ mapM (M.get tx uriKeyToUri) uriKeys             
    pure $ Locations locations

putObject :: (MonadIO m, Storage s) => 
            Tx s 'RW 
            -> RpkiObjectStore s 
            -> StorableObject RpkiObject
            -> WorldVersion -> m ()
putObject tx RpkiObjectStore {..} so@StorableObject {..} wv = liftIO $ do
    let h = getHash object
    exists <- M.exists tx hashToKey h    
    unless exists $ do          
        SequenceValue k <- nextValue tx keys
        let objectKey = ObjectKey $ ArtificialKey k
        M.put tx hashToKey h objectKey
        M.put tx objects objectKey (Compressed so)
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


deleteObject :: (MonadIO m, Storage s) => Tx s 'RW -> DB s -> Hash -> m ()
deleteObject tx db@DB { objectStore = RpkiObjectStore {..} } h = liftIO $
    ifJustM (M.get tx hashToKey h) $ \objectKey ->             
        ifJustM (getObjectByKey tx db objectKey) $ \ro -> do 
            M.delete tx objects objectKey
            M.delete tx objectInsertedBy objectKey        
            M.delete tx hashToKey h        
            M.delete tx objectBriefs objectKey    
            case ro of 
                CerRO c -> M.delete tx certBySKI (getSKI c)
                _       -> pure ()
            ifJustM (M.get tx objectKeyToUrlKeys objectKey) $ \urlKeys -> do 
                M.delete tx objectKeyToUrlKeys objectKey            
                forM_ urlKeys $ \urlKey ->
                    MM.delete tx urlKeyToObjectKey urlKey objectKey                
            
            for_ (getAKI ro) $ \aki' -> 
                case ro of
                    MftRO mft -> MM.delete tx mftByAKI aki' (objectKey, getMftTimingMark mft)
                    _         -> pure ()        


findLatestMftByAKI :: (MonadIO m, Storage s) => 
                    Tx s mode -> DB s -> AKI -> m (Maybe (Keyed (Located MftObject)))
findLatestMftByAKI tx db@DB { objectStore = RpkiObjectStore {..} } aki' = liftIO $ 
    MM.foldS tx mftByAKI aki' chooseLatest Nothing >>= \case
        Nothing     -> pure Nothing
        Just (k, _) -> do 
            o <- getLocatedByKey tx db k
            pure $! case o of 
                Just (Located loc (MftRO mft)) -> Just $ Keyed (Located loc mft) k
                _                              -> Nothing
  where
    chooseLatest latest _ (k, timingMark) = 
        pure $! case latest of 
            Nothing                       -> Just (k, timingMark)
            Just (_, latestMark) 
                | timingMark > latestMark -> Just (k, timingMark)
                | otherwise               -> latest                                  


markAsValidated :: (MonadIO m, Storage s) => 
                    Tx s 'RW -> DB s 
                -> Set.Set Hash 
                -> WorldVersion -> m ()
markAsValidated tx db@DB { objectStore = RpkiObjectStore {..} } hashes worldVersion = liftIO $ do 
    exisingVersions <- validationVersions tx db    
        
    allKeys <- fmap (Set.fromList . catMaybes) 
                $ forM (Set.toList hashes) $ \h -> M.get tx hashToKey h                    

    M.put tx validatedByVersion worldVersion (Compressed allKeys)
    
    case exisingVersions of 
        [] -> pure ()
        _ -> do
            -- This is an optimisation, but a necessary one:
            -- Delete 'validatedKeys' from the previous version if
            -- they are present in the last one. In most cases it
            -- will delete most of the entries.
            let previousVersion = List.maximum exisingVersions 
            ifJustM (M.get tx validatedByVersion previousVersion) $ \(Compressed previousKeys) ->                
                M.put tx validatedByVersion previousVersion $ 
                        Compressed $ previousKeys `Set.difference` allKeys

saveObjectBrief :: (MonadIO m, Storage s) => 
                Tx s 'RW -> DB s -> Hash -> EEBrief -> m ()
saveObjectBrief tx DB {         
        objectStore = RpkiObjectStore {..} } 
    hash brief = liftIO $ ifJustM (M.get tx hashToKey hash) $ \key -> 
        M.put tx objectBriefs key (Compressed brief)


getOjectBrief :: (MonadIO m, Storage s) => 
            Tx s mode -> DB s -> Hash -> m (Maybe (Keyed (Located EEBrief)))
getOjectBrief tx db@DB { objectStore = RpkiObjectStore {..} } h =  
    liftIO $ runMaybeT $ do 
        objectKey <- MaybeT $ M.get tx hashToKey h
        brief <- unCompressed <$> MaybeT (M.get tx objectBriefs objectKey)        
        locations <- MaybeT $ getLocationsByKey tx db objectKey
        pure $ Keyed (Located locations brief) objectKey


-- This is for testing purposes mostly
getAll :: (MonadIO m, Storage s) => Tx s mode -> DB s -> m [Located RpkiObject]
getAll tx db@DB { objectStore = RpkiObjectStore {..} } = liftIO $ do 
    allKeys <- M.keys tx objects
    catMaybes <$> forM allKeys (getLocatedByKey tx db)    


-- | Get something from the manifest that would allow us to judge 
-- which MFT is newer/older.
getMftTimingMark :: MftObject -> MftTimingMark
getMftTimingMark mft = let 
    m = getCMSContent $ cmsPayload mft 
    in MftTimingMark (thisTime m) (nextTime m)


saveLatestValidMfts :: (MonadIO m, Storage s) => 
                        Tx s 'RW -> DB s -> Map AKI ObjectKey -> m ()
saveLatestValidMfts tx DB { objectStore = RpkiObjectStore {..}} valids = liftIO $ do
    z <- M.get tx lastValidMfts lastValidMftKey
    let valids' = case z of 
            Nothing              -> valids
            Just (Compressed vs) -> Map.unionWith (\a _ -> a) valids vs
    M.put tx lastValidMfts lastValidMftKey (Compressed valids')


getLatestValidMfts :: (MonadIO m, Storage s) => 
                        Tx s mode -> DB s -> m (Map AKI ObjectKey)
getLatestValidMfts tx DB { objectStore = RpkiObjectStore {..}} = liftIO $ do
    z <- M.get tx lastValidMfts lastValidMftKey
    pure $ case z of 
        Nothing             -> Map.empty
        Just (Compressed r) -> r


getBySKI :: (MonadIO m, Storage s) => Tx s mode -> DB s -> SKI -> m (Maybe (Located CaCerObject))
getBySKI tx db@DB { objectStore = RpkiObjectStore {..} } ski = liftIO $ runMaybeT $ do 
    objectKey <- MaybeT $ M.get tx certBySKI ski
    located   <- MaybeT $ getLocatedByKey tx db objectKey
    pure $ located & #payload %~ (\(CerRO c) -> c) 

-- TA store functions

saveTA :: (MonadIO m, Storage s) => Tx s 'RW -> DB s -> StorableTA -> m ()
saveTA tx DB { taStore = TAStore s } ta = liftIO $ M.put tx s (getTaName $ tal ta) ta

getTA :: (MonadIO m, Storage s) => Tx s mode -> DB s -> TaName -> m (Maybe StorableTA)
getTA tx DB { taStore = TAStore s } name = liftIO $ M.get tx s name

getTAs :: (MonadIO m, Storage s) => Tx s mode -> DB s -> m [(TaName, StorableTA)]
getTAs tx DB { taStore = TAStore s } = liftIO $ M.all tx s

saveValidations :: (MonadIO m, Storage s) => 
            Tx s 'RW -> DB s -> WorldVersion -> Validations -> m ()
saveValidations tx DB { validationsStore = ValidationsStore s } wv validations = 
    liftIO $ M.put tx s wv (Compressed validations)

validationsForVersion :: (MonadIO m, Storage s) => 
                        Tx s mode -> DB s -> WorldVersion -> m (Maybe Validations)
validationsForVersion tx DB { validationsStore = ValidationsStore {..} } wv = 
    liftIO $ fmap unCompressed <$> M.get tx results wv

deleteValidations :: (MonadIO m, Storage s) => 
                Tx s 'RW -> DB s -> WorldVersion -> m ()
deleteValidations tx DB { validationsStore = ValidationsStore {..} } wv = 
    liftIO $ M.delete tx results wv
                 

getVrps :: (MonadIO m, Storage s) => 
            Tx s mode -> DB s -> WorldVersion -> m (Maybe Vrps)
getVrps tx DB { vrpStore = VRPStore m } wv = liftIO $ fmap unCompressed <$> M.get tx m wv

deleteVRPs :: (MonadIO m, Storage s) => 
            Tx s 'RW -> DB s -> WorldVersion -> m ()
deleteVRPs tx DB { vrpStore = VRPStore vrpMap } wv = liftIO $ M.delete tx vrpMap wv

getAspas :: (MonadIO m, Storage s) => 
            Tx s mode -> DB s -> WorldVersion -> m (Maybe (Set.Set Aspa))
getAspas tx DB { aspaStore = AspaStore m } wv = 
    liftIO $ fmap unCompressed <$> M.get tx m wv    

deleteAspas :: (MonadIO m, Storage s) => 
            Tx s 'RW -> DB s -> WorldVersion -> m ()
deleteAspas tx DB { aspaStore = AspaStore m } wv = liftIO $ M.delete tx m wv

saveAspas :: (MonadIO m, Storage s) => 
            Tx s 'RW -> DB s -> Set.Set Aspa -> WorldVersion -> m ()
saveAspas tx DB { aspaStore = AspaStore m } aspas worldVersion = 
    liftIO $ M.put tx m worldVersion (Compressed aspas)

getGbrs :: (MonadIO m, Storage s) => 
            Tx s mode -> DB s -> WorldVersion -> m (Maybe (Set.Set (Hash, Gbr)))
getGbrs tx DB { gbrStore = GbrStore m } wv = 
    liftIO $ fmap unCompressed <$> M.get tx m wv    

deleteGbrs :: (MonadIO m, Storage s) => 
            Tx s 'RW -> DB s -> WorldVersion -> m ()
deleteGbrs tx DB { gbrStore = GbrStore m } wv = liftIO $ M.delete tx m wv

saveGbrs :: (MonadIO m, Storage s) => 
            Tx s 'RW -> DB s -> Set.Set (Hash, Gbr) -> WorldVersion -> m ()
saveGbrs tx DB { gbrStore = GbrStore m } gbrs worldVersion = 
    liftIO $ M.put tx m worldVersion (Compressed gbrs)

saveVrps :: (MonadIO m, Storage s) => 
            Tx s 'RW -> DB s -> Vrps -> WorldVersion -> m ()
saveVrps tx DB { vrpStore = VRPStore vrpMap } vrps worldVersion = 
    liftIO $ M.put tx vrpMap worldVersion (Compressed vrps)

saveBgps :: (MonadIO m, Storage s) => 
            Tx s 'RW -> DB s -> Set.Set BGPSecPayload -> WorldVersion -> m ()
saveBgps tx DB { bgpStore = BgpStore bgpMap } bgps worldVersion = 
    liftIO $ M.put tx bgpMap worldVersion (Compressed bgps)

deleteBgps :: (MonadIO m, Storage s) => 
            Tx s 'RW -> DB s -> WorldVersion -> m ()
deleteBgps tx DB { bgpStore = BgpStore bgpMap } wv = liftIO $ M.delete tx bgpMap wv

getBgps :: (MonadIO m, Storage s) => 
            Tx s mode -> DB s -> WorldVersion -> m (Maybe (Set.Set BGPSecPayload))
getBgps tx DB { bgpStore = BgpStore m } wv = 
    liftIO $ fmap unCompressed <$> M.get tx m wv    

saveVersion :: (MonadIO m, Storage s) => 
        Tx s 'RW -> DB s -> WorldVersion -> VersionKind -> m ()
saveVersion tx DB { versionStore = VersionStore s } wv versionState = 
    liftIO $ M.put tx s wv versionState

allVersions :: (MonadIO m, Storage s) => Tx s mode -> DB s -> m [(WorldVersion, VersionKind)]
allVersions tx DB { versionStore = VersionStore s } = liftIO $ M.all tx s

validationVersions :: (MonadIO m, Storage s) => Tx s mode -> DB s -> m [WorldVersion]
validationVersions tx DB { versionStore = VersionStore s } = liftIO $ do 
    z <- M.all tx s
    pure [ wv | (wv, vk) <- z, vk == validationKind ]

deleteVersion :: (MonadIO m, Storage s) => 
        Tx s 'RW -> DB s -> WorldVersion -> m ()
deleteVersion tx DB { versionStore = VersionStore s } wv = liftIO $ M.delete tx s wv

completeWorldVersion :: Storage s => Tx s 'RW -> DB s -> WorldVersion -> IO ()
completeWorldVersion tx database worldVersion =    
    saveVersion tx database worldVersion validationKind

generalWorldVersion :: Storage s => Tx s 'RW -> DB s -> WorldVersion -> IO ()
generalWorldVersion tx database worldVersion =    
    saveVersion tx database worldVersion generalKind

asyncFetchWorldVersion :: Storage s => Tx s 'RW -> DB s -> WorldVersion -> IO ()
asyncFetchWorldVersion tx database worldVersion =    
    saveVersion tx database worldVersion asyncFetchKind


saveMetrics :: (MonadIO m, Storage s) => Tx s 'RW -> DB s -> WorldVersion -> RawMetric -> m ()
saveMetrics tx DB { metricStore = MetricStore s } wv appMetric = 
    liftIO $ M.put tx s wv (Compressed appMetric)

metricsForVersion :: (MonadIO m, Storage s) => Tx s mode -> DB s  -> WorldVersion -> m (Maybe RawMetric)
metricsForVersion tx DB { metricStore = MetricStore {..} } wv = 
    liftIO $ fmap unCompressed <$> M.get tx metrics wv    

deleteMetrics :: (MonadIO m, Storage s) => Tx s 'RW -> DB s -> WorldVersion -> m ()
deleteMetrics tx DB { metricStore = MetricStore s } wv = liftIO $ M.delete tx s wv

saveSlurm :: (MonadIO m, Storage s) => Tx s 'RW -> DB s -> WorldVersion -> Slurm -> m ()
saveSlurm tx DB { slurmStore = SlurmStore s } wv slurm = liftIO $ M.put tx s wv (Compressed slurm)

slurmForVersion :: (MonadIO m, Storage s) => Tx s mode -> DB s -> WorldVersion -> m (Maybe Slurm)
slurmForVersion tx DB { slurmStore = SlurmStore s } wv = 
    liftIO $ fmap unCompressed <$> M.get tx s wv    

deleteSlurms :: (MonadIO m, Storage s) => Tx s 'RW -> DB s -> WorldVersion -> m ()
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

getPublicationPoints :: (MonadIO m, Storage s) => Tx s mode -> DB s -> m PublicationPoints
getPublicationPoints tx DB { repositoryStore = RepositoryStore {..}} = liftIO $ do
    rrdps <- M.all tx rrdpS
    rsyns <- M.all tx rsyncS
    lasts <- M.all tx lastS
    pure $ PublicationPoints
            (RrdpMap $ Map.fromList rrdps)
            (RsyncTree $ Map.fromList rsyns)
            (EverSucceededMap $ Map.fromList lasts)

savePublicationPoints :: (MonadIO m, Storage s) => Tx s 'RW -> DB s -> PublicationPoints -> m ()
savePublicationPoints tx db newPPs' = do
    ppsInDb <- getPublicationPoints tx db
    let changes = changeSet ppsInDb newPPs'
    applyChangeSet tx db changes


setJobCompletionTime :: (MonadIO m, Storage s) => Tx s 'RW -> DB s -> Text -> Instant -> m ()
setJobCompletionTime tx DB { jobStore = JobStore s } job t = liftIO $ M.put tx s job t

allJobs :: (MonadIO m, Storage s) => Tx s mode -> DB s -> m [(Text, Instant)]
allJobs tx DB { jobStore = JobStore s } = liftIO $ M.all tx s

getDatabaseVersion :: (MonadIO m, Storage s) => Tx s mode -> DB s -> m (Maybe Integer)
getDatabaseVersion tx DB { metadataStore = MetadataStore s } = 
    liftIO $ runMaybeT $ do 
        v <- MaybeT $ M.get tx s databaseVersionKey
        MaybeT $ pure $ readMaybe $ Text.unpack v        

saveCurrentDatabaseVersion :: (MonadIO m, Storage s) => Tx s 'RW -> DB s -> m ()
saveCurrentDatabaseVersion tx DB { metadataStore = MetadataStore s } = 
    liftIO $ M.put tx s databaseVersionKey (Text.pack $ show currentDatabaseVersion)


-- More complicated operations

data CleanUpResult = CleanUpResult {
        deletedObjects :: Int,
        keptObjects :: Int,
        deletedURLs :: Int
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)


-- Clean up older version payloads, e.g. VRPs, ASPAs, validation results, etc.
-- 
deleteOldPayloads :: (MonadIO m, Storage s) => 
                    DB s -> 
                    (WorldVersion -> Bool) -> -- ^ function that determines if an object is too old to be in cache
                    m Int
deleteOldPayloads db tooOld = 
    mapException (AppException . storageError) <$> liftIO $ do
        rwTx db $ \tx -> do 
            versions <- validationVersions tx db    
            let toDelete = 
                    case versions of            
                        []       -> []                
                        finished -> 
                            -- delete all too old except for the last finished one    
                            let lastFinished = List.maximum finished 
                            in filter ( /= lastFinished) $ filter tooOld versions            
            forM_ toDelete $ deletePayloads tx db        
            pure $! List.length toDelete


deleteStaleContent :: (MonadIO m, Storage s) => 
                    DB s -> 
                    (WorldVersion -> Bool) -> -- ^ function that determines if an object is too old to be in cache
                    m CleanUpResult
deleteStaleContent db@DB { objectStore = RpkiObjectStore {..} } tooOld = 
    mapException (AppException . storageError) <$> liftIO $ do                

        versions <- roTx db (`validationVersions` db)
        let (toDelete, toKeep) = List.partition tooOld versions
        
        if null toDelete then do 
            kept <- roTx db $ \tx -> M.fold tx hashToKey (\n _ _ -> pure $! n + 1) 0
            pure $ CleanUpResult 0 kept 0
        else do
            rwTx db $ \tx -> do
                -- delete versions and payloads associated with them, 
                -- e.g. VRPs, ASPAs, BGPSec certificatees, etc.
                forM_ toDelete $ \version -> do 
                    deleteVersion tx db version                    
                    deletePayloads tx db version
                    M.delete tx validatedByVersion version
                        
                (deleted, kept) <- deleteStaleObjects tx toKeep
                                                        
                -- Clean up the association between AKI and last valid manifest hash            
                cleanupLatestValidMfts tx

                -- Delete URLs that are now not referred by any object
                deletedUrs <- deleteDanglingUrls db tx

                pure $! CleanUpResult deleted kept deletedUrs
  where

    cleanupLatestValidMfts tx = liftIO $ do
        z <- M.get tx lastValidMfts lastValidMftKey
        for_ z $ \(Compressed valids) -> do
            exisingKeys <- M.fold tx hashToKey (\allKeys _ k -> pure $! k `Set.insert` allKeys) Set.empty
            let existingValids = Map.filter (`Set.member` exisingKeys) valids    
            M.put tx lastValidMfts lastValidMftKey (Compressed existingValids)

    deleteStaleObjects tx versionsToKeep = do 
        -- Set of all objects touched by validation with versions
        -- that are not "too old".
        touchedObjectKeys <- foldM 
            (\allKeys version ->
                M.get tx validatedByVersion version >>= \case                        
                    Nothing                 -> pure $! allKeys
                    Just (Compressed keys') -> pure $! allKeys <> keys')
            mempty 
            versionsToKeep

        -- Objects inserted by validation with version that is not too old.
        -- We want to preseve these objects in the cache even if they are 
        -- never used, they may still be used later. That may happens if
        -- a repository updates a manifest aftert updating its children.
        recentlyInsertedObjectKeys <- M.fold tx objectInsertedBy 
            (\allKeys key version -> 
                pure $! if tooOld version 
                    then allKeys 
                    else key `Set.insert` allKeys) mempty

        let keysToKeep = touchedObjectKeys <> recentlyInsertedObjectKeys            

        -- Everything else must be purged
        hashesToDelete <- M.fold tx hashToKey 
            (\allHashes hash key ->
                if key `Set.member` keysToKeep
                    then pure $! allHashes
                    else pure $! hash `Set.insert` allHashes) mempty 
        
        forM_ hashesToDelete $ deleteObject tx db        
        pure (Set.size hashesToDelete, Set.size keysToKeep)

deleteDanglingUrls :: DB s -> Tx s 'RW -> IO Int
deleteDanglingUrls DB { objectStore = RpkiObjectStore {..} } tx = do 
    referencedUrlKeys <- M.fold tx objectKeyToUrlKeys
            (\allUrlKey _ urlKeys -> pure $! Set.fromList urlKeys <> allUrlKey)
            mempty

    urlsToDelete <- M.fold tx uriKeyToUri 
            (\result urlKey url -> 
                pure $! 
                    if urlKey `Set.notMember` referencedUrlKeys
                        then (urlKey, url) : result
                        else result)
            mempty


    forM_ urlsToDelete $ \(urlKey, url) -> do 
        M.delete tx uriKeyToUri urlKey
        M.delete tx uriToUriKey (makeSafeUrl url)           

    pure $ length urlsToDelete


deletePayloads :: (MonadIO m, Storage s) => 
            Tx s 'RW -> DB s -> WorldVersion -> m ()
deletePayloads tx db worldVersion = do    
    deleteValidations tx db worldVersion
    deleteAspas tx db worldVersion            
    deleteGbrs tx db worldVersion            
    deleteBgps tx db worldVersion            
    deleteVRPs tx db worldVersion            
    deleteMetrics tx db worldVersion
    deleteSlurms tx db worldVersion


-- | Find the latest completed world version 
-- 
getLastCompletedVersion :: (Storage s) => DB s -> Tx s 'RO -> IO (Maybe WorldVersion)
getLastCompletedVersion database tx = do 
        vs <- map fst <$> allVersions tx database
        pure $ case vs of         
            []  -> Nothing
            vs' -> Just $ maximum vs'

getLastVersionOfKind :: (Storage s) => DB s -> Tx s 'RO -> VersionKind -> IO (Maybe WorldVersion)
getLastVersionOfKind database tx versionKind = do 
        versions <- allVersions tx database        
        pure $ case [ v | (v, k) <- versions, k == versionKind ] of         
                []  -> Nothing
                vs' -> Just $ maximum vs'

getLatestVRPs :: Storage s => DB s -> IO (Maybe Vrps)
getLatestVRPs db = 
    roTx db $ \tx ->        
        runMaybeT $ do 
            version <- MaybeT $ getLastCompletedVersion db tx
            MaybeT $ getVrps tx db version

getLatestAspas :: Storage s => DB s -> IO (Set.Set Aspa)
getLatestAspas db = roTx db $ \tx -> getLatestX tx db getAspas

getLatestGbrs :: Storage s => DB s -> IO [Located RpkiObject]
getLatestGbrs db = 
    roTx db $ \tx -> do 
        gbrs <- Set.toList <$> getLatestX tx db getGbrs 
        fmap catMaybes $ forM gbrs $ \(hash, _) -> getByHash tx db hash                       

getLatestBgps :: Storage s => DB s -> IO (Set.Set BGPSecPayload)
getLatestBgps db = roTx db $ \tx -> getLatestX tx db getBgps    
    
getLatestX :: (Storage s, Monoid b) =>
            Tx s 'RO
            -> DB s
            -> (Tx s 'RO -> DB s -> WorldVersion -> IO (Maybe b))
            -> IO b
getLatestX tx db f =      
        getLastCompletedVersion db tx >>= \case         
            Nothing      -> pure mempty
            Just version -> fromMaybe mempty <$> f tx db version    


getRtrPayloads :: (MonadIO m, Storage s) => Tx s 'RO -> DB s -> WorldVersion -> m (Maybe RtrPayloads)
getRtrPayloads tx db worldVersion = 
    liftIO $ runMaybeT $ do 
            vrps <- MaybeT $ getVrps tx db worldVersion
            bgps <- MaybeT $ getBgps tx db worldVersion
            pure $ mkRtrPayloads vrps bgps

getTotalDbStats :: (MonadIO m, Storage s) => DB s -> m (DBStats, SStats)
getTotalDbStats db = do 
    dbStats <- getDbStats db        
    pure (dbStats, totalStats dbStats)

-- Compute database stats
getDbStats :: (MonadIO m, Storage s) => DB s -> m DBStats
getDbStats db@DB {..} = liftIO $ roTx db $ \tx -> do 
    taStats         <- let TAStore sm = taStore in M.stats tx sm
    repositoryStats <- repositoryStats' tx
    rpkiObjectStats <- rpkiObjectStats' tx
    vResultStats    <- vResultStats' tx    
    vrpStats        <- let VRPStore sm = vrpStore in M.stats tx sm
    aspaStats       <- let AspaStore sm = aspaStore in M.stats tx sm
    bgpStats        <- let BgpStore sm = bgpStore in M.stats tx sm
    gbrStats        <- let GbrStore sm = gbrStore in M.stats tx sm
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
        lastValidMftsStats <- M.stats tx lastValidMfts

        uriToUriKeyStat <- M.stats tx uriToUriKey
        uriKeyToUriStat <- M.stats tx uriKeyToUri
        uriKeyToObjectKeyStat <- MM.stats tx urlKeyToObjectKey
        objectKeyToUrlKeysStat <- M.stats tx objectKeyToUrlKeys

        objectInsertedByStats <- M.stats tx objectInsertedBy
        objecBriefStats <- M.stats tx objectBriefs
        validatedByVersionStats <- M.stats tx validatedByVersion
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

-- Get all SStats and `<>` them
totalStats :: DBStats -> SStats
totalStats = foldOf (types @SStats)

emptyDBMaps :: (MonadIO m, Storage s) => Tx s 'RW -> DB s -> m ()
emptyDBMaps tx DB {..} = liftIO $ 
    forM_ erasables $ \(EraseWrapper t) -> erase tx t


-- Utilities to have storage transaction in ValidatorT monad.

roAppTx :: (Storage s, WithStorage s ws) => ws -> (Tx s 'RO -> ValidatorT IO a) -> ValidatorT IO a 
roAppTx ws f = appTx ws f roTx    

rwAppTx :: (Storage s, WithStorage s ws) => ws -> (Tx s 'RW -> ValidatorT IO a) -> ValidatorT IO a
rwAppTx ws f = appTx ws f rwTx


appTx :: (Storage s, WithStorage s ws) => 
        ws 
        -> (Tx s mode -> ValidatorT IO a) 
        -> (ws 
            -> (Tx s mode -> IO (Either AppError a, ValidationState))
            -> IO (Either AppError a, ValidationState)) 
        -> ValidatorT IO a
appTx ws f txF = do
    s <- askScopes
    embedValidatorT $ transaction s `catch` 
                    (\(TxRollbackException e vs) -> pure (Left e, vs))
  where
    transaction scopes = txF ws $ \tx -> do 
        z@(r, vs) <- runValidatorT scopes (f tx)
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