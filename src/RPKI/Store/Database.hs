{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE DeriveAnyClass        #-}

module RPKI.Store.Database where

import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Reader     (ask)
import           Data.Foldable            (for_)

import qualified Data.List                as List
import           Data.Maybe               (catMaybes, fromMaybe, isJust, listToMaybe)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Map.Strict          as Map
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import           Data.Ord
import           Data.Tuple.Strict

import           Data.Generics.Product.Typed

import           GHC.Generics
import           GHC.Natural
import           Text.Read

import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))
import           RPKI.Store.Base.SafeMap  (SafeMap)
import           RPKI.TAL
import           RPKI.RRDP.Types
import           RPKI.SLURM.Types
import           RPKI.Repository

import qualified RPKI.Store.Base.Map      as M
import qualified RPKI.Store.Base.MultiMap as MM
import qualified RPKI.Store.Base.SafeMap as SM
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import           RPKI.Store.Base.Serialisation
import           RPKI.Store.Sequence
import           RPKI.Store.Types
import           RPKI.Validation.Types

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
currentDatabaseVersion = 46

-- Some constant keys
databaseVersionKey, validatedByVersionKey :: Text
databaseVersionKey = "database-version"
validatedByVersionKey  = "validated-by-version-map"

-- All of the stores of the application in one place
data DB s = DB {
    keys             :: Sequence s,
    taStore          :: TAStore s, 
    repositoryStore  :: RepositoryStore s, 
    objectStore      :: RpkiObjectStore s,    
    validationsStore :: ValidationsStore s,    
    roaStore         :: RoaStore s,
    splStore         :: SplStore s,
    aspaStore        :: AspaStore s,
    gbrStore         :: GbrStore s,
    bgpStore         :: BgpStore s,
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
-- That's why all the fiddling with locations in saveObject, getLocatedByKey 
-- and deleteObject.
-- 
-- Also, since URLs are relativly long, there's a separate mapping between 
-- URLs and artificial UrlKeys.
-- 
data RpkiObjectStore s = RpkiObjectStore {        
        objects        :: SMap "objects" s ObjectKey (Compressed (StorableObject RpkiObject)),
        hashToKey      :: SMap "hash-to-key" s Hash ObjectKey,    
        mftsForKI      :: SMultiMap "mfts-for-ki" s AKI MftMeta,
        certBySKI      :: SMap "cert-by-ski" s SKI ObjectKey,    
        objectMetas    :: SMap "object-meta" s ObjectKey ObjectMeta,

        validatedByVersion :: SMap "validated-by-version" s Text (Compressed (Map.Map ObjectKey WorldVersion)),

        -- Object URL mapping
        uriToUriKey    :: SafeMap "uri-to-uri-key" s RpkiURL UrlKey,
        uriKeyToUri    :: SMap "uri-key-to-uri" s UrlKey RpkiURL,

        urlKeyToObjectKey  :: SMultiMap "uri-key-to-object-key" s UrlKey ObjectKey,
        objectKeyToUrlKeys :: SMap "object-key-to-uri" s ObjectKey [UrlKey],

        mftShortcuts       :: MftShortcutStore s,
        originals          :: SMap "object-original" s ObjectKey (Verbatim ObjectOriginal)
    } 
    deriving stock (Generic)


instance Storage s => WithStorage s (RpkiObjectStore s) where
    storage = storage . objects


-- | TA Store 
newtype TAStore s = TAStore { 
        tas :: SafeMap "trust-anchors" s TaName StorableTA
    }
    deriving stock (Generic)

instance Storage s => WithStorage s (TAStore s) where
    storage (TAStore s) = storage s

newtype ValidationsStore s = ValidationsStore {         
        validations :: SMap "validations" s ArtificialKey (Compressed Validations)
    }
    deriving stock (Generic)

instance Storage s => WithStorage s (ValidationsStore s) where
    storage (ValidationsStore s) = storage s

newtype MetricStore s = MetricStore {
        metrics :: SMap "metrics" s ArtificialKey (Compressed Metrics)
    }    
    deriving stock (Generic)

instance Storage s => WithStorage s (MetricStore s) where
    storage (MetricStore s) = storage s


-- | ROA/VRP store
newtype RoaStore s = RoaStore {    
        roas :: SMap "roas" s ArtificialKey (Compressed Roas)
    }
    deriving stock (Generic)

newtype SplStore s = SplStore {    
        spls :: SMap "spls" s ArtificialKey (Compressed (Set.Set SplN))
    }
    deriving stock (Generic)

-- | ASPA store
newtype AspaStore s = AspaStore {    
        aspas :: SMap "aspas" s ArtificialKey (Compressed (Set.Set Aspa))
    } 
    deriving stock (Generic)

-- | GBR store
newtype GbrStore s = GbrStore {    
        gbrs :: SMap "gbrs" s ArtificialKey (Compressed (Set.Set (T2 Hash Gbr)))
    } 
    deriving stock (Generic)

-- | BGP certificate store
newtype BgpStore s = BgpStore {    
        bgps :: SMap "bgps" s ArtificialKey (Compressed (Set.Set BGPSecPayload))
    }
    deriving stock (Generic)

instance Storage s => WithStorage s (RoaStore s) where
    storage (RoaStore s) = storage s


-- Version store
newtype VersionStore s = VersionStore {
        versions :: SMap "versions" s WorldVersion VersionMeta
    }
    deriving stock (Generic)

instance Storage s => WithStorage s (VersionStore s) where
    storage (VersionStore s) = storage s


newtype SlurmStore s = SlurmStore {
        slurms :: SMap "slurms" s WorldVersion (Compressed Slurm)
    }
    deriving stock (Generic)

data RepositoryStore s = RepositoryStore {
        rrdpS       :: SMap "rrdp-repositories" s RrdpURL RrdpRepository,
        rsyncS      :: SMap "rsync-repositories" s RsyncHost RsyncNodeNormal,
        rrdpVState  :: SMap "rrdp-validation-state" s RrdpURL (Compressed ValidationState),
        rsyncVState :: SMap "rsync-validation-state" s RsyncHost (Compressed (RsyncTree ValidationState))
    }
    deriving stock (Generic)

instance Storage s => WithStorage s (RepositoryStore s) where
    storage (RepositoryStore s _ _ _) = storage s

newtype JobStore s = JobStore {
        jobs :: SMap "jobs" s Text Instant
    }
    deriving stock (Generic)

newtype MetadataStore s = MetadataStore {
        metadata :: SMap "metadata" s Text Text
    }
    deriving stock (Generic)

-- Some DTOs for storing MFT shortcuts
data MftShortcutMeta = MftShortcutMeta {
        key            :: ObjectKey,        
        notValidBefore :: Instant,
        notValidAfter  :: Instant,        
        serial         :: Serial,
        manifestNumber :: Serial,
        crlShortcut    :: CrlShortcut
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

newtype MftShortcutChildren = MftShortcutChildren {
        nonCrlEntries :: Map.Map ObjectKey MftEntry
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)


data MftShortcutStore s = MftShortcutStore {
        mftMetas    :: SMap "mfts-shortcut-meta" s AKI (Verbatim (Compressed MftShortcutMeta)),
        mftChildren :: SMap "mfts-shortcut-children" s AKI (Verbatim (Compressed MftShortcutChildren))
    }
    deriving stock (Generic)


getKeyByHash :: (MonadIO m, Storage s) => 
                Tx s mode -> DB s -> Hash -> m (Maybe ObjectKey)
getKeyByHash tx DB { objectStore = RpkiObjectStore {..} } h = 
    liftIO $ M.get tx hashToKey h

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
getByUri tx db uri = liftIO $ do
    keys' <- getKeysByUri tx db uri
    catMaybes <$> mapM (getLocatedByKey tx db) keys'

getKeysByUri :: (MonadIO m, Storage s) => 
                Tx s mode -> DB s -> RpkiURL -> m [ObjectKey]
getKeysByUri tx DB { objectStore = RpkiObjectStore {..} } uri = liftIO $
    SM.safeGet tx uriToUriKey uri >>= \case 
        Nothing     -> pure []
        Just uriKey -> MM.allForKey tx urlKeyToObjectKey uriKey
                
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


-- Very specifis for optimising locations validation
getLocationCountByKey :: (MonadIO m, Storage s) => 
                        Tx s mode -> DB s -> ObjectKey -> m Int
getLocationCountByKey tx DB { objectStore = RpkiObjectStore {..} } k = liftIO $ do         
    M.get tx objectKeyToUrlKeys k >>= \case 
        Nothing      -> pure 0
        Just urlKeys -> pure $! length urlKeys    

getLocationsByKey :: (MonadIO m, Storage s) => 
                Tx s mode -> DB s -> ObjectKey -> m (Maybe Locations)
getLocationsByKey tx DB { objectStore = RpkiObjectStore {..} } k = liftIO $ runMaybeT $ do         
    uriKeys   <- MaybeT $ M.get tx objectKeyToUrlKeys k
    locations <- MaybeT 
                    $ (toNESet . catMaybes <$>)
                    $ mapM (M.get tx uriKeyToUri) uriKeys             
    pure $ Locations locations

saveObject :: (MonadIO m, Storage s) => 
            Tx s 'RW 
            -> DB s 
            -> StorableObject RpkiObject
            -> WorldVersion 
            -> m ObjectKey
saveObject tx DB { objectStore = RpkiObjectStore {..}, .. } so@StorableObject {..} wv = liftIO $ do
    let h = getHash object
    existingKey <- M.get tx hashToKey h
    case existingKey of
        Just key -> pure key
        Nothing  -> do
            SequenceValue k <- nextValue tx keys
            let objectKey = ObjectKey $ ArtificialKey k
            M.put tx hashToKey h objectKey
            M.put tx objects objectKey (Compressed so)
            M.put tx objectMetas objectKey (ObjectMeta wv (getRpkiObjectType object))
            case object of
                CerRO c -> 
                    M.put tx certBySKI (getSKI c) objectKey
                MftRO mft -> 
                    for_ (getAKI object) $ \aki_ -> 
                        MM.put tx mftsForKI aki_ (getMftMeta mft objectKey)
                _ -> pure ()        

            pure objectKey

saveOriginal :: (MonadIO m, Storage s) => 
                Tx s 'RW 
                -> DB s 
                -> ObjectOriginal
                -> Hash          
                -> ObjectMeta  
                -> m ()
saveOriginal tx DB { objectStore = RpkiObjectStore {..}, .. } (ObjectOriginal blob) hash objectMeta = liftIO $ do    
    exists <- M.exists tx hashToKey hash    
    unless exists $ do          
        SequenceValue k <- nextValue tx keys
        let key = ObjectKey $ ArtificialKey k
        M.put tx hashToKey hash key
        M.put tx originals key (Verbatim $ Storable blob)       
        M.put tx objectMetas key objectMeta


getOriginalBlob :: (MonadIO m, Storage s) => 
                Tx s mode
                -> DB s 
                -> ObjectKey            
                -> m (Maybe ObjectOriginal)
getOriginalBlob tx DB { objectStore = RpkiObjectStore {..} } key = liftIO $ do    
    fmap (ObjectOriginal . unStorable . unVerbatim) <$> M.get tx originals key

getOriginalBlobByHash :: (MonadIO m, Storage s) => 
                        Tx s mode
                        -> DB s 
                        -> Hash 
                        -> m (Maybe ObjectOriginal)
getOriginalBlobByHash tx db hash =     
    getKeyByHash tx db hash >>= \case 
        Nothing  -> pure Nothing
        Just key -> getOriginalBlob tx db key    

getObjectMeta :: (MonadIO m, Storage s) => 
                Tx s mode
                -> DB s 
                -> ObjectKey            
                -> m (Maybe ObjectMeta)
getObjectMeta tx DB { objectStore = RpkiObjectStore {..} } key = 
    liftIO $ M.get tx objectMetas key                

linkObjectToUrl :: (MonadIO m, Storage s) => 
                Tx s 'RW 
                -> DB s 
                -> RpkiURL
                -> Hash
                -> m ()
linkObjectToUrl tx DB { objectStore = RpkiObjectStore {..}, .. } rpkiURL hash = liftIO $ do    
    ifJustM (M.get tx hashToKey hash) $ \objectKey -> do        
        z <- SM.safeGet tx uriToUriKey rpkiURL 
        urlKey <- maybe (saveUrl rpkiURL) pure z                
        
        M.get tx objectKeyToUrlKeys objectKey >>= \case 
            Nothing -> do 
                M.put tx objectKeyToUrlKeys objectKey [urlKey]
                MM.put tx urlKeyToObjectKey urlKey objectKey
            Just existingUrlKeys -> 
                unless (urlKey `elem` existingUrlKeys) $ do 
                    M.put tx objectKeyToUrlKeys objectKey (urlKey : existingUrlKeys)
                    MM.put tx urlKeyToObjectKey urlKey objectKey
  where
    saveUrl safeUrl = do 
        SequenceValue k <- nextValue tx keys
        let urlKey = UrlKey $ ArtificialKey k
        SM.safePut tx uriToUriKey safeUrl urlKey
        M.put tx uriKeyToUri urlKey rpkiURL            
        pure urlKey


hashExists :: (MonadIO m, Storage s) => 
            Tx s mode -> DB s -> Hash -> m Bool
hashExists tx DB { objectStore = RpkiObjectStore {..} } h = 
    liftIO $ M.exists tx hashToKey h


deleteObjectByHash :: (MonadIO m, Storage s) => Tx s 'RW -> DB s -> Hash -> m ()
deleteObjectByHash tx db@DB { objectStore = RpkiObjectStore {..} } hash = liftIO $ 
    ifJustM (M.get tx hashToKey hash) $ deleteObjectByKey tx db

deleteObjectByKey :: (MonadIO m, Storage s) => Tx s 'RW -> DB s -> ObjectKey -> m ()
deleteObjectByKey tx db@DB { objectStore = RpkiObjectStore { mftShortcuts = MftShortcutStore {..}, ..} } objectKey = liftIO $ do 
    ifJustM (getObjectByKey tx db objectKey) $ \ro -> do 
        M.delete tx objects objectKey
        M.delete tx objectMetas objectKey        
        M.delete tx hashToKey (getHash ro)
        case ro of 
            CerRO c -> M.delete tx certBySKI (getSKI c)
            _       -> pure ()
        ifJustM (M.get tx objectKeyToUrlKeys objectKey) $ \urlKeys -> do 
            M.delete tx objectKeyToUrlKeys objectKey            
            forM_ urlKeys $ \urlKey ->
                MM.delete tx urlKeyToObjectKey urlKey objectKey                
        
        for_ (getAKI ro) $ \aki_ -> 
            case ro of
                MftRO mft -> do 
                    MM.delete tx mftsForKI aki_ (getMftMeta mft objectKey)                    
                    ifJustM (M.get tx mftMetas aki_) $ \(unCompressed . restoreFromRaw -> mftShort) ->
                        when (mftShort ^. #key == objectKey) $
                            deleteMftShortcut tx db aki_
                _  -> pure ()   

getMftsForAKI :: (MonadIO m, Storage s) => 
                Tx s mode -> DB s -> AKI -> m [MftMeta]
getMftsForAKI tx DB { objectStore = RpkiObjectStore {..} } aki_ = 
    liftIO $ List.sortOn Down <$> MM.allForKey tx mftsForKI aki_

findAllMftsByAKI :: (MonadIO m, Storage s) => 
                    Tx s mode -> DB s -> AKI -> m [(MftMeta, Keyed (Located MftObject))]
findAllMftsByAKI tx db aki_ = liftIO $ do
    mftMetas <- getMftsForAKI tx db aki_
    fmap catMaybes $ forM mftMetas $ \meta@MftMeta {..} -> fmap (meta,) <$> getMftByKey tx db key
    

getMftByKey :: (MonadIO m, Storage s) => 
                Tx s mode -> DB s -> ObjectKey -> m (Maybe (Keyed (Located MftObject)))
getMftByKey tx db k = do 
    o <- getLocatedByKey tx db k
    pure $! case o of 
        Just (Located loc (MftRO mft)) -> Just $ Keyed (Located loc mft) k
        _                              -> Nothing       


getMftShorcut :: (MonadIO m, Storage s) => 
                Tx s mode -> DB s -> AKI -> m (Maybe MftShortcut)
getMftShorcut tx DB { objectStore = RpkiObjectStore {..} } aki = liftIO $ do 
    let MftShortcutStore {..} = mftShortcuts 
    runMaybeT $ do 
        mftMeta_ <- MaybeT $ M.get tx mftMetas aki
        let MftShortcutMeta {..} = unCompressed $ restoreFromRaw mftMeta_
        mftChildren_ <- MaybeT $ M.get tx mftChildren aki
        let MftShortcutChildren {..} = unCompressed $ restoreFromRaw mftChildren_
        pure $! MftShortcut {..}

saveMftShorcutMeta :: (MonadIO m, Storage s) => 
                    Tx s 'RW -> DB s -> AKI -> Verbatim (Compressed MftShortcutMeta) -> m ()
saveMftShorcutMeta tx 
    DB { objectStore = RpkiObjectStore { mftShortcuts = MftShortcutStore {..} } } 
    aki raw = liftIO $ M.put tx mftMetas aki raw

saveMftShorcutChildren :: (MonadIO m, Storage s) => 
                        Tx s 'RW -> DB s -> AKI -> Verbatim (Compressed MftShortcutChildren) -> m ()
saveMftShorcutChildren tx 
    DB { objectStore = RpkiObjectStore { mftShortcuts = MftShortcutStore {..} } } 
    aki raw = liftIO $ M.put tx mftChildren aki raw


deleteMftShortcut :: (MonadIO m, Storage s) => 
                    Tx s 'RW -> DB s -> AKI -> m ()
deleteMftShortcut tx 
    DB { objectStore = RpkiObjectStore { mftShortcuts = MftShortcutStore {..} } } 
    aki = liftIO $ do
        M.delete tx mftMetas aki
        M.delete tx mftChildren aki

markAsValidated :: (MonadIO m, Storage s) => 
                    Tx s 'RW -> DB s 
                -> Set.Set ObjectKey 
                -> WorldVersion -> m ()
markAsValidated tx db allKeys worldVersion = 
    liftIO $ void $ updateValidatedByVersionMap tx db $ \m -> 
        foldr (`Map.insert` worldVersion) (fromMaybe mempty m) allKeys


-- This is for testing purposes mostly
getAll :: (MonadIO m, Storage s) => Tx s mode -> DB s -> m [Located RpkiObject]
getAll tx db@DB { objectStore = RpkiObjectStore {..} } = liftIO $ do 
    allKeys <- M.keys tx objects
    catMaybes <$> forM allKeys (getLocatedByKey tx db)    

getMftMeta :: MftObject -> ObjectKey -> MftMeta
getMftMeta mft key = let 
    Manifest {..} = getCMSContent $ cmsPayload mft 
    in MftMeta {..}


getBySKI :: (MonadIO m, Storage s) => Tx s mode -> DB s -> SKI -> m (Maybe (Located CaCerObject))
getBySKI tx db@DB { objectStore = RpkiObjectStore {..} } ski = liftIO $ runMaybeT $ do 
    objectKey <- MaybeT $ M.get tx certBySKI ski
    located   <- MaybeT $ getLocatedByKey tx db objectKey
    pure $ located & #payload %~ (\(CerRO c) -> c) 

-- TA store functions

saveTA :: (MonadIO m, Storage s) => Tx s 'RW -> DB s -> StorableTA -> m ()
saveTA tx DB { taStore = TAStore s } ta = liftIO $ SM.safePut tx s (getTaName $ tal ta) ta

deleteTA :: (MonadIO m, Storage s) => Tx s 'RW -> DB s -> TAL -> m ()
deleteTA tx DB { taStore = TAStore s } tal = liftIO $ SM.safeDelete tx s (getTaName tal)

getTA :: (MonadIO m, Storage s) => Tx s mode -> DB s -> TaName -> m (Maybe StorableTA)
getTA tx DB { taStore = TAStore s } name = liftIO $ SM.safeGet tx s name

getTAs :: (MonadIO m, Storage s) => Tx s mode -> DB s -> m [StorableTA]
getTAs tx DB { taStore = TAStore s } = liftIO $ SM.safeValues tx s


getValidationsPerTA :: (MonadIO m, Storage s) => 
            Tx s mode -> DB s -> WorldVersion -> m (PerTA Validations)
getValidationsPerTA tx db@DB {..} version = 
    liftIO $ getPayloadsForTas tx db version $ 
        \_ _ ValidationVersion {..} -> 
            fmap unCompressed <$> M.get tx (validationsStore ^. #validations) validationsKey

getMetricsPerTA :: (MonadIO m, Storage s) => 
            Tx s mode -> DB s -> WorldVersion -> m (PerTA Metrics)
getMetricsPerTA tx db@DB {..} version = 
    liftIO $ getPayloadsForTas tx db version $ 
        \_ _ ValidationVersion {..} -> 
            fmap unCompressed <$> M.get tx (metricStore ^. #metrics) metricsKey                   

getCommonMetrics :: (MonadIO m, Storage s) => 
                    Tx s mode -> DB s -> WorldVersion -> m Metrics
getCommonMetrics tx DB {..} version = 
    liftIO $ do 
        fmap (fromMaybe mempty) $ runMaybeT $ do
            VersionMeta {..} <- MaybeT $ M.get tx (versionStore ^. typed) version            
            MaybeT $ fmap unCompressed <$> M.get tx (metricStore ^. #metrics) commonMetricsKey           

getValidationOutcomes :: (MonadIO m, Storage s) => 
                        Tx s mode 
                        -> DB s 
                        -> WorldVersion 
                        -> m (Validations, Metrics, PerTA (Validations, Metrics))
getValidationOutcomes tx db@DB {..} version = liftIO $ do 
    (commonV, commonM) <- 
        fmap (fromMaybe mempty) $ runMaybeT $ do
                    VersionMeta {..} <- MaybeT $ M.get tx (versionStore ^. typed) version            
                    getOutcomes commonValidationKey commonMetricsKey                    

    perTAOutcomes <- 
        getPayloadsForTas tx db version $ 
            \_ _ ValidationVersion {..} -> 
                runMaybeT $ getOutcomes validationsKey metricsKey                    

    pure (commonV, commonM, perTAOutcomes)
  where
    getOutcomes validationsKey metricsKey = do 
        v <- MaybeT $ fmap unCompressed <$> M.get tx (validationsStore ^. #validations) validationsKey   
        m <- MaybeT $ fmap unCompressed <$> M.get tx (metricStore ^. #metrics) metricsKey   
        pure (v, m)        


getVrps :: (MonadIO m, Storage s) => 
            Tx s mode -> DB s -> WorldVersion -> m (PerTA Vrps)
getVrps tx db version = fmap toVrps <$> getRoas tx db version 

getVrpsForTA :: (MonadIO m, Storage s) => 
            Tx s mode -> DB s -> WorldVersion -> TaName -> m Vrps
getVrpsForTA tx DB {..} version taName = 
    liftIO $ fmap (toVrps . maybe mempty unCompressed) $ runMaybeT $ do 
        VersionMeta {..} <- MaybeT $ M.get tx (versionStore ^. typed) version
        ValidationVersion {..} <- MaybeT $ pure $ getForTA perTa taName
        MaybeT (M.get tx (roaStore ^. typed) roasKey)

            
getRoas :: (MonadIO m, Storage s) => 
            Tx s mode -> DB s -> WorldVersion -> m (PerTA Roas)
getRoas tx db@DB { roaStore = RoaStore m } version = liftIO 
    $ getPayloadsForTas tx db version $ \_ _ ValidationVersion {..} -> 
        fmap unCompressed <$> M.get tx m roasKey    

getAspas :: (MonadIO m, Storage s) => 
            Tx s mode -> DB s -> WorldVersion -> m (Maybe (Set.Set Aspa))
getAspas tx db@DB { aspaStore = AspaStore m } version = 
    liftIO $ fmap (Just . allTAs)
        $ getPayloadsForTas tx db version $ 
            \_ _ ValidationVersion {..} -> 
                fmap unCompressed <$> M.get tx m aspasKey    

getGbrs :: (MonadIO m, Storage s) => 
            Tx s mode -> DB s -> WorldVersion -> m (Maybe (Set.Set (T2 Hash Gbr)))
getGbrs tx db@DB { gbrStore = GbrStore m } version = 
    liftIO $ fmap (Just . allTAs)
        $ getPayloadsForTas tx db version $ 
            \_ _ ValidationVersion {..} -> 
                fmap unCompressed <$> M.get tx m gbrsKey

getBgps :: (MonadIO m, Storage s) => 
            Tx s mode -> DB s -> WorldVersion -> m (Maybe (Set.Set BGPSecPayload))
getBgps tx db@DB { bgpStore = BgpStore m } version = 
    liftIO $ fmap (Just . allTAs)
        $ getPayloadsForTas tx db version $ 
            \_ _ ValidationVersion {..} -> 
                fmap unCompressed <$> M.get tx m bgpCertsKey

getSpls :: (MonadIO m, Storage s) => 
            Tx s mode -> DB s -> WorldVersion -> m (Maybe (Set.Set SplN))
getSpls tx db@DB { splStore = SplStore m } version = 
    liftIO $ fmap (Just . allTAs)
        $ getPayloadsForTas tx db version $ 
            \_ _ ValidationVersion {..} -> 
                fmap unCompressed <$> M.get tx m splsKey


versionsBackwards :: (MonadIO m, Storage s) => Tx s mode -> DB s -> m [(WorldVersion, VersionMeta)]
versionsBackwards tx DB { versionStore = VersionStore s } = 
    liftIO $ List.sortOn (Down . fst) <$> M.all tx s

previousVersion :: (MonadIO m, Storage s) => 
                    Tx s mode -> DB s -> WorldVersion -> m (Maybe WorldVersion)
previousVersion tx DB { versionStore = VersionStore s } version = liftIO $ do 
    versions <- M.all tx s    
    case List.filter (\(v, _) -> v < version) versions of 
        [] -> pure Nothing
        v  -> pure $ Just $ maximum $ map fst v


getPayloadsForTas :: (MonadIO m, Storage s)
                => Tx s mode 
                -> DB s 
                -> WorldVersion
                -> (Tx s mode -> DB s -> ValidationVersion -> IO (Maybe payload)) 
                -> m (PerTA payload)
getPayloadsForTas tx db@DB {..} version f = liftIO $ do
    fmap (toPerTA . catMaybes) $ 
        M.get tx (versionStore ^. typed) version >>= \case
            Nothing          -> pure []
            Just versionMeta -> 
                forM (perTA $ versionMeta ^. typed) $ \(ta, vv) -> 
                    fmap (ta,) <$> f tx db vv


getLatestVersions :: (MonadIO m, Storage s) => 
                    Tx s mode -> DB s -> m (PerTA WorldVersion)
getLatestVersions tx db = liftIO $
    getLatestVersion tx db >>= \case    
        Nothing            -> pure $ PerTA MonoidalMap.empty
        Just latestVersion -> 
            getPayloadsForTas tx db latestVersion $ 
                    \_ _ ValidationVersion {..} -> pure $ Just validatedBy

saveValidationVersion :: forall m s . (MonadIO m, Storage s) => 
                        Tx s 'RW 
                    -> DB s 
                    -> WorldVersion                     
                    -> [TaName]
                    -> PerTA (Payloads, ValidationState)                    
                    -> ValidationState
                    -> m ()
saveValidationVersion tx db@DB { ..} 
    validatedBy allTaNames results@(PerTA perTAResults) commonVS = liftIO $ do         

    commonValidationKey <- save (commonVS ^. typed @Validations) $ validationsStore ^. #validations
    commonMetricsKey <- save (commonVS ^. typed @Metrics) $ metricStore ^. #metrics

    -- For the TAs present in results save the results
    addedResults <- 
        forM (perTA results) $ \(taName, (Payloads {..}, vs)) -> do                         
            roasKey  <- save roas $ roaStore ^. typed
            aspasKey <- save aspas $ aspaStore ^. typed
            splsKey  <- save spls $ splStore ^. typed
            gbrsKey  <- save gbrs $ gbrStore ^. typed
            bgpCertsKey <- save bgpCerts $ bgpStore ^. typed

            validationsKey <- save (vs ^. typed @Validations) $ validationsStore ^. #validations
            metricsKey <- save (vs ^. typed @Metrics) $ metricStore ^. #metrics
            
            -- The keys may refer to nonexistent entries
            pure (taName, ValidationVersion {..})

    -- For the TAs not present in results find the latest metas that has 
    -- result for the TA and add to the current version
    let notPresentTAs = filter (`MonoidalMap.notMember` perTAResults) allTaNames 

    earlierResults <- 
        case notPresentTAs of 
            [] -> pure []
            _  -> do     
                versions <- versionsBackwards tx db 
                pure [ (ta, r) | (ta, Just r) <- fillUpEarlierTAData versions notPresentTAs mempty ]

    M.put tx (versionStore ^. typed) validatedBy $ 
        VersionMeta { perTa = toPerTA $ addedResults <> earlierResults, .. }

  where
    save :: forall what mapName . 
            (AsStorable what, Monoid what, Eq what) 
            => what 
            -> SMap mapName s ArtificialKey (Compressed what) -> IO ArtificialKey
    save what whereTo = do                 
        key <- ArtificialKey . unSequenceValue <$> nextValue tx keys
        M.put tx whereTo key (Compressed what)
        pure key

    fillUpEarlierTAData [] _ accPerTa = accPerTa
    fillUpEarlierTAData _ [] accPerTa = accPerTa

    fillUpEarlierTAData ((_, versionMeta) : versions) tasToFind accPerTa = do 
        let (found, notFound) = List.partition (isJust . snd) 
                [ (ta, MonoidalMap.lookup ta (unPerTA $ versionMeta ^. typed)) | ta <- tasToFind ]        
        
        fillUpEarlierTAData versions (map fst notFound) (accPerTa <> found)          


deleteValidationVersion :: (MonadIO m, Storage s) => 
            Tx s 'RW -> DB s -> WorldVersion -> m ()
deleteValidationVersion tx DB {..} worldVersion = liftIO $ do  
    ifJustM (M.get tx (versionStore ^. typed) worldVersion) $ \versionMeta -> do
        M.delete tx (validationsStore ^. typed) (versionMeta ^. #commonValidationKey)                
        M.delete tx (metricStore ^. typed) (versionMeta ^. #commonMetricsKey)
        for_ (perTA $ versionMeta ^. typed) $ \(_, ValidationVersion {..}) -> do
            M.delete tx (roaStore ^. typed) roasKey
            M.delete tx (aspaStore ^. typed) aspasKey
            M.delete tx (splStore ^. typed) splsKey
            M.delete tx (gbrStore ^. typed) gbrsKey
            M.delete tx (bgpStore ^. typed) bgpCertsKey
            M.delete tx (metricStore ^. typed) metricsKey
            M.delete tx (validationsStore ^. typed) validationsKey        
        
    M.delete tx (slurmStore ^. typed) worldVersion
    M.delete tx (versionStore ^. typed) worldVersion


saveSlurm :: (MonadIO m, Storage s) => Tx s 'RW -> DB s -> WorldVersion -> Slurm -> m ()
saveSlurm tx DB { slurmStore = SlurmStore s } version slurm = 
    liftIO $ M.put tx s version (Compressed slurm)

getSlurm :: (MonadIO m, Storage s) => Tx s mode -> DB s -> WorldVersion -> m (Maybe Slurm)
getSlurm tx DB { slurmStore = SlurmStore s } version = 
    liftIO $ fmap unCompressed <$> M.get tx s version    

updateRrdpMeta :: (MonadIO m, Storage s) =>
                Tx s 'RW -> DB s -> RrdpMeta -> RrdpURL -> m ()
updateRrdpMeta tx db meta url = liftIO $ do
    updateRrdpMetaM tx db url $ \_ -> pure $ Just meta    

updateRrdpMetaM :: (MonadIO m, Storage s) =>
                    Tx s 'RW 
                -> DB s
                -> RrdpURL                  
                -> (Maybe RrdpMeta -> IO (Maybe RrdpMeta))                
                -> m ()
updateRrdpMetaM tx DB { repositoryStore = RepositoryStore {..} } url f = liftIO $ do    
    maybeRepo <- M.get tx rrdpS url
    for_ maybeRepo $ \repo -> do        
        maybeNewMeta <- f $ repo ^. #rrdpMeta
        for_ maybeNewMeta $ \newMeta -> 
            M.put tx rrdpS url (repo { rrdpMeta = Just newMeta })
 
getPublicationPoints :: (MonadIO m, Storage s) => Tx s mode -> DB s -> m PublicationPoints
getPublicationPoints tx DB { repositoryStore = RepositoryStore {..}} = liftIO $ do
    rrdps <- M.all tx rrdpS
    rsyns <- M.all tx rsyncS    
    pure $ PublicationPoints
            (RrdpMap $ Map.fromList rrdps)
            (RsyncForestGen $ Map.fromList rsyns)  

getRepository :: (MonadIO m, Storage s) => Tx s mode -> DB s -> RpkiURL -> m (Maybe Repository)
getRepository tx db = \case
        RrdpU u  -> fmap RrdpR <$> getRrdpRepository tx db u
        RsyncU u -> fmap RsyncR <$> getRsyncRepository tx db u

getRrdpRepository :: (MonadIO m, Storage s) => Tx s mode -> DB s -> RrdpURL -> m (Maybe RrdpRepository)
getRrdpRepository tx DB { repositoryStore = RepositoryStore {..}} url = 
    liftIO $ M.get tx rrdpS url

getRsyncRepository :: (MonadIO m, Storage s) => Tx s mode -> DB s -> RsyncURL -> m (Maybe RsyncRepository)
getRsyncRepository tx db url = Map.lookup url <$> getRsyncRepositories tx db [url]        

getRsyncRepositories :: (MonadIO m, Storage s) => Tx s mode -> DB s -> [RsyncURL] -> m (Map.Map RsyncURL RsyncRepository)
getRsyncRepositories tx DB { repositoryStore = RepositoryStore {..}} urls = 
    getRsyncAnything urls 
        (M.get tx rsyncS) 
        (\url meta -> RsyncRepository { repoPP = RsyncPublicationPoint url, .. })  

getRsyncAnything :: MonadIO m 
                => [RsyncURL] 
                -> (RsyncHost -> IO (Maybe (RsyncTree a)))
                -> (RsyncURL -> a -> b)
                -> m (Map.Map RsyncURL b)
getRsyncAnything urls extractTree create = liftIO $ do 

    let groupedByHost = Map.fromListWith (<>) [ (host, [u]) | u@(RsyncURL host _) <- urls ]    

    fmap (Map.fromList . mconcat)
        $ forM (Map.toList groupedByHost) 
        $ \(host, thisHostUrls) -> do        
            z <- extractTree host
            pure $ case z of
                Nothing   -> []
                Just tree -> 
                    [ (u, create url' content) |
                        u@(RsyncURL _ path) <- thisHostUrls,
                        Just (path', content ) <- [ lookupInRsyncTree path tree ],
                        let url' = RsyncURL host path'
                    ] 

saveRepositories :: (MonadIO m, Storage s) => Tx s 'RW -> DB s -> [Repository] -> m ()
saveRepositories tx db@DB { repositoryStore = RepositoryStore {..}} repositories = liftIO $ do    
    let (rrdps, rsyncs) = separate repositories
    forM_ rrdps $ \r -> M.put tx rrdpS (r ^. #uri) r
    saveRsyncRepositories tx db rsyncs
  where
    separate = foldr f ([], [])
      where
        f (RrdpR r)  (rrdps, rsyncs) = (r : rrdps, rsyncs)
        f (RsyncR r) (rrdps, rsyncs) = (rrdps, r : rsyncs)

saveRepositoryValidationStates :: (MonadIO m, Storage s) => Tx s 'RW -> DB s -> [(Repository, ValidationState)] -> m ()
saveRepositoryValidationStates tx db@DB { repositoryStore = RepositoryStore {..}} repositories = liftIO $ do    
    let (rrdps, rsyncs) = separate repositories
    forM_ rrdps $ \(r, vs) -> M.put tx rrdpVState (r ^. #uri) (Compressed vs)
    saveRsyncValidationStates tx db rsyncs
  where
    separate = foldr f ([], [])
      where
        f (RrdpR r, a)  (rrdps, rsyncs) = ((r, a) : rrdps, rsyncs)
        f (RsyncR r, a) (rrdps, rsyncs) = (rrdps, (r, a) : rsyncs)

saveRsyncRepositories :: (MonadIO m, Storage s) => Tx s 'RW -> DB s -> [RsyncRepository] -> m ()
saveRsyncRepositories tx DB { repositoryStore = RepositoryStore {..}} repositories = liftIO $ do
    saveRsyncAnything (map (\r -> (r, r ^. #meta)) repositories)
        (M.get tx rsyncS)
        (M.put tx rsyncS)        

saveRsyncValidationStates :: (MonadIO m, Storage s) => Tx s 'RW -> DB s -> [(RsyncRepository, ValidationState)] -> m ()
saveRsyncValidationStates tx DB { repositoryStore = RepositoryStore {..}} repositories = liftIO $ do
    saveRsyncAnything repositories
        (fmap (fmap unCompressed) . M.get tx rsyncVState)
        (\host tree -> M.put tx rsyncVState host (Compressed tree))        

saveRsyncAnything :: MonadIO m
                    => [(RsyncRepository, a)] 
                    -> (RsyncHost -> IO (Maybe (RsyncTree a)))
                    -> (RsyncHost -> RsyncTree a -> IO ())
                    -> m ()
saveRsyncAnything repositories extractTree saveTree = liftIO $ do 

    let groupedByHost = Map.fromListWith (<>) [ (host, [(path, a)]) | 
            (RsyncRepository { repoPP = RsyncPublicationPoint (RsyncURL host path) }, a) <- repositories ] 
        
    for_ (Map.toList groupedByHost) $ \(host, pathAndA) -> do
        startTree <- fromMaybe newRsyncTree <$> extractTree host
        let tree' = foldr (uncurry pathToRsyncTree) startTree pathAndA
        saveTree host tree'

getRepositories :: (MonadIO m, Storage s) 
                        => Tx s mode 
                        -> DB s 
                        -> (RpkiURL -> Bool)
                        -> m [(Repository, ValidationState)]
getRepositories tx DB { repositoryStore = RepositoryStore {..}} filterF = liftIO $ do
    rrdps <- M.all tx rrdpS
    rsyncs <- M.all tx rsyncS    

    rrpdRepos <- fmap mconcat $ 
        forM rrdps $ \(url, r) -> do 
            if filterF (RrdpU url) then 
                M.get tx rrdpVState url >>= \case 
                    Nothing              -> pure []
                    Just (Compressed vs) -> pure [(RrdpR r, vs)]
            else pure []

    rsyncRepos <- fmap mconcat $ 
        forM rsyncs $ \(host, metas) -> do 
            M.get tx rsyncVState host >>= \case 
                Nothing               -> pure []
                Just (Compressed vss) -> 
                    pure [ (RsyncR repo, vs) | 
                            (RsyncURL _ path, meta) <- flattenTree host metas,
                            let uri = RsyncURL host path,
                            let repo = RsyncRepository { repoPP = RsyncPublicationPoint uri, .. },
                            filterF (RsyncU uri),
                            Just (_, vs) <- [lookupInRsyncTree path vss]
                        ]
    pure $ rrpdRepos <> rsyncRepos

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


updateValidatedByVersionMap :: (MonadIO m, Storage s) 
                            => Tx s 'RW 
                            -> DB s 
                            -> (Maybe (Map.Map ObjectKey WorldVersion) -> Map.Map ObjectKey WorldVersion)
                            -> m (Map.Map ObjectKey WorldVersion)
updateValidatedByVersionMap tx DB { objectStore = RpkiObjectStore {..} } f = liftIO $ do
    validatedBy <- M.get tx validatedByVersion validatedByVersionKey    
    let validatedBy' = f $ unCompressed <$> validatedBy
    M.put tx validatedByVersion validatedByVersionKey $ Compressed validatedBy'
    pure validatedBy'


getObjectsStats :: (MonadIO m, Storage s) => Tx s mode -> DB s -> m ObjectStats
getObjectsStats tx DB { objectStore = RpkiObjectStore {..} } = 
    liftIO $ do 
        stats <- M.fold tx objectMetas
            (\acc key (ObjectMeta _ type_) -> do 
                objectSize <- M.binarySize tx objects key >>= \case 
                    Nothing -> pure mempty
                    Just size -> pure $ Size $ fromIntegral size            

                pure $ acc & #totalObjects %~ (+1) 
                           & #totalSize %~ (+ objectSize)                
                           & #countPerType %~ Map.insertWith (+) type_ 1
                           & #totalSizePerType %~ Map.insertWith (+) type_ objectSize
                           & #minSizePerType %~ Map.alter (Just . maybe objectSize (min objectSize)) type_
                           & #maxSizePerType %~ Map.alter (Just . maybe objectSize (max objectSize)) type_
            )
            mempty

        pure $ stats & #avgSizePerType .~ 
            Map.mapWithKey (\k s -> maybe 0 (s `divSize`) $ Map.lookup k (stats ^. #countPerType)) (stats ^. #totalSizePerType)
  
    

-- More complicated operations

data CleanUpResult = CleanUpResult {
        deletedObjects  :: Int,
        deletedPerType  :: Map.Map RpkiObjectType Integer,
        keptObjects     :: Int,
        deletedURLs     :: Int,
        deletedVersions :: Int
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data DeletionCriteria = DeletionCriteria {
        versionIsTooOld :: WorldVersion -> Bool,
        objectIsTooOld  :: WorldVersion -> RpkiObjectType -> Bool
    }
    deriving (Generic)


deleteOldestVersionsIfNeeded :: (MonadIO m, Storage s) => 
                               Tx s 'RW 
                            -> DB s 
                            -> Natural 
                            -> m [WorldVersion]
deleteOldestVersionsIfNeeded tx db versionNumberToKeep =  
    mapException (AppException . storageError) <$> liftIO $ do
        versions <- versionsBackwards tx db
        -- Keep at least 2 versions (current and previous)
        let reallyToKeep = max 2 (fromIntegral versionNumberToKeep)
        if length versions > reallyToKeep
            then do           
                let versionsToDelete = map fst $ findEnoughForEachTA reallyToKeep versions mempty                          
                forM_ versionsToDelete $ deleteValidationVersion tx db
                pure versionsToDelete
            else pure []        
  where    
    -- There should be at least `numberToKeep` versions for each TA, 
    -- only after that we can delete the older versions.
    findEnoughForEachTA _ [] _ = []

    findEnoughForEachTA numberToKeep ((_, meta) : versions) acc = 
        if any (\v -> Set.size v < fromIntegral numberToKeep) $ MonoidalMap.elems acc'
            then findEnoughForEachTA numberToKeep versions acc'
            else versions
      where         
        acc' = acc <> mconcat [ MonoidalMap.singleton ta (Set.singleton v) | (ta, v) <- perTA $ meta ^. #perTa ]        
                

deleteStaleContent :: (MonadIO m, Storage s) => 
                DB s                 
                -> DeletionCriteria                                       
                -> m CleanUpResult
deleteStaleContent db@DB { objectStore = RpkiObjectStore {..} } DeletionCriteria {..} = 
    mapException (AppException . storageError) <$> liftIO $ do                
        
        -- Delete old versions associated with async fetches and 
        -- other non-validation related stuff        
        rwTx db $ \tx -> do            
            deletedVersions <- deleteOldVersions tx

            -- Now delete objects that are last used by the deleted versions
            (deletedObjects, deletedPerType, keptObjects) <- deleteStaleObjects tx

            -- Delete URLs that are now not referred by any object
            deletedURLs <- deleteDanglingUrls db tx

            pure CleanUpResult {..}
  where

    deleteOldVersions tx = do
        toDelete <- filter versionIsTooOld . map fst <$> versionsBackwards tx db
        forM_ toDelete $ deleteValidationVersion tx db
        pure $ List.length toDelete

    deleteStaleObjects tx = do 

        validatedBy <- maybe mempty unCompressed <$> M.get tx validatedByVersion validatedByVersionKey        

        deletedPerType <- newTVarIO mempty
        keptTotal      <- newTVarIO 0
        keysToDelete <- M.fold tx objectMetas 
            (\toDelete key (ObjectMeta insertedBy type_) -> do 
                -- Object was inserted by a version that is "too old".
                let insertedWayBack = objectIsTooOld insertedBy type_                

                -- Object was validated by versions that is "too old" or not validated at all.
                let validatedWayBack = 
                        case Map.lookup key validatedBy of 
                            Just validated -> objectIsTooOld validated type_
                            Nothing        -> True
            
                -- An object can stay in the cache if 
                -- 1. It was inserted by a recent version or
                -- 2. It was validated by a recent version
                if insertedWayBack && validatedWayBack
                    then do 
                        atomically $ modifyTVar' deletedPerType $ Map.unionWith (+) (Map.singleton type_ 1)
                        pure $! key : toDelete
                    else do 
                        atomically $ modifyTVar' keptTotal (+1)                            
                        pure $! toDelete
            )
            mempty
                        
        let validatedBy' = foldr Map.delete validatedBy keysToDelete        
        M.put tx validatedByVersion validatedByVersionKey $ Compressed validatedBy'        

        forM_ keysToDelete $ deleteObjectByKey tx db

        atomically $ do             
            deleted <- readTVar deletedPerType
            let deletedCount = fromIntegral $ sum $ Map.elems deleted
            kept    <- readTVar keptTotal
            pure (deletedCount, deleted, kept)


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

    for_ urlsToDelete $ \(urlKey, url) -> do 
        M.delete tx uriKeyToUri urlKey
        SM.safeDelete tx uriToUriKey url           

    pure $ length urlsToDelete



-- TODO implement min and max in maps?
getLatestVersion :: (Storage s) => Tx s mode -> DB s -> IO (Maybe WorldVersion)
getLatestVersion tx db = do
    listToMaybe . map fst <$> versionsBackwards tx db
        
                    
getGbrObjects :: (MonadIO m, Storage s) => 
                Tx s mode -> DB s -> WorldVersion -> m [Located RpkiObject]
getGbrObjects tx db version = do    
    gbrs <- maybe [] Set.toList <$> getGbrs tx db version
    fmap catMaybes $ forM gbrs $ \(T2 hash _) -> getByHash tx db hash
    

getRtrPayloads :: (MonadIO m, Storage s) => Tx s 'RO -> DB s -> WorldVersion -> m (Maybe RtrPayloads)
getRtrPayloads tx db worldVersion = 
    liftIO $ runMaybeT $ do 
        vrps <- MaybeT $ Just <$> getVrps tx db worldVersion
        bgps <- MaybeT $ getBgps tx db worldVersion
        pure $ mkRtrPayloads vrps bgps                       

-- Get all SStats and `<>` them
totalStats :: StorageStats -> SStats
totalStats (StorageStats s) = mconcat $ Map.elems s
   

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


                