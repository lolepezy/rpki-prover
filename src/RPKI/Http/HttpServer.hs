{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module RPKI.Http.HttpServer where

import           Control.Lens
import           Control.Monad

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Error.Class

import           FileEmbedLzma

import           Servant.Server.Generic
import           Servant hiding (contentType, URI)
import           Servant.Swagger.UI

import           Data.Ord
import           Data.Maybe                       (maybeToList, fromMaybe)
import qualified Data.Set                         as Set
import qualified Data.List                        as List
import qualified Data.Map.Strict                  as Map
import qualified Data.Map.Monoidal.Strict         as MonoidalMap
import qualified Data.List.NonEmpty               as NonEmpty
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.String.Interpolate.IsString

import           Text.Read                        (readMaybe)

import           RPKI.AppContext
import           RPKI.AppTypes
import           RPKI.AppState
import           RPKI.Domain
import           RPKI.Config
import           RPKI.Metrics.Prometheus
import           RPKI.Metrics.System
import           RPKI.Time
import           RPKI.TAL
import           RPKI.Reporting
import           RPKI.Repository
import           RPKI.Http.Api
import           RPKI.Http.Types
import           RPKI.Http.Dto
import           RPKI.Http.UI
import           RPKI.Store.Base.Storage hiding (get)
import           RPKI.Store.Database
import           RPKI.Store.AppStorage
import           RPKI.Store.Types
import           RPKI.SLURM.Types
import           RPKI.Util
import           RPKI.SLURM.SlurmProcessing (applySlurmBgpSec)


httpServer :: (Storage s, MaintainableStorage s) => AppContext s -> Application
httpServer appContext = genericServe HttpApi {
        api     = apiServer,
        metrics = convert <$> textualMetrics,
        ui      = uiServer appContext,
        staticContent = serveDirectoryEmbedded $(embedRecursiveDir "static"),
        swagger = swaggerSchemaUIServer swaggerDoc
    }
  where
    apiServer = genericServer API {
        vrpsJson = getVRPValidated appContext,
        vrpsCsv = getVRPValidatedRaw appContext,
        vrpsCsvExt = getRoasValidatedRaw appContext,

        vrpsCsvFiltered  = getVRPSlurmedRaw appContext,
        vrpsJsonFiltered = getVRPSlurmed appContext,

        vrpsCsvUnique = getVRPsUniqueRaw appContext,
        vrpsJsonUnique = getVRPsUnique appContext,

        gbrs  = liftIO $ getGBRs appContext,
        -- splJson = liftIO $ getSpls appContext,
        aspas = liftIO $ getASPAs appContext,
        bgpCerts = liftIO $ getBGPCerts appContext,
        bgpCertsFiltered = liftIO $ getBGPCertsFiltered appContext,

        slurm = getSlurm appContext,
        slurms = getAllSlurms appContext,
        tals = getAllTALs appContext,

        fullValidationResults     = getValidationsDto appContext,
        minimalValidationResults  = toMinimalValidations <$> getValidationsDto appContext,
        originalValidationResults = getValidationsOriginalDto appContext,
        metrics = snd <$> getMetrics appContext,
        repositories = getPPs appContext,
        lmdbStats = getStats appContext,
        jobs = getJobs appContext,
        objectView = getRpkiObject appContext,
        originals  = getOriginal appContext,
        system = liftIO $ getSystem appContext,
        rtr = getRtr appContext,
        versions = getVersions appContext
    }

    uiServer AppContext {..} = do
        db <- liftIO $ readTVarIO database
        version <- liftIO $ roTx db $ \tx -> getLastValidationVersion db tx 
        case version of 
            Nothing -> notFoundException
            Just validationVersion -> do                  
                asyncFetch <- liftIO $ roTx db $ \tx -> runMaybeT $ do 
                                afVersion     <- MaybeT $ getLastAsyncFetchVersion appContext
                                afMetrics     <- MaybeT $ metricsForVersion tx db afVersion
                                afValidations <- MaybeT $ getValidationsForVersion appContext afVersion
                                resolvedValidations <- lift $ resolveVDto tx db afValidations
                                pure (afVersion, resolvedValidations, afMetrics)
                    
                validation <- liftIO $ roTx db $ \tx -> runMaybeT $ do                 
                                vMetrics     <- MaybeT $ metricsForVersion tx db validationVersion
                                vValidations <- MaybeT $ getValidationsForVersion appContext validationVersion
                                resolvedValidations <- lift $ resolveVDto tx db vValidations
                                pure (validationVersion, resolvedValidations, vMetrics)

                systemInfo <- liftIO $ readTVarIO $ appContext ^. #appState . #system
                pure $ mainPage systemInfo validation asyncFetch

getVRPValidated :: (MonadIO m, Storage s, MonadError ServerError m)
                => AppContext s -> Maybe Text -> m [VrpDto]
getVRPValidated appContext version =
    getValuesByVersion appContext version 
        (fmap (asMaybe . (^. #vrps)) . readTVar . (^. #validated)) 
        getVrps toVrpDtos

getVRPSlurmed :: (MonadIO m, Storage s, MonadError ServerError m)
                => AppContext s -> Maybe Text -> m [VrpDto]
getVRPSlurmed appContext version =
    getValuesByVersion appContext version 
        (fmap (asMaybe . (^. #vrps)) . readTVar . (^. #filtered)) 
        getVrps toVrpDtos 

getVRPValidatedRaw :: (MonadIO m, Storage s, MonadError ServerError m)
                    => AppContext s -> Maybe Text -> m RawCSV
getVRPValidatedRaw appContext version = 
    vrpDtosToCSV <$> getVRPValidated appContext version        

getVRPSlurmedRaw :: (MonadIO m, Storage s, MonadError ServerError m)
                    => AppContext s -> Maybe Text -> m RawCSV
getVRPSlurmedRaw appContext version = 
    vrpDtosToCSV <$> getVRPSlurmed appContext version

getVRPsUniqueRaw :: (MonadIO m, Storage s, MonadError ServerError m)
                    => AppContext s -> Maybe Text -> m RawCSV
getVRPsUniqueRaw appContext version = 
    vrpSetToCSV <$> 
        getValuesByVersion appContext version 
        (fmap (asMaybe . (^. #vrps)) . readTVar . (^. #filtered)) 
        getVrps toVrpSet

getVRPsUnique :: (MonadIO m, Storage s, MonadError ServerError m)
                    => AppContext s -> Maybe Text -> m [VrpMinimalDto]
getVRPsUnique appContext version = 
    getValuesByVersion appContext version 
        (fmap (asMaybe . (^. #vrps)) . readTVar . (^. #filtered)) 
        getVrps toVrpMinimalDtos


getRoasValidatedRaw :: (MonadIO m, Storage s, MonadError ServerError m)
                    => AppContext s -> Maybe Text -> m RawCSV
getRoasValidatedRaw appContext version =     
    getValuesByVersion appContext version  
        (\_ -> pure Nothing)
        getRoaDtos (vrpExtDtosToCSV . fromMaybe [])
  where
    getRoaDtos tx db version_ = do  
        getRoas tx db version_ >>= \case  
            Nothing       -> pure Nothing     
            Just (Roas r) -> 
                fmap (Just . mconcat . mconcat) $ 
                    forM (MonoidalMap.toList r) $ \(taName, r1) -> 
                        forM (MonoidalMap.toList r1) $ \(roaKey, vrps) ->                             
                            getLocationsByKey tx db roaKey >>= \case 
                                Nothing   -> pure []
                                Just locs -> 
                                    pure $! [ VrpExtDto { 
                                                    uri = toText $ pickLocation locs,
                                                    vrp = toVrpDto vrp taName
                                                } | vrp <- Set.toList vrps ] 
asMaybe :: (Eq a, Monoid a) => a -> Maybe a
asMaybe a = if mempty == a then Nothing else Just a


getValuesByVersion :: (MonadIO m, Storage s, MonadError ServerError m)
                    => AppContext s
                    -> Maybe Text
                    -> (AppState -> STM (Maybe v))   
                    -> (Tx s 'RO -> DB s -> WorldVersion -> IO (Maybe v))
                    -> (Maybe v -> a)                   
                    -> m a
getValuesByVersion AppContext {..} version readFromState readForVersion convertToResult = do
    case version of
        Nothing -> liftIO $ convertToResult <$> getLatest
        Just v  ->
            case parseWorldVersion v of
                Left e            -> throwError $ err400 { errBody = [i|'version' is not valid #{v}, error: #{e}|] }
                Right worlVersion -> convertToResult <$> getByVersion worlVersion
  where
    getLatest = do
        atomically (readFromState appState) >>= \case   
            Nothing -> 
                roTxT database $ \tx db -> 
                    getLastValidationVersion db tx >>= \case 
                        Nothing            -> pure Nothing
                        Just latestVersion -> readForVersion tx db latestVersion                    
            Just values -> 
                pure $ Just values

    getByVersion worldVersion = do        
        versions <- roTxT database allVersions        
        case filter ((worldVersion == ) . fst) versions of
            [] -> throwError $ err404 { errBody = [i|Version #{worldVersion} doesn't exist.|] }
            _  -> roTxT database $ \tx db -> 
                    readForVersion tx db worldVersion


getASPAs :: Storage s => AppContext s -> IO [AspaDto]
getASPAs AppContext {..} = do
    aspas <- getLatestAspas =<< readTVarIO database
    pure $ map aspaToDto $ Set.toList aspas

-- getSpls :: Storage s => AppContext s -> IO [SplDto]
-- getSpls AppContext {..} = do
--     aspas <- getLatestAspas =<< readTVarIO database
--     pure $ map aspaToDto $ Set.toList aspas

getBGPCerts :: Storage s => AppContext s -> IO [BgpCertDto]
getBGPCerts AppContext {..} =
    fmap (map bgpSecToDto . Set.toList) 
        $ getLatestBgps =<< readTVarIO database    

getBGPCertsFiltered :: Storage s => AppContext s -> IO [BgpCertDto]
getBGPCertsFiltered AppContext {..} = do
    db <- readTVarIO database
    fmap (fromMaybe mempty) $ roTx db $ \tx ->
        getLastValidationVersion db tx >>= \case      
            Nothing      -> pure mempty   
            Just version -> runMaybeT $ do 
                bgps  <- MaybeT $ getBgps tx db version
                slurm <- MaybeT $ slurmForVersion tx db version                        
                pure $ map bgpSecToDto $ Set.toList $ applySlurmBgpSec slurm bgps    
  
getGBRs :: Storage s => AppContext s -> IO [Located GbrDto]
getGBRs AppContext {..} = do
    gbrs <- getLatestGbrs =<< readTVarIO database
    pure [ Located { payload = gbrObjectToDto g, .. }
         | Located { payload = GbrRO g, .. } <- gbrs ]    
 

getValidationsForVersion :: Storage s => 
                            AppContext s 
                        -> WorldVersion 
                        -> IO (Maybe (ValidationsDto OriginalVDto))
getValidationsForVersion appContext worldVersion = 
    getValidationsImpl appContext (\_ _ -> pure $ Just worldVersion)

getValidationsOriginalDto :: (MonadIO m, Storage s, MonadError ServerError m) =>
                            AppContext s -> m (ValidationsDto OriginalVDto)
getValidationsOriginalDto appContext = do
    vs <- liftIO (getValidationsImpl appContext getLastValidationVersion)
    maybe notFoundException pure vs
    
getValidationsDto :: (MonadIO m, Storage s, MonadError ServerError m) =>
                    AppContext s -> m (ValidationsDto ResolvedVDto)
getValidationsDto appContext@AppContext {..} = do
    liftIO (getValidationsImpl appContext getLastValidationVersion) >>= \case 
        Nothing -> notFoundException
        Just vs -> roTxT database $ \tx db -> resolveVDto tx db vs        
    

getValidationsImpl :: Storage s => 
                    AppContext s    
                -> (DB s -> Tx s 'RO -> IO (Maybe WorldVersion))
                -> IO (Maybe (ValidationsDto OriginalVDto))
getValidationsImpl AppContext {..} getVersionF = do
    db <- readTVarIO database
    roTx db $ \tx ->
        runMaybeT $ do
            version     <- MaybeT $ getVersionF db tx
            (Validations vMap) <- MaybeT $ validationsForVersion tx db version
            let validationDtos = map toVDto $ Map.toList vMap
            pure $ ValidationsDto {
                    worldVersion = version,
                    timestamp    = versionToMoment version,
                    validations  = validationDtos
                }                

getLastAsyncFetchVersion :: Storage s => AppContext s -> IO (Maybe WorldVersion)
getLastAsyncFetchVersion appContext = getLastKindVersion appContext asyncFetchKind

getLastKindVersion :: Storage s => AppContext s -> VersionKind -> IO (Maybe WorldVersion)
getLastKindVersion AppContext {..} versionKind = do
    db <- readTVarIO database
    roTx db $ \tx -> getLastVersionOfKind db tx versionKind

getMetrics :: (MonadIO m, Storage s, MonadError ServerError m) =>
            AppContext s -> m (RawMetric, MetricsDto)
getMetrics appContext = getMetricsImpl appContext getLastValidationVersion    

getMetricsForVersion :: (MonadIO m, Storage s, MonadError ServerError m) =>
                        AppContext s 
                    -> WorldVersion 
                    -> m (RawMetric, MetricsDto)
getMetricsForVersion appContext worldVersion = 
    getMetricsImpl appContext (\_ _ -> pure $ Just worldVersion)

getMetricsImpl :: (MonadIO m, Storage s, MonadError ServerError m) =>
                AppContext s 
            -> (DB s -> Tx s 'RO -> IO (Maybe WorldVersion))
            -> m (RawMetric, MetricsDto)
getMetricsImpl AppContext {..} getVersionF = do
    db <- liftIO $ readTVarIO database
    metrics <- liftIO $ roTx db $ \tx ->
        runMaybeT $ do
            version <- MaybeT $ getVersionF db tx
            rawMetrics  <- MaybeT $ metricsForVersion tx db version
            pure (rawMetrics, toMetricsDto rawMetrics)
    maybe notFoundException pure metrics


notFoundException :: MonadError ServerError m => m a
notFoundException = throwError err404 { 
    errBody = "No finished validations yet." }

getSlurm :: (MonadIO m, Storage s, MonadError ServerError m) =>
            AppContext s -> m Slurm
getSlurm AppContext {..} = do
    db <- liftIO $ readTVarIO database
    z  <- liftIO $ roTx db $ \tx ->
        runMaybeT $ do
                lastVersion <- MaybeT $ getLastValidationVersion db tx
                MaybeT $ slurmForVersion tx db lastVersion
    case z of
        Nothing -> throwError err404 { errBody = "No SLURM for this version" }
        Just m  -> pure m

getAllSlurms :: (MonadIO m, Storage s, MonadError ServerError m) =>
                AppContext s -> m [(WorldVersion, Slurm)]
getAllSlurms AppContext {..} = do
    db <- liftIO $ readTVarIO database
    liftIO $ roTx db $ \tx -> do
        versions <- List.sortOn Down <$> validationVersions tx db
        slurms   <- mapM (\wv -> (wv, ) <$> slurmForVersion tx db wv) versions        
        pure [ (w, s) | (w, Just s) <- slurms ]

getAllTALs :: (MonadIO m, Storage s, MonadError ServerError m) =>
                AppContext s -> m [TalDto]
getAllTALs AppContext {..} = do
    db <- liftIO $ readTVarIO database
    liftIO $ roTx db $ \tx -> do        
        tas <- getTAs tx db     
        pure [ TalDto {..} | (_, StorableTA {..}) <- tas, 
                let repositories = map (toText . getRpkiURL) 
                        $ NonEmpty.toList 
                        $ unPublicationPointAccess initialRepositories ]           


getStats :: (MonadIO m, MaintainableStorage s, Storage s) => AppContext s -> m TotalDBStats
getStats appContext = liftIO $ do 
    storageStats <- getStorageStats appContext
    let total = totalStats storageStats    
    fileSize <- getCacheFsSize appContext
    let fileStats = DBFileStats {..}
    pure TotalDBStats {..}


getJobs :: (MonadIO m, Storage s) => AppContext s -> m JobsDto
getJobs AppContext {..} = liftIO $ do
    db   <- readTVarIO database
    jobs <- roTx db $ \tx -> allJobs tx db
    pure JobsDto {..}

getPPs :: (MonadIO m, Storage s) => AppContext s -> m PublicationPointDto
getPPs AppContext {..} = liftIO $ do
    db <- readTVarIO database
    pps <- roTx db $ \tx -> getPublicationPoints tx db
    pure $ toPublicationPointDto pps

getRpkiObject :: (MonadIO m, Storage s, MonadError ServerError m)
                => AppContext s
                -> Maybe Text
                -> Maybe Text
                -> Maybe Text
                -> m [RObject]
getRpkiObject AppContext {..} uri hash key =
    case (uri, hash, key) of
        (Nothing,  Nothing, Nothing) ->
            throwError $ err400 { errBody = "'uri', 'hash' or 'key' must be provided." }

        (Just u, Nothing, Nothing) ->
            case parseRpkiURL u of
                Left _ ->
                    throwError $ err400 { errBody = "'uri' is not a valid object URL." }

                Right rpkiUrl ->                     
                    roTxT database $ \tx db ->
                        getByUri tx db rpkiUrl >>= \case     
                            [] -> do                                
                                -- try TA certificates
                                tas <- getTAs tx db                                 
                                pure [ locatedDto (Located locations (CerRO taCert)) | 
                                        (_, StorableTA {..}) <- tas, 
                                        let locations = talCertLocations tal, 
                                        oneOfLocations locations rpkiUrl ]                                
                                
                            os -> pure $ map locatedDto os
                        
        (Nothing, Just hash', Nothing) ->
            case parseHash hash' of
                Left _  -> throwError err400
                Right h -> 
                    roTxT database $ \tx db ->
                        (locatedDto <$>) . maybeToList <$> getByHash tx db h

        (Nothing, Nothing, Just key') ->
            case readMaybe (convert key') of
                Nothing -> 
                    throwError err400 { errBody = convert $ "Could not parse integer key " <> key' } 
                Just (k :: Integer) -> do 
                    let kk = ObjectKey $ ArtificialKey $ fromIntegral k
                    roTxT database $ \tx db ->
                        (locatedDto <$>) . maybeToList <$> getLocatedByKey tx db kk
        _ ->
            throwError $ err400 { errBody =
                "Only one of 'uri', 'hash' or 'key' must be provided." }
  where
    locatedDto located = RObject $ located & #payload %~ objectToDto

getOriginal :: (MonadIO m, Storage s, MonadError ServerError m)
                => AppContext s
                -> Maybe Text           
                -> m ObjectOriginal
getOriginal AppContext {..} hash =
    case hash of
        Nothing ->
            throwError $ err400 { errBody = "or 'hash' must be provided." }
                        
        Just hash' ->
            case parseHash hash' of
                Left _  -> throwError err400
                Right h -> do
                    z <- roTxT database $ \tx db -> getOriginalBlobByHash tx db h
                    case z of 
                        Nothing -> throwError err404
                        Just b  -> pure b


{- 
getOriginal :: (MonadIO m, Storage s, MonadError ServerError m)
                => AppContext s
                -> Maybe Text           
                -> m ObjectOriginal
getOriginal AppContext {..} hash =
    case (uri, hash) of
        (Nothing,  Nothing) ->
            throwError $ err400 { errBody = "'uri' or 'hash' must be provided." }

        (Just u, Nothing) ->
            case parseRpkiURL u of
                Left _ ->
                    throwError $ err400 { errBody = "'uri' is not a valid object URL." }

                Right rpkiUrl ->                     
                    roTxT database $ \tx db -> 
                        getKeysByUri tx db rpkiUrl >>= \case     
                            []    -> pure [] 
                            [key] -> (locatedDto <$>) <$> getObjectByKey tx db key

                        getByUri tx db rpkiUrl >>= \case     
                            [] -> do                                
                                -- try TA certificates
                                tas <- getTAs tx db                                 
                                pure [ locatedDto (Located locations (CerRO taCert)) | 
                                        (_, StorableTA {..}) <- tas, 
                                        let locations = talCertLocations tal, 
                                        oneOfLocations locations rpkiUrl ]                                
                                
                            os -> pure $ map locatedDto os
                        
        (Nothing, Just hash') ->
            case parseHash hash' of
                Left _  -> throwError err400
                Right h -> do
                    z <- roTxT database $ \tx db -> getOriginalBlobByHash tx db h
                    case z of 
                        Nothing -> throwError err404
                        Just b  -> pure b

        (Just _, Just _) ->
            throwError $ err400 { errBody =
                "Only 'uri' or 'hash' must be provided, not both." }                        
  where
    locatedDto located = RObject $ located & #payload %~ objectToDto  
-}



getSystem :: Storage s =>  AppContext s -> IO SystemDto
getSystem AppContext {..} = do
    now <- unNow <$> thisInstant
    SystemInfo {..} <- readTVarIO $ appState ^. #system
    let proverVersion = rpkiProverVersion    
    
    let z = MonoidalMap.toList $ unMetricMap $ metrics ^. #resources
    resources <- forM z $ \(scope, resourceUsage) -> do  
                    let aggregatedCpuTime = resourceUsage ^. #aggregatedCpuTime
                    let aggregatedClockTime = resourceUsage ^. #aggregatedClockTime
                    let maxMemory = resourceUsage ^. #maxMemory
                    let avgCpuTimeMsPerSecond = cpuTimePerSecond aggregatedCpuTime startUpTime now
                    tag <- fmtScope scope
                    pure ResourcesDto {..}

    pure SystemDto {..}  
  where
    fmtScope scope =
        fmap (Text.intercalate "/") $
            roTxT database $ \tx db -> do         
                forM (scopeList scope) $ \s -> 
                    resolvedFocusToText <$> resolveLocations tx db s 



getRtr :: (MonadIO m, Storage s, MonadError ServerError m) =>
            AppContext s -> m RtrDto
getRtr AppContext {..} = do
    liftIO (readTVarIO $ appState ^. #rtrState) >>= \case
        Nothing -> throwError $ err400 { errBody =
                "RTR state doesn't exist, RTR server is waiting for a validation result or disabled." }
        Just rtrState -> pure RtrDto {..}

getVersions :: (MonadIO m, Storage s, MonadError ServerError m) =>
                AppContext s -> m [(WorldVersion, VersionKind)]
getVersions AppContext {..} = liftIO $ do
    db <- readTVarIO database
    -- Sort versions from latest to earliest
    List.sortOn (Down . fst) <$> roTx db (`allVersions` db)


resolveVDto :: (MonadIO m, Storage s) => 
                Tx s 'RO
                -> DB s 
                -> ValidationsDto OriginalVDto 
                -> m (ValidationsDto ResolvedVDto)
resolveVDto tx db vs = liftIO $ 
    #validations (mapM resolveOrigDto) vs
  where
    resolveOrigDto (OriginalVDto fd) = 
        fmap ResolvedVDto $ #path (mapM (resolveLocations tx db)) fd
    

resolveLocations :: Storage s => 
                   Tx s 'RO
                -> DB s 
                -> Focus 
                -> IO FocusResolvedDto
resolveLocations tx db = \case 
    TAFocus t               -> pure $ TextDto t
    TextFocus t             -> pure $ TextDto t
    PPFocus u               -> pure $ DirectLink $ toText u
    RepositoryFocus u       -> pure $ DirectLink $ toText u    
    LocationFocus (URI uri) -> pure $ ObjectLink uri
    LinkFocus (URI uri)     -> pure $ DirectLink uri
    ObjectFocus key         -> locations key
    HashFocus hash          -> getKeyByHash tx db hash >>= \case 
                                    Nothing  -> pure $ TextDto [i|Can't find key for hash #{hash}|]
                                    Just key -> locations key        
  where
    locations key = do 
        getLocationsByKey tx db key >>= \case 
            Nothing  -> pure $ TextDto [i|Can't find locations for key #{key}|]
            Just loc -> pure $ ObjectLink $ toText $ pickLocation loc

    
