{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module RPKI.Http.HttpServer where

import           Control.Lens

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Error.Class

import           FileEmbedLzma

import           Servant.Server.Generic
import           Servant hiding (contentType)
import           Servant.Swagger.UI

import           Data.Ord
import           Data.List                        (sortBy)
import           Data.Maybe                       (maybeToList, fromMaybe)
import qualified Data.Set                         as Set
import qualified Data.List                        as List
import qualified Data.Map.Monoidal.Strict         as MonoidalMap
import qualified Data.List.NonEmpty               as NonEmpty
import           Data.Text                        (Text)
import           Data.String.Interpolate.IsString

import           RPKI.AppContext
import           RPKI.AppTypes
import           RPKI.AppState
import           RPKI.Domain
import           RPKI.Config
import           RPKI.Metrics.Prometheus
import           RPKI.Metrics.System
import           RPKI.Time
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


httpApi :: (Storage s, MaintainableStorage s) => AppContext s -> Application
httpApi appContext = genericServe HttpApi {
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

        vrpsCsvFiltered  = getVRPSlurmedRaw appContext,
        vrpsJsonFiltered = getVRPSlurmed appContext,

        vrpsCsvUnique = getVRPsUniqueRaw appContext,
        vrpsJsonUnique = getVRPsUnique appContext,

        gbrs  = liftIO $ getGBRs appContext,
        aspas = liftIO $ getASPAs appContext,
        bgpCerts = liftIO $ getBGPCerts appContext,
        bgpCertsFiltered = liftIO $ getBGPCertsFiltered appContext,

        slurm = getSlurm appContext,
        slurms = getAllSlurms appContext,
        tals = getAllTALs appContext,

        fullValidationResults    = getValidationsDto appContext,
        validationResultsMinimal = toMinimalValidations <$> getValidationsDto appContext,
        metrics = snd <$> getMetrics appContext,
        repositories = getPPs appContext,
        lmdbStats = getStats appContext,
        jobs = getJobs appContext,
        objectView = getRpkiObject appContext,
        system = liftIO $ getSystem appContext,
        rtr = getRtr appContext,
        versions = getVersions appContext
    }

    uiServer AppContext {..} = do
        version <- liftIO $ getLastValidationVersion appContext        
        case version of 
            Nothing -> notFoundException
            Just validationVersion -> do  
                db <- liftIO $ readTVarIO database
                af <- liftIO $ roTx db $ \tx -> runMaybeT $ do 
                        afVersion     <- MaybeT $ getLastAsyncFetchVersion appContext
                        afMetrics     <- MaybeT $ metricsForVersion tx db afVersion
                        afValidations <- MaybeT $ getValidationsForVersion appContext afVersion
                        pure (afVersion, afValidations, afMetrics)
                    
                vv <- liftIO $ roTx db $ \tx -> runMaybeT $ do                 
                        vMetrics     <- MaybeT $ metricsForVersion tx db validationVersion
                        vValidations <- MaybeT $ getValidationsForVersion appContext validationVersion
                        pure (validationVersion, vValidations, vMetrics)

                pure $ mainPage vv af

getVRPValidated :: (MonadIO m, Storage s, MonadError ServerError m)
                => AppContext s -> Maybe Text -> m [VrpDto]
getVRPValidated appContext version =
    getVRPs appContext version (fmap (^. #vrps) . readTVar . (^. #validated)) toVrpDtos

getVRPSlurmed :: (MonadIO m, Storage s, MonadError ServerError m)
                => AppContext s -> Maybe Text -> m [VrpDto]
getVRPSlurmed appContext version =
    getVRPs appContext version (fmap (^. #vrps) . readTVar . (^. #filtered)) toVrpDtos

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
    vrpSetToCSV <$> getVRPs appContext version (fmap (^. #vrps) . readTVar . (^. #filtered)) toVrpSet

getVRPsUnique :: (MonadIO m, Storage s, MonadError ServerError m)
                    => AppContext s -> Maybe Text -> m [VrpMinimalDto]
getVRPsUnique appContext version = 
    getVRPs appContext version (fmap (^. #vrps) . readTVar . (^. #filtered)) toVrpMinimalDtos


getVRPs :: (MonadIO m, Storage s, MonadError ServerError m)
        => AppContext s
        -> Maybe Text
        -> (AppState -> STM Vrps)        
        -> (Maybe Vrps -> a)        
        -> m a
getVRPs AppContext {..} version readVrps convertVrps = do
    case version of
        Nothing -> liftIO getLatest
        Just v  ->
            case parseWorldVersion v of
                Left e            -> throwError $ err400 { errBody = [i|'version' is not valid #{v}, error: #{e}|] }
                Right worlVersion -> convertVrps <$> getByVersion worlVersion
  where
    getLatest =
        convertVrps <$> do
            vrps <- atomically (readVrps appState)
            if vrps == mempty
                then
                    getLatestVRPs =<< readTVarIO database
                else
                    pure $ Just vrps

    getByVersion worldVersion = do
        db       <- liftIO $ readTVarIO database
        versions <- liftIO $ roTx db (`allVersions` db)
        case filter ((worldVersion == ) . fst) versions of
            [] -> throwError $ err404 { errBody = [i|Version #{worldVersion} doesn't exist.|] }
            _  -> liftIO $ roTx db $ \tx -> getVrps tx db worldVersion


getASPAs :: Storage s => AppContext s -> IO [AspaDto]
getASPAs AppContext {..} = do
    aspas <- getLatestAspas =<< readTVarIO database
    pure $ map aspaToDto $ Set.toList aspas

getBGPCerts :: Storage s => AppContext s -> IO [BgpCertDto]
getBGPCerts AppContext {..} =
    fmap (map bgpSecToDto . Set.toList) 
        $ getLatestBgps =<< readTVarIO database    

getBGPCertsFiltered :: Storage s => AppContext s -> IO [BgpCertDto]
getBGPCertsFiltered AppContext {..} = do
    db <- readTVarIO database
    fmap (fromMaybe mempty) $ roTx db $ \tx ->
        getLastCompletedVersion db tx >>= \case      
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


getValidations :: Storage s => AppContext s -> IO (Maybe (ValidationsDto FullVDto))
getValidations appContext = getValidationsImpl appContext 
    (\db tx -> getLastVersionOfKind db tx validationKind)

getValidationsForVersion :: Storage s => 
                            AppContext s 
                        -> WorldVersion 
                        -> IO (Maybe (ValidationsDto FullVDto))
getValidationsForVersion appContext worldVersion = 
    getValidationsImpl appContext (\_ _ -> pure $ Just worldVersion)

getValidationsImpl :: Storage s => 
                    AppContext s 
                -> (DB s -> Tx s 'RO -> IO (Maybe WorldVersion))
                -> IO (Maybe (ValidationsDto FullVDto))
getValidationsImpl AppContext {..} getVersionF = do
    db <- readTVarIO database
    roTx db $ \tx ->
        runMaybeT $ do
            version     <- MaybeT $ getVersionF db tx
            validations <- MaybeT $ validationsForVersion tx db version
            let validationDtos = map toVR $ validationsToList validations
            pure $ ValidationsDto {
                    worldVersion = version,
                    timestamp    = versionToMoment version,
                    validations  = validationDtos
                }                

getValidationsDto :: (MonadIO m, Storage s, MonadError ServerError m) =>
                    AppContext s -> m (ValidationsDto FullVDto)
getValidationsDto appContext = do
    vs <- liftIO $ getValidations appContext
    maybe notFoundException pure vs

getLastVersion :: Storage s => AppContext s -> IO (Maybe WorldVersion)
getLastVersion AppContext {..} = do
    db <- readTVarIO database
    roTx db $ getLastCompletedVersion db

getLastValidationVersion :: Storage s => AppContext s -> IO (Maybe WorldVersion)
getLastValidationVersion appContext = getLastKindVersion appContext validationKind

getLastAsyncFetchVersion :: Storage s => AppContext s -> IO (Maybe WorldVersion)
getLastAsyncFetchVersion appContext = getLastKindVersion appContext asyncFetchKind

getLastKindVersion :: Storage s => AppContext s -> VersionKind -> IO (Maybe WorldVersion)
getLastKindVersion AppContext {..} versionKind = do
    db <- readTVarIO database
    roTx db $ \tx -> getLastVersionOfKind db tx versionKind

getMetrics :: (MonadIO m, Storage s, MonadError ServerError m) =>
            AppContext s -> m (RawMetric, MetricsDto)
getMetrics appContext = getMetricsImpl appContext 
    (\db tx -> getLastVersionOfKind db tx validationKind)

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
            lastVersion <- MaybeT $ getVersionF db tx
            rawMetrics  <- MaybeT $ metricsForVersion tx db lastVersion
            pure (rawMetrics, toMetricsDto rawMetrics)
    maybe notFoundException pure metrics


notFoundException :: MonadError ServerError m => m a
notFoundException = throwError err404 { errBody = "Working, please hold still!" }

getSlurm :: (MonadIO m, Storage s, MonadError ServerError m) =>
            AppContext s -> m Slurm
getSlurm AppContext {..} = do
    db <- liftIO $ readTVarIO database
    z  <- liftIO $ roTx db $ \tx ->
        runMaybeT $ do
                lastVersion <- MaybeT $ getLastCompletedVersion db tx
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
getStats appContext@AppContext {..} = liftIO $ do 
    (dbStats, total) <- getTotalDbStats =<< readTVarIO database
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
                -> m [RObject]
getRpkiObject AppContext {..} uri hash =
    case (uri, hash) of
        (Nothing,  Nothing) ->
            throwError $ err400 { errBody = "'uri' or 'hash' must be provided." }

        (Just u, Nothing) ->
            case parseRpkiURL u of
                Left _ ->
                    throwError $ err400 { errBody = "'uri' is not a valid object URL." }

                Right rpkiUrl -> liftIO $ do
                    db <- readTVarIO database
                    roTx db $ \tx ->
                        (locatedDto <$>) <$> getByUri tx db rpkiUrl

        (Nothing, Just hash') ->
            case parseHash hash' of
                Left _  -> throwError err400
                Right h -> liftIO $ do
                    db <- readTVarIO database
                    roTx db $ \tx -> 
                        (locatedDto <$>) . maybeToList <$> getByHash tx db h

        (Just _, Just _) ->
            throwError $ err400 { errBody =
                "Only 'uri' or 'hash' must be provided, not both." }
  where
    locatedDto located = RObject $ located & #payload %~ objectToDto


getSystem :: Storage s =>  AppContext s -> IO SystemDto
getSystem AppContext {..} = do
    now <- unNow <$> thisInstant
    SystemInfo {..} <- readTVarIO $ appState ^. #system
    let proverVersion = getVersion

    let toResources (scope, resourceUsage) = let
                tag = scopeText scope
                aggregatedCpuTime = resourceUsage ^. #aggregatedCpuTime
                maxMemory = resourceUsage ^. #maxMemory
                avgCpuTimeMsPerSecond = cpuTimePerSecond aggregatedCpuTime startUpTime now
            in ResourcesDto {..}

    let resources = map toResources $ MonoidalMap.toList $ unMetricMap $ metrics ^. #resources        
    pure SystemDto {..}

getRtr :: (MonadIO m, Storage s, MonadError ServerError m) =>
            AppContext s -> m RtrDto
getRtr AppContext {..} = do
    liftIO (readTVarIO $ appState ^. #rtrState) >>= \case
        Nothing -> throwError $ err400 { errBody =
                "RTR state doesn't exist, RTR server is waiting for a validation result or disabled." }
        Just rtrState -> pure RtrDto {..}

getVersions :: (MonadIO m, Storage s, MonadError ServerError m) =>
                AppContext s -> m [WorldVersion]
getVersions AppContext {..} = liftIO $ do
    db <- readTVarIO database
    -- Sort versions from latest to earliest
    sortBy (flip compare) . map fst <$> roTx db (`allVersions` db)
