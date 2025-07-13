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
import           Control.Monad.Trans.Maybe
import           Control.Monad.Error.Class

import           Data.Generics.Product.Typed

import           FileEmbedLzma

import           Servant.Server.Generic
import           Servant hiding (contentType, URI)
import           Servant.Swagger.UI

import           Data.Maybe                       (maybeToList, fromMaybe, fromJust, catMaybes)
import qualified Data.Set                         as Set
import qualified Data.List                        as List
import qualified Data.Map.Strict                  as Map
import qualified Data.Map.Monoidal.Strict         as MonoidalMap
import qualified Data.List.NonEmpty               as NonEmpty
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Tuple.Strict
import qualified Data.Vector                      as V
import           Data.String.Interpolate.IsString

import           Text.Read                        (readMaybe)

import           RPKI.AppContext
import           RPKI.AppTypes
import           RPKI.AppState
import           RPKI.Domain
import           RPKI.Logging
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
import           RPKI.Store.Database    (DB)
import qualified RPKI.Store.Database    as DB
import           RPKI.Store.AppStorage
import           RPKI.Store.Types
import           RPKI.SLURM.Types
import           RPKI.Resources.Resources
import           RPKI.Resources.Validity
import           RPKI.Util
import           RPKI.SLURM.SlurmProcessing (applySlurmBgpSec)
import           RPKI.Version


httpServer :: (Storage s, MaintainableStorage s) => AppContext s -> [TAL] -> Application
httpServer appContext tals = genericServe HttpApi {
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

        gbrs  = getGbrs_ appContext,
        splJson = getSpls_ appContext,
        aspas = getAspas_ appContext,
        bgpCerts = getBgps_ appContext,
        bgpCertsFiltered = getBGPCertsFiltered_ appContext,

        slurm = getSlurm appContext,
        slurms = getAllSlurms appContext,

        fullValidationResults     = getValidationsDto appContext,
        minimalValidationResults  = fmap toMinimalValidations . getValidationsDto appContext,
        originalValidationResults = getValidationsOriginalDto appContext,
        metrics = getMetrics appContext,
        repositories = getPPs appContext,
        lmdbStats = getStats appContext,
        jobs = getJobs appContext,
        objectView = getRpkiObject appContext,
        originals  = getOriginal appContext,
        manifests  = getManifests appContext,
        system = liftIO $ getSystem appContext,
        rtr = getRtr appContext,
        versions = getVersions appContext,
        fetcheables = getFetcheables appContext,
        validity = getPrefixValidity appContext,
        validityAsnPrefix = getQueryPrefixValidity appContext,
        validityBulk = getBulkPrefixValidity appContext
    }

    uiServer AppContext {..} = do
        db <- liftIO $ readTVarIO database
        version <- liftIO $ roTx db $ \tx -> DB.getLatestVersion tx db 
        case version of 
            Nothing                      -> throwError err404 { errBody = "No finished validations yet." }
            Just latestValidationVersion -> do                      
                liftIO $ roTx db $ \tx -> do                    
                    (commonValidations, commonMetrics, perTaOutcomes) <- 
                        DB.getValidationOutcomes tx db latestValidationVersion

                    let metricsDto = toMetricsDto commonMetrics (fmap snd perTaOutcomes)

                    resolvedValidations <- traverse (mapM (resolveOriginalDto tx db) . toVDtos . fst) perTaOutcomes
                    resolvedCommons     <- mapM (resolveOriginalDto tx db) $ toVDtos commonValidations                    

                    
                    allFetcheables      <- do 
                            fetcheables <- readTVarIO $ appState ^. #fetcheables
                            pure $ mconcat $ map (uncurry Set.insert) $ MonoidalMap.toList $ unFetcheables fetcheables
                            
                    fetchesDtos         <- toRepositoryDtos appContext =<< DB.getRepositories tx db (`Set.member` allFetcheables)
                    systemInfo          <- readTVarIO $ appContext ^. #appState . #system

                    pure $ mainPage
                            latestValidationVersion
                            systemInfo
                            resolvedValidations
                            resolvedCommons
                            fetchesDtos
                            metricsDto


getVRPValidated :: (MonadIO m, Storage s, MonadError ServerError m)
                => AppContext s -> Maybe Text -> m [VrpDto]
getVRPValidated appContext version =
    getValuesByVersion appContext version 
        (fmap (asMaybe . (^. #vrps)) . readTVar . (^. #validated)) 
        (\tx db v -> Just <$> DB.getVrps tx db v) 
        (toVrpDtos . fromMaybe mempty)

getVRPSlurmed :: (MonadIO m, Storage s, MonadError ServerError m)
                => AppContext s -> Maybe Text -> m [VrpDto]
getVRPSlurmed appContext version =
    getValuesByVersion appContext version 
        (fmap (asMaybe . (^. #vrps)) . readTVar . (^. #filtered)) 
        (\tx db v -> Just <$> DB.getVrps tx db v) 
        (toVrpDtos . fromMaybe mempty)

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
        (\tx db v -> Just <$> DB.getVrps tx db v) 
        (toVrpV . (allTAs <$>))

getVRPsUnique :: (MonadIO m, Storage s, MonadError ServerError m)
                    => AppContext s -> Maybe Text -> m [VrpMinimalDto]
getVRPsUnique appContext version = 
    getValuesByVersion appContext version 
        (fmap (asMaybe . (^. #vrps)) . readTVar . (^. #filtered)) 
        (\tx db v -> Just <$> DB.getVrps tx db v) 
        (toVrpMinimalDtos . (allTAs <$>))        


getRoasValidatedRaw :: (MonadIO m, Storage s, MonadError ServerError m)
                    => AppContext s -> Maybe Text -> m RawCSV
getRoasValidatedRaw appContext version =     
    getValuesByVersion appContext version  
        (\_ -> pure Nothing)
        getRoaDtos (vrpExtDtosToCSV . fromMaybe [])
  where
    getRoaDtos tx db version_ = do  
        roas <- DB.getRoas tx db version_
        fmap (Just . mconcat . mconcat) $ 
            forM (perTA roas) $ \(taName, Roas r) -> 
                forM (MonoidalMap.toList r) $ \(roaKey, vrps) ->                             
                    DB.getLocationsByKey tx db roaKey >>= \case 
                        Nothing   -> pure []
                        Just locs -> do 
                            let uri = toText $ pickLocation locs
                            pure $! [ VrpExtDto { vrp = toVrpDto vrp taName, .. } | vrp <- V.toList vrps ] 

asMaybe :: (Eq a, Monoid a) => a -> Maybe a
asMaybe a = if mempty == a then Nothing else Just a


getValuesByVersion :: (MonadIO m, Storage s, MonadError ServerError m)
                    => AppContext s
                    -> Maybe Text
                    -> (AppState -> STM (Maybe v))   
                    -> (Tx s 'RO -> DB.DB s -> WorldVersion -> IO (Maybe v))
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
                    DB.getLatestVersion tx db >>= \case 
                        Nothing            -> pure Nothing
                        Just latestVersion -> readForVersion tx db latestVersion                    
            Just values -> 
                pure $ Just values

    getByVersion worldVersion = do        
        versions <- roTxT database DB.versionsBackwards        
        case filter ((worldVersion == ) . fst) versions of
            [] -> throwError $ err404 { errBody = [i|Version #{worldVersion} doesn't exist.|] }
            _  -> roTxT database $ \tx db -> 
                    readForVersion tx db worldVersion


getAspas_ :: (MonadIO m, Storage s, MonadError ServerError m) => 
             AppContext s -> Maybe Text -> m [AspaDto]
getAspas_ appContext version = 
    getValuesByVersion appContext version  
        (\_ -> pure Nothing) DB.getAspas toDtos
  where
    toDtos = map aspaToDto . maybe [] Set.toList


getSpls_ :: (MonadIO m, Storage s, MonadError ServerError m) => 
           AppContext s -> Maybe Text -> m [SplDto]
getSpls_ appContext version =
    getValuesByVersion appContext version  
        (\_ -> pure Nothing) DB.getSpls toDtos
  where
    toDtos spls = 
        map (\(SplN asn prefix) -> SplDto {..}) $ 
            maybe [] Set.toList spls

getBgps_ :: (MonadIO m, Storage s, MonadError ServerError m) => 
           AppContext s -> Maybe Text -> m [BgpCertDto]
getBgps_ appContext version =
    getValuesByVersion appContext version  
        (\_ -> pure Nothing) DB.getBgps toDtos
  where
    toDtos = map bgpSecToDto . maybe [] Set.toList


getBGPCertsFiltered_ :: (MonadIO m, Storage s, MonadError ServerError m) => 
                        AppContext s -> Maybe Text -> m [BgpCertDto]
getBGPCertsFiltered_ appContext version =
    getValuesByVersion appContext version  
        (\_ -> pure Nothing) getSlurmedBgps toDtos
  where
    toDtos = map bgpSecToDto . maybe [] Set.toList

    getSlurmedBgps tx db v = runMaybeT $ do 
        bgps  <- MaybeT $ DB.getBgps tx db v
        slurm <- MaybeT $ DB.getSlurm tx db v                        
        pure $ applySlurmBgpSec slurm bgps    


getGbrs_ :: (MonadIO m, Storage s, MonadError ServerError m) => 
            AppContext s -> Maybe Text -> m [Located GbrDto]
getGbrs_ appContext version = 
    getValuesByVersion appContext version  
        (\_ -> pure Nothing) 
        (\tx db v -> Just <$> DB.getGbrObjects tx db v) toDtos
  where
    toDtos gbrs = 
        [ Located { payload = gbrObjectToDto g, .. }
        | Located { payload = GbrRO g, .. } <- fromMaybe [] gbrs ]    
 

getValidationsOriginalDto :: (MonadIO m, Storage s, MonadError ServerError m) =>
                            AppContext s 
                        -> Maybe Text 
                        -> m (ValidationsDto OriginalVDto)
getValidationsOriginalDto appContext versionText = do
    getValuesByVersion appContext versionText
        (\_ -> pure Nothing) 
        (\tx db version ->
            Just . validationsToDto version . allTAs <$> DB.getValidationsPerTA tx db version)
        fromJust
        
getValidationsDto :: (MonadIO m, Storage s, MonadError ServerError m) =>
                    AppContext s 
                    -> Maybe Text 
                    -> m (ValidationsDto ResolvedVDto)
getValidationsDto appContext versionText = 
    getValuesByVersion appContext versionText
        (\_ -> pure Nothing) 
        (\tx db version -> do 
            originalDtos <- validationsToDto version . allTAs <$> DB.getValidationsPerTA tx db version            
            Just <$> resolveValidationDto tx db originalDtos)
        fromJust

getMetrics :: (MonadIO m, Storage s, MonadError ServerError m) =>
            AppContext s 
        -> Maybe Text 
        -> m MetricsDto
getMetrics appContext versionText = 
    getValuesByVersion appContext versionText
        (\_ -> pure Nothing) 
        (\tx db version -> 
            fmap Just $ toMetricsDto <$> 
                            DB.getCommonMetrics tx db version <*> 
                            DB.getMetricsPerTA tx db version)
        fromJust

getSlurm :: (MonadIO m, Storage s, MonadError ServerError m) =>
            AppContext s -> m Slurm
getSlurm AppContext {..} = do
    db <- liftIO $ readTVarIO database
    z  <- liftIO $ roTx db $ \tx ->
        runMaybeT $ do
                lastVersion <- MaybeT $ DB.getLatestVersion tx db
                MaybeT $ DB.getSlurm tx db lastVersion
    case z of
        Nothing -> throwError err404 { errBody = "No SLURM for this version" }
        Just m  -> pure m

getAllSlurms :: (MonadIO m, Storage s, MonadError ServerError m) =>
                AppContext s -> m [(WorldVersion, Slurm)]
getAllSlurms AppContext {..} = 
    liftIO $ roTxT database $ \tx db -> do
        versions <- DB.versionsBackwards tx db
        slurms   <- mapM (\(wv, _) -> (wv, ) <$> DB.getSlurm tx db wv) versions        
        pure [ (w, s) | (w, Just s) <- slurms ]

    

getStats :: (MonadIO m, MaintainableStorage s, Storage s) => AppContext s -> m TotalDBStats
getStats appContext = liftIO $ do 
    storageStats <- getStorageStats appContext
    let total = DB.totalStats storageStats    
    fileSize <- getCacheFsSize appContext
    let fileStats = DBFileStats {..}
    pure TotalDBStats {..}


getJobs :: (MonadIO m, Storage s) => AppContext s -> m JobsDto
getJobs AppContext {..} = liftIO $ do
    db   <- readTVarIO database
    jobs <- roTx db $ \tx -> DB.allJobs tx db
    pure JobsDto {..}

getPPs :: (MonadIO m, Storage s) => AppContext s -> m PublicationPointsDto
getPPs AppContext {..} = liftIO $ do
    db <- readTVarIO database
    pps <- roTx db $ \tx -> DB.getPublicationPoints tx db
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
                        DB.getByUri tx db rpkiUrl >>= \case     
                            [] -> do                                
                                -- try TA certificates
                                tas <- DB.getTAs tx db                                 
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
                        (locatedDto <$>) . maybeToList <$> DB.getByHash tx db h

        (Nothing, Nothing, Just key') ->
            case readMaybe (convert key') of
                Nothing -> 
                    throwError err400 { errBody = convert $ "Could not parse integer key " <> key' } 
                Just (k :: Integer) -> do 
                    let kk = ObjectKey $ ArtificialKey $ fromIntegral k
                    roTxT database $ \tx db ->
                        (locatedDto <$>) . maybeToList <$> DB.getLocatedByKey tx db kk
        _ ->
            throwError $ err400 { errBody =
                "Only one of 'uri', 'hash' or 'key' must be provided." }
  where
    locatedDto located = RObject $ located & #payload %~ objectToDto

getOriginal :: (MonadIO m, Storage s, MonadError ServerError m)
                => AppContext s
                -> Maybe Text           
                -> m ObjectOriginal
getOriginal AppContext {..} hashText =
    case hashText of
        Nothing ->
            throwError $ err400 { errBody = "'hash' parameter must be provided." }
                        
        Just hashText' ->
            case parseHash hashText' of
                Left _  -> throwError err400
                Right hash -> do
                    z <- roTxT database $ \tx db -> DB.getOriginalBlobByHash tx db hash
                    case z of 
                        Nothing -> throwError err404
                        Just b  -> pure b

getManifests :: (MonadIO m, Storage s, MonadError ServerError m)
                => AppContext s
                -> Maybe Text           
                -> m ManifestsDto
getManifests AppContext {..} akiText = 
    case akiText of
        Nothing ->
            throwError $ err400 { errBody = "'aki' parameter must be provided." }
                        
        Just akiText' ->
            case parseAki akiText' of 
                Left _    -> throwError err400
                Right aki -> do
                    roTxT database $ \tx db -> do 
                        shortcutMft     <- fmap toMftShortcutDto <$> DB.getMftShorcut tx db aki                        
                        manifestObjects <- DB.findAllMftsByAKI tx db aki
                        let manifests = fmap (\(Keyed (Located _ m) _) -> manifestDto m) manifestObjects
                        pure ManifestsDto {..}
            

getSystem :: Storage s =>  AppContext s -> IO SystemDto
getSystem AppContext {..} = do
    now <- unNow <$> thisInstant
    SystemInfo {..} <- readTVarIO $ appState ^. #system
    let proverVersion = rpkiProverVersion    
    let gitInfo       = getGitInfo
    
    let z = MonoidalMap.toList $ unMetricMap $ metrics ^. #resources
    resources <- 
        forM z $ \(scope, resourceUsage) -> do  
            let AggregatedCPUTime aggregatedCpuTime = resourceUsage ^. #aggregatedCpuTime
            let LatestCPUTime latestCpuTime = resourceUsage ^. #latestCpuTime
            let aggregatedClockTime = resourceUsage ^. #aggregatedClockTime
            let maxMemory = resourceUsage ^. #maxMemory
            let avgCpuTimeMsPerSecond = cpuTimePerSecond aggregatedCpuTime (Earlier startUpTime) (Later now)
            tag <- fmtScope scope
            pure ResourcesDto {..}
    
    let wiToDto WorkerInfo {..} = let pid = fromIntegral workerPid in WorkerInfoDto {..}

    rsyncClients <- map (wiToDto . snd) . Map.toList <$> readTVarIO (appState ^. #runningRsyncClients)

    tals <- getTALs
    pure SystemDto {..}  
  where
    fmtScope scope =
        fmap (Text.intercalate "/") $
            roTxT database $ \tx db -> do         
                forM (scopeList scope) $ \s -> 
                    resolvedFocusToText <$> resolveLocations tx db s 

    getTALs = do
        db <- liftIO $ readTVarIO database
        liftIO $ roTx db $ \tx -> do        
            tas <- DB.getTAs tx db     
            pure [ TalDto {..} | (_, StorableTA {..}) <- tas, 
                    let repositories = map (toText . getRpkiURL) 
                            $ NonEmpty.toList 
                            $ unPublicationPointAccess initialRepositories ]                     


getRtr :: (MonadIO m, Storage s, MonadError ServerError m) =>
            AppContext s -> m RtrDto
getRtr AppContext {..} = do
    liftIO (readTVarIO $ appState ^. #rtrState) >>= \case
        Nothing -> throwError $ err400 { errBody =
                "RTR state doesn't exist, RTR server is waiting for a validation result or disabled." }
        Just rtrState -> pure RtrDto {..}

getVersions :: (MonadIO m, Storage s, MonadError ServerError m) =>
                AppContext s -> m [WorldVersion]
getVersions AppContext {..} = 
    liftIO $ map fst <$> roTxT database DB.versionsBackwards

getFetcheables :: (MonadIO m, Storage s, MonadError ServerError m) =>
                  AppContext s -> m Fetcheables
getFetcheables AppContext {..} = 
    liftIO $ readTVarIO $ appState ^. #fetcheables


getQueryPrefixValidity :: (MonadIO m, Storage s, MonadError ServerError m)
                        => AppContext s
                        -> Maybe String           
                        -> Maybe String
                        -> m ValidityResultDto
getQueryPrefixValidity appContext maybeAsn maybePrefix = do 
    case (maybeAsn, maybePrefix) of 
        (Just asn, Just prefix) -> getPrefixValidity appContext asn [prefix]
        (Nothing,   _         ) -> throwError $ err400 { errBody = "Parameter 'asn' is not set" }
        (_,         Nothing   ) -> throwError $ err400 { errBody = "Parameter 'prefix' is not set" }
        

getPrefixValidity :: (MonadIO m, Storage s, MonadError ServerError m)
                => AppContext s
                -> String           
                -> [String]         
                -> m ValidityResultDto
getPrefixValidity appContext asnText (List.intercalate "/" -> prefixText) = do 
    withPrefixIndex appContext $ \prefixIndex -> 
        case parsePrefix prefixText of 
            Nothing     -> throwError $ err400 { errBody = [i|Could not parse prefix #{prefixText}.|] }
            Just prefix -> 
                case parseAsn asnText of 
                    Nothing  -> throwError $ err400 { errBody = [i|Could not parse ASN #{asnText}.|] }
                    Just asn -> do 
                        Now now <- liftIO thisInstant
                        let validity = prefixValidity asn prefix prefixIndex
                        pure $! toValidityResultDto now asn prefix validity

getBulkPrefixValidity :: (MonadIO m, Storage s, MonadError ServerError m)
                        => AppContext s
                        -> [ValidityBulkInputDto]
                        -> m ValidityBulkResultDto
getBulkPrefixValidity appContext inputs =
    withPrefixIndex appContext $ \prefixIndex -> do
        Now now <- liftIO thisInstant
        !results <- forM inputs $ \(ValidityBulkInputDto {..}) -> 
            case parsePrefixT prefix of 
                Nothing -> throwError $ err400 { errBody = [i|Could not parse prefix #{prefix}.|] }
                Just p  -> 
                    case parseAsnT asn of 
                        Nothing -> throwError $ err400 { errBody = [i|Could not parse ASN #{asn}.|] }
                        Just a  -> pure $! T3 a p (prefixValidity a p prefixIndex)

        pure $! toBulkResultDto now results  

withPrefixIndex :: (MonadIO m, Storage s, MonadError ServerError m) => 
                    AppContext s 
                -> (PrefixIndex -> m a) 
                -> m a
withPrefixIndex AppContext {..} f = do
    liftIO (readTVarIO $ appState ^. #prefixIndex) >>= \case     
        Nothing          -> throwError $ err404 { errBody = [i|Prefix index is not (yet) built.|] }
        Just prefixIndex -> f prefixIndex                


toRepositoryDtos :: Storage s => AppContext s -> [(Repository, ValidationState)] -> IO [RepositoryDto] 
toRepositoryDtos AppContext {..} inputs = do
    let rrdps = [(r, s) | (RrdpR r, s) <- inputs]
    let rsyncs = [(r, s) | (RsyncR r, s) <- inputs]

    roTxT database $ \tx db -> do
        rrdpRepos <- 
            fmap (fmap RrdpDto . catMaybes)
            $ forM rrdps $ \(repository@RrdpRepository {..}, state) -> do
                let validationDtos = toVDtos $ filterRepositoryValidations (RrdpU uri) $ state ^. typed
                -- TODO That is probably not needed at all, there's nothing to resolve?
                resolved <- forM validationDtos $ resolveOriginalDto tx db                 

                pure $ fmap (\metrics -> RrdpRepositoryDto { validations = resolved, .. }) 
                        $ filterRepositoryMetrics (RrdpU uri) $ state ^. typed @Metrics . #rrdpMetrics

        rsyncRepos <- 
            fmap (fmap RsyncDto . catMaybes)
            $ forM rsyncs $ \(RsyncRepository { repoPP = RsyncPublicationPoint {..}, ..}, state) -> do
                let validationDtos = toVDtos $ filterRepositoryValidations (RsyncU uri) $ state ^. typed
                resolved <- forM validationDtos $ resolveOriginalDto tx db

                pure $ fmap (\metrics -> RsyncRepositoryDto { validations = resolved, .. }) 
                        $ filterRepositoryMetrics (RsyncU uri) $ state ^. typed @Metrics . #rsyncMetrics

        pure $ rrdpRepos <> rsyncRepos            
  where

    filterRepositoryValidations uri (Validations vs) = 
        Validations $ Map.filterWithKey (\scope _ -> relevantToRepository uri scope) vs

    filterRepositoryMetrics uri (MetricMap m) = 
        case [ metric | (scope, metric) <- MonoidalMap.toList m, relevantToRepository uri scope ] of 
            []        -> Nothing
            metric: _ -> Just metric

    relevantToRepository uri (Scope scope) = 
        uri `elem` [ u | RepositoryFocus u <- NonEmpty.toList scope ]


resolveOriginalDto :: (MonadIO m, Storage s) 
                    => Tx s 'RO
                    -> DB s 
                    -> OriginalVDto 
                    -> m ResolvedVDto
resolveOriginalDto tx db (OriginalVDto fd) = liftIO $ 
    ResolvedVDto <$> 
        #path (mapM (resolveLocations tx db)) fd
  
resolveValidationDto :: (MonadIO m, Storage s) => 
                        Tx s 'RO
                        -> DB s 
                        -> ValidationsDto OriginalVDto
                        -> m (ValidationsDto ResolvedVDto)
resolveValidationDto tx db vs = liftIO $ 
    #validations (mapM (resolveOriginalDto tx db)) vs
    

resolveLocations :: Storage s => 
                   Tx s 'RO
                -> DB s 
                -> Focus 
                -> IO ResolvedFocusDto
resolveLocations tx db = \case 
    TAFocus t               -> pure $ TextDto t
    TextFocus t             -> pure $ TextDto t
    PPFocus u               -> pure $ DirectLink $ toText u
    RepositoryFocus u       -> pure $ DirectLink $ toText u    
    LocationFocus (URI uri) -> pure $ ObjectLink uri
    LinkFocus (URI uri)     -> pure $ DirectLink uri
    ObjectFocus key         -> locations key
    HashFocus hash          -> DB.getKeyByHash tx db hash >>= \case 
                                    Nothing  -> pure $ TextDto [i|Can't find key for hash #{hash}|]
                                    Just key -> locations key        
  where
    locations key = do 
        DB.getLocationsByKey tx db key >>= \case 
            Nothing  -> pure $ TextDto [i|Can't find locations for key #{key}|]
            Just loc -> pure $ ObjectLink $ toText $ pickLocation loc

    
