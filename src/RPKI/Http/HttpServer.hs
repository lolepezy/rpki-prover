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
import           Servant
import           Servant.Swagger.UI

import qualified Data.ByteString.Builder          as BS

import           Data.List                        (sortBy)
import qualified Data.List.NonEmpty               as NonEmpty
import           Data.Maybe                       (maybeToList, fromMaybe)
import qualified Data.Set                         as Set
import qualified Data.List                         as List
import qualified Data.Map.Monoidal.Strict         as MonoidalMap
import           Data.Text                       (Text)
import           Data.String.Interpolate.IsString

import           RPKI.AppContext
import           RPKI.AppTypes
import           RPKI.AppState
import           RPKI.Domain
import           RPKI.Config
import           RPKI.Messages
import           RPKI.Metrics.Prometheus
import           RPKI.Time
import           RPKI.Reporting
import           RPKI.Http.Api
import           RPKI.Http.Types
import           RPKI.Http.UI
import           RPKI.Store.Base.Storage hiding (get)
import           RPKI.Store.Database
import           RPKI.Resources.Types
import           RPKI.RTR.Types
import           RPKI.Store.Types
import           RPKI.SLURM.Types
import           RPKI.Util
import Data.Ord
import RPKI.SLURM.SlurmProcessing (applySlurmBgpSec)

httpApi :: Storage s => AppContext s -> Application
httpApi appContext = genericServe HttpApi {
        api     = apiServer,
        metrics = convert <$> textualMetrics,
        ui      = uiServer,
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

        aspas = liftIO $ getASPAs appContext,
        bgpCerts = liftIO $ getBGPCerts appContext,
        bgpCertsFiltered = liftIO $ getBGPCertsFiltered appContext,

        slurm = getSlurm appContext,
        slurms = getAllSlurms appContext,

        fullValidationResults    = getValidationsDto appContext,
        validationResultsMinimal = toMinimalValidations <$> getValidationsDto appContext,
        metrics = snd <$> getMetrics appContext,
        publicationsPoints = getPPs appContext,
        lmdbStats = getStats appContext,
        jobs = getJobs appContext,
        objectView = getRpkiObject appContext,
        system = liftIO $ getSystem appContext,
        rtr = getRtr appContext,
        versions = getVersions appContext
    }

    uiServer = do
        worldVersion <- liftIO $ getLastVersion appContext
        vResults     <- liftIO $ getValidations appContext
        metrics <- getMetrics appContext
        pure $ mainPage worldVersion vResults metrics

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

toVrpDtos :: Maybe Vrps -> [VrpDto]
toVrpDtos = \case
    Nothing   -> []
    Just vrps -> [ VrpDto a p len (unTaName ta) |
                        (ta, vrpSet) <- MonoidalMap.toList $ unVrps vrps,
                        Vrp a p len  <- Set.toList vrpSet ]

toVrpSet :: Maybe Vrps -> Set.Set AscOrderedVrp
toVrpSet = maybe mempty uniqVrps

toVrpMinimalDtos :: Maybe Vrps -> [VrpMinimalDto]
toVrpMinimalDtos = map asDto . Set.toList . toVrpSet
  where
    asDto (AscOrderedVrp (Vrp asn prefix maxLength)) = VrpMinimalDto {..}


getASPAs :: Storage s => AppContext s -> IO [AspaDto]
getASPAs AppContext {..} = do
    aspas <- getLatestAspas =<< readTVarIO database
    pure $ map toDto $ Set.toList aspas
  where
    toDto aspa =
        AspaDto {
            customerAsn = aspa ^. #customerAsn,
            providerAsns = map (\(asn, afiLimit) -> ProviderAsn {..}) $ aspa ^. #providerAsns
        }

getBGPCerts :: Storage s => AppContext s -> IO [BgpCertDto]
getBGPCerts AppContext {..} =
    fmap (map bgpSecToDto . Set.toList) 
        $ getLatestBgps =<< readTVarIO database    

getBGPCertsFiltered :: Storage s => AppContext s -> IO [BgpCertDto]
getBGPCertsFiltered AppContext {..} = do
    db <- readTVarIO database
    fmap (fromMaybe mempty) $ roTx db $ \tx -> do 
        getLastCompletedVersion db tx >>= \case      
            Nothing      -> pure mempty   
            Just version -> runMaybeT $ do 
                bgps  <- MaybeT $ getBgps tx db version
                slurm <- MaybeT $ slurmForVersion tx db version                        
                pure $ map bgpSecToDto $ Set.toList $ applySlurmBgpSec slurm bgps    
  

bgpSecToDto :: BGPSecPayload -> BgpCertDto
bgpSecToDto BGPSecPayload {..} = BgpCertDto {
        ski = bgpSecSki,
        asns = bgpSecAsns,
        subjectPublicKeyInfo = bgpSecSpki
    }        

getValidations :: Storage s => AppContext s -> IO (Maybe (ValidationsDto FullVDto))
getValidations AppContext {..} = do
    db@DB {..} <- readTVarIO database
    roTx versionStore $ \tx ->
        runMaybeT $ do
            lastVersion <- MaybeT $ getLastCompletedVersion db tx
            validations <- MaybeT $ validationsForVersion tx validationsStore lastVersion
            let validationDtos = map toVR $ validationsToList validations
            pure $ ValidationsDto {
                    worldVersion = lastVersion,
                    timestamp    = versionToMoment lastVersion,
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

getMetrics :: (MonadIO m, Storage s, MonadError ServerError m) =>
            AppContext s -> m (RawMetric, MetricsDto)
getMetrics AppContext {..} = do
    db <- liftIO $ readTVarIO database
    metrics <- liftIO $ roTx db $ \tx ->
        runMaybeT $ do
            lastVersion <- MaybeT $ getLastCompletedVersion db tx
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
        versions <- List.sortOn Down <$> allVersions tx db
        slurms   <- mapM (\(wv, _) -> (wv, ) <$> slurmForVersion tx db wv) versions        
        pure [ (w, s) | (w, Just s) <- slurms ]


toVR :: (Scope a, Set.Set VIssue) -> FullVDto
toVR (Scope scope, issues) = FullVDto {
        issues = map toDto $ Set.toList issues,
        path   = NonEmpty.toList scope,
        url    = NonEmpty.head scope
    }
  where
    toDto = \case
        VErr e               -> ErrorDto $ toMessage e
        (VWarn (VWarning w)) -> WarningDto $ toMessage w


getStats :: (MonadIO m, Storage s) => AppContext s -> m TotalDBStats
getStats AppContext {..} = liftIO $ getTotalDbStats =<< readTVarIO database

getJobs :: (MonadIO m, Storage s) => AppContext s -> m JobsDto
getJobs AppContext {..} = liftIO $ do
    db <- readTVarIO database
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
                    DB {..} <- readTVarIO database
                    roTx objectStore $ \tx ->
                        (RObject <$>) <$> getByUri tx objectStore rpkiUrl

        (Nothing, Just hash') ->
            case parseHash hash' of
                Left _  -> throwError err400
                Right h -> liftIO $ do
                    db <- readTVarIO database
                    roTx db $ \tx -> 
                        (RObject <$>) . maybeToList <$> getByHash tx db h

        (Just _, Just _) ->
            throwError $ err400 { errBody =
                "Only 'uri' or 'hash' must be provided, not both." }

getSystem :: Storage s =>  AppContext s -> IO SystemDto
getSystem AppContext {..} = do
    now <- unNow <$> thisInstant
    si <- readTVarIO $ appState ^. #system
    let proverVersion = getVersion

    let toResources (scope, resourceUsage) = let
                tag = scopeText scope
                aggregatedCpuTime = resourceUsage ^. #aggregatedCpuTime
                maxMemory = resourceUsage ^. #maxMemory
                avgCpuTimeMsPerSecond = cpuTimePerSecond aggregatedCpuTime (si ^. #startTime) now
            in ResourcesDto {..}

    let resources = map toResources $ MonoidalMap.toList $ unMetricMap $ si ^. #metrics . #resources
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


vrpDtosToCSV :: [VrpDto] -> RawCSV
vrpDtosToCSV vrpDtos =
    rawCSV 
        (str "ASN,IP Prefix,Max Length,Trust Anchor\n")
        (mconcat $ map toBS vrpDtos)
  where
    toBS VrpDto {
            asn = ASN as,
            maxLength = PrefixLength ml,
            ..
        } = str "AS" <> str (show as) <> ch ',' <>
            str (prefixStr prefix) <> ch ',' <>
            str (show ml) <> ch ',' <>
            str (convert ta) <> ch '\n'

vrpSetToCSV :: Set.Set AscOrderedVrp -> RawCSV
vrpSetToCSV vrpDtos =
    rawCSV 
        (str "ASN,IP Prefix,Max Length\n")
        (mconcat $ map toBS $ Set.toList vrpDtos)
  where
    toBS (AscOrderedVrp (Vrp (ASN asn) prefix (PrefixLength maxLength))) = 
        str "AS" <> str (show asn) <> ch ',' <>
        str (prefixStr prefix) <> ch ',' <>
        str (show maxLength) <> ch '\n'


rawCSV :: BS.Builder -> BS.Builder -> RawCSV
rawCSV header body = RawCSV $ BS.toLazyByteString $ header <> body
    

prefixStr :: IpPrefix -> String
prefixStr (Ipv4P (Ipv4Prefix p)) = show p
prefixStr (Ipv6P (Ipv6Prefix p)) = show p

str :: String -> BS.Builder
str = BS.stringUtf8

ch :: Char -> BS.Builder
ch  = BS.charUtf8    
