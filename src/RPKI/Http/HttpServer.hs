{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell #-}

module RPKI.Http.HttpServer where

import           Control.Lens                     ((^.))

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Error.Class
import           FileEmbedLzma
import           Servant hiding (URI)

import qualified Data.List.NonEmpty               as NonEmpty
import           Data.Maybe                       (fromMaybe, maybeToList)
import qualified Data.Set                         as Set
import           Data.Text                       (Text)

import           RPKI.AppContext
import           RPKI.AppTypes
import           RPKI.AppState
import           RPKI.Domain
import           RPKI.Metrics
import           RPKI.Reporting
import           RPKI.Http.Api
import           RPKI.Http.Types
import           RPKI.Http.UI
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import           RPKI.Store.Types
import           RPKI.Util

staticServer :: Server StaticContent
staticServer = serveDirectoryEmbedded $(embedRecursiveDir "static")

type FullAPI = API :<|> UI :<|> PrometheusAPI :<|> StaticContent

prometheus :: Server PrometheusAPI
prometheus = liftIO $ convert <$> textualMetrics

httpApi :: Storage s => AppContext s -> Application
httpApi appContext = serve 
                        (Proxy @FullAPI)
                        (apiServer appContext                                                  
                        :<|> uiServer appContext
                        :<|> prometheus
                        :<|> staticServer)

uiServer :: Storage s => AppContext s -> Server UI
-- uiServer appContext = withUI . validaionResultsHtml <$> liftIO (getVResults appContext)
uiServer appContext = do 
    worldVersion <- liftIO $ getLastVersion appContext
    vResults <- liftIO $ getVResults appContext
    metrics  <- getMetrics appContext
    pure $ mainPage worldVersion vResults metrics

apiServer :: Storage s => AppContext s -> Server API
apiServer appContext = 
    liftIO (getVRPs appContext)
    :<|> liftIO (getVRPs appContext)
    :<|> liftIO (getVResults appContext)
    :<|> getMetrics appContext
    :<|> getStats  appContext
    :<|> getRpkiObject appContext

-- TODO CSV should contain TA name as well
getVRPs :: Storage s => AppContext s -> IO [VrpDto]
getVRPs AppContext {..} = do 
    vrps <- do 
        vrps <- atomically $ allCurrentVrps appState
        if Set.null vrps
            then do 
                db@DB {..} <- readTVarIO database 
                roTx versionStore $ \tx ->
                    fmap (fromMaybe []) $ 
                        runMaybeT $ do 
                            version <- MaybeT $ getLastCompletedVersion db tx
                            vs      <- MaybeT $ getVrps tx db version
                            pure $ Set.toList $ allVrps vs                                        

            else pure $! Set.toList vrps

    pure $! map (\(Vrp a p len) -> VrpDto a p len) vrps                    

getVResults :: Storage s => AppContext s -> IO [ValidationResult]
getVResults AppContext {..} = do 
    db@DB {..} <- readTVarIO database 
    roTx versionStore $ \tx -> 
        let txValidations = runMaybeT $ do
                lastVersion <- MaybeT $ getLastCompletedVersion db tx
                validations <- MaybeT $ validationsForVersion tx validationsStore lastVersion
                pure $ map toVR $ validationsToList validations
        in fromMaybe [] <$> txValidations

getLastVersion :: Storage s => AppContext s -> IO (Maybe WorldVersion)
getLastVersion AppContext {..} = do 
    db@DB {..} <- readTVarIO database 
    roTx versionStore $ getLastCompletedVersion db                
        
getMetrics :: (MonadIO m, Storage s, MonadError ServerError m) => 
            AppContext s -> m AppMetric
getMetrics AppContext {..} = do
    db@DB {..} <- liftIO $ readTVarIO database 
    metrics <- liftIO $ roTx versionStore $ \tx -> do
        runMaybeT $ do
                lastVersion <- MaybeT $ getLastCompletedVersion db tx
                MaybeT $ metricsForVersion tx metricStore lastVersion                        
    case metrics of 
        Nothing -> throwError err404 { errBody = "Working, please hold still!" }
        Just m  -> pure m
    
toVR :: (Path a, Set.Set VProblem) -> ValidationResult
toVR (Path path, problems) = 
    ValidationResult (Set.toList problems) (NonEmpty.toList path)    

getStats :: (MonadIO m, Storage s) => AppContext s -> m TotalDBStats
getStats AppContext {..} = liftIO $ getTotalDbStats =<< readTVarIO database             

getRpkiObject :: (MonadIO m, Storage s, MonadError ServerError m) 
                => AppContext s 
                -> Maybe Text 
                -> Maybe Text 
                -> m [RObject]
getRpkiObject AppContext {..} uri hash =
    case (uri, hash) of 
        (Nothing,  Nothing) -> 
            throwError $ err400 { errBody = "'uri' or 'hash' must be provided." }

        (Just u, Nothing) -> do 
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
                    DB {..} <- readTVarIO database 
                    roTx objectStore $ \tx -> 
                        (RObject <$>) . maybeToList <$> getByHash tx objectStore h                                      

        (Just _, Just _) -> 
            throwError $ err400 { errBody = 
                "Only 'uri' or 'hash' must be provided, not both." }

