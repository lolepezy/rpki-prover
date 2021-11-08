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
import           Servant.Server.Generic

import qualified Data.List.NonEmpty               as NonEmpty
import           Data.Maybe                       (fromMaybe, maybeToList)
import qualified Data.Set                         as Set
import qualified Data.Map.Strict                  as Map
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
import           RPKI.SLURM.Types
import           RPKI.Util


httpApi :: Storage s => AppContext s -> Application
httpApi appContext = genericServe HttpApi {
        api     = apiServer,
        metrics = convert <$> textualMetrics,        
        ui      = uiServer,
        staticContent = serveDirectoryEmbedded $(embedRecursiveDir "static")
    }
  where
    apiServer = genericServer API {
        vrpsCsv  = liftIO (getVRPValidated appContext),
        vrpsJson = liftIO (getVRPValidated appContext),

        vrpsCsvFiltered  = liftIO (getVRPSlurmed appContext),
        vrpsJsonFiltered = liftIO (getVRPSlurmed appContext),

        slurm = getSlurm appContext,
                
        validationResults = liftIO (getVResults appContext),
        appMetrics        = getMetrics appContext,                
        lmdbStats = getStats appContext,
        objectView = getRpkiObject appContext    
    }

    uiServer = do 
        worldVersion <- liftIO $ getLastVersion appContext
        vResults <- liftIO $ getVResults appContext
        metrics  <- getMetrics appContext
        pure $ mainPage worldVersion vResults metrics    


-- TODO CSV should contain TA name as well
getVRPValidated :: Storage s => AppContext s -> IO [VrpDto]
getVRPValidated appContext = getVRPs appContext (readTVar . (^. #currentVrps))

getVRPSlurmed :: Storage s => AppContext s -> IO [VrpDto]
getVRPSlurmed appContext = getVRPs appContext (readTVar . (^. #filteredVrps))         


getVRPs :: Storage s => AppContext s -> (AppState -> STM Vrps) -> IO [VrpDto] 
getVRPs AppContext {..} func = do 
    z <- do 
        vrps <- atomically (func appState)
        if vrps == mempty
            then                  
                getLatestVRPs =<< readTVarIO database                
            else 
                pure $ Just vrps
    pure $ case z of 
        Nothing   -> []
        Just vrps -> [ VrpDto a p len (unTaName ta) | 
                        (ta, vrpSet)  <- Map.toList $ unVrps vrps,
                        (Vrp a p len) <- Set.toList vrpSet
                    ]    

getVResults :: Storage s => AppContext s -> IO [ValidationResult]
getVResults AppContext {..} = do 
    db@DB {..} <- readTVarIO database 
    roTx versionStore $ \tx -> 
        fmap (fromMaybe []) $ runMaybeT $ do
                lastVersion <- MaybeT $ getLastCompletedVersion db tx
                validations <- MaybeT $ validationsForVersion tx validationsStore lastVersion
                pure $ map toVR $ validationsToList validations        

getLastVersion :: Storage s => AppContext s -> IO (Maybe WorldVersion)
getLastVersion AppContext {..} = do 
    db <- readTVarIO database 
    roTx db $ getLastCompletedVersion db                
        
getMetrics :: (MonadIO m, Storage s, MonadError ServerError m) => 
            AppContext s -> m AppMetric
getMetrics AppContext {..} = do
    db@DB {..} <- liftIO $ readTVarIO database 
    metrics <- liftIO $ roTx db $ \tx -> do
        runMaybeT $ do
                lastVersion <- MaybeT $ getLastCompletedVersion db tx
                MaybeT $ metricsForVersion tx metricStore lastVersion                        
    case metrics of 
        Nothing -> throwError err404 { errBody = "Working, please hold still!" }
        Just m  -> pure m

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

