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
import           FileEmbedLzma
import           Servant hiding (URI)

import qualified Data.List.NonEmpty               as NonEmpty
import           Data.Maybe                       (fromMaybe)
import qualified Data.Set                         as Set

import           Data.String.Interpolate.IsString

import           RPKI.AppContext
import           RPKI.Domain
import           RPKI.Logging
import           RPKI.Metrics
import           RPKI.Time
import           RPKI.Reporting
import           RPKI.Http.Api
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import           RPKI.Util



validatorServer :: Storage s => AppContext s -> Server API
validatorServer AppContext {..} = 
    liftIO getVRPs
    :<|> liftIO getVRPs
    :<|> liftIO getVResults
    :<|> getMetrics
    :<|> getStats
    :<|> getRpkiObject
    where
        getVRPs = do 
            vrps <- do 
                vrps <- readTVarIO $ appState ^. #currentVrps
                if Set.null vrps
                    then do 
                        database@DB {..} <- readTVarIO database 
                        roTx versionStore $ \tx ->
                            getLastCompletedVersion database tx >>= 
                                maybe (pure []) (getVrps tx database)
                                
                    else pure $! Set.toList vrps                    

            pure $! map (\(Vrp a p len) -> VrpDto a p len) vrps                    

        getVResults = do 
            database@DB {..} <- readTVarIO database 
            roTx versionStore $ \tx -> 
                let txValidations = runMaybeT $ do
                        lastVersion <- MaybeT $ getLastCompletedVersion database tx
                        validations <- MaybeT $ validationsForVersion tx validationsStore lastVersion
                        pure $ map toVR $ validationsToList validations
                in fromMaybe [] <$> txValidations
                
        getMetrics = do
            database@DB {..} <- liftIO $ readTVarIO database 
            metrics <- liftIO $ roTx versionStore $ \tx -> do
                runMaybeT $ do
                        lastVersion <- MaybeT $ getLastCompletedVersion database tx
                        MaybeT $ metricsForVersion tx metricStore lastVersion                        
            case metrics of 
                Nothing -> throwError err404
                Just m  -> pure m
            
        toVR (Path path, problems) = 
            ValidationResult (Set.toList problems) (NonEmpty.toList path)    

        getStats = liftIO $ dbStats =<< readTVarIO database             

        getRpkiObject uri hash =
            case (uri, hash) of 
                (Nothing,  Nothing) -> 
                    throwError $ err400 { errBody = "'uri' or 'hash' must be provided." }

                (Just uri', Nothing) -> 
                    liftIO $ do 
                        DB {..} <- readTVarIO database 
                        roTx objectStore $ \tx -> 
                            (RObject <$>) <$> getByUri tx objectStore (URI uri')

                (Nothing, Just hash') -> 
                    case parseHash hash' of 
                        Left e  -> throwError err400 
                        Right h -> liftIO $ do 
                            DB {..} <- readTVarIO database 
                            roTx objectStore $ \tx -> 
                                (RObject <$>) <$> getByHash tx objectStore h                                      

                (Just _, Just _) -> 
                    throwError $ err400 { errBody = "Only 'uri' or 'hash' must be provided, not both." }


embeddedUI :: Server Raw
embeddedUI = serveDirectoryEmbedded $(embedDir "ui")

type FullAPI = API :<|> PrometheusAPI :<|> Raw

prometheus :: Server PrometheusAPI
prometheus = liftIO $ convert <$> textualMetrics

httpApi :: Storage s => AppContext s -> Application
httpApi appContext = serve 
                        (Proxy @FullAPI)
                        (validatorServer appContext                          
                        :<|> prometheus
                        :<|> embeddedUI)
