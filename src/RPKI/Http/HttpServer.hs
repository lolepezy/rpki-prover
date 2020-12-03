{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE TemplateHaskell #-}

module RPKI.Http.HttpServer where

import           Control.Lens                     ((^.))

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           FileEmbedLzma
import           Servant

import qualified Data.List.NonEmpty               as NonEmpty
import           Data.Maybe                       (fromMaybe)
import qualified Data.Set                         as Set

import           Data.String.Interpolate.IsString

import           RPKI.AppContext
import           RPKI.Domain
import           RPKI.Logging
import           RPKI.Time
import           RPKI.Reporting
import           RPKI.Http.Api
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database



validatorServer :: forall s . Storage s => AppContext s -> Server API
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
                        roTx versionStore $ \tx ->
                            getLastCompletedVersion database tx >>= \case 
                                Nothing          -> pure []            
                                Just lastVersion -> do                        
                                    (!vrps, elapsed) <- timedMS $ getVrps tx database lastVersion
                                    logDebugM logger [i|Read VRPs, took #{elapsed}ms.|]        
                                    pure vrps
                    else pure $! Set.toList vrps                    

            pure $! map (\(Vrp a p len) -> VrpDto a p len) vrps                    

        getVResults = 
            roTx versionStore $ \tx -> 
                let txValidations = runMaybeT $ do
                        lastVersion <- MaybeT $ getLastCompletedVersion database tx
                        validations <- MaybeT $ validationsForVersion tx validationsStore lastVersion
                        pure $ map toVR $ validationsToList validations
                in fromMaybe [] <$> txValidations
                
        getMetrics = do
            metrics <- liftIO $ roTx versionStore $ \tx -> do
                runMaybeT $ do
                        lastVersion <- MaybeT $ getLastCompletedVersion database tx
                        MaybeT $ metricsForVersion tx metricStore lastVersion                        
            case metrics of 
                Nothing -> throwError err404
                Just m  -> pure m
            
        toVR (Path path, problems) = 
            ValidationResult (Set.toList problems) (NonEmpty.toList path)    

        getStats = dbStats database

        getRpkiObject hash = 
            liftIO $ roTx objectStore $ \tx -> 
                (RObject <$>) <$> getByHash tx objectStore hash            

        DB {..} = database


embeddedUI :: Server Raw
embeddedUI = serveDirectoryEmbedded $(embedDir "ui")

httpApi :: Storage s => AppContext s -> Application
httpApi appContext = serve 
                        (Proxy @(API :<|> Raw))
                        (validatorServer appContext :<|> embeddedUI)
