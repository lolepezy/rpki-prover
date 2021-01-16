{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StrictData #-}

module RPKI.Metrics where

import           Control.Lens                     ((^.), (%~), (&))
import           Data.Generics.Product.Typed

import qualified Data.ByteString.Lazy             as LBS

import           Data.Hourglass
import           Data.Int                         (Int64)
import qualified Data.List.NonEmpty               as NonEmpty
import qualified Data.Set                         as Set
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map

import           Data.Text                        (Text)

import           Data.String.Interpolate.IsString

import Prometheus

import           RPKI.AppState
import           RPKI.CommonTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Store.Database
import           RPKI.TopDown

import           RPKI.AppContext
import           RPKI.RTR.RtrServer
import           RPKI.Store.Base.Storage
import           RPKI.TAL
import           RPKI.Time

import           RPKI.Store.Base.LMDB
import           RPKI.Store.AppStorage
import GHC.Generics
import Control.Monad.IO.Class
import Control.Monad
import Control.Lens.Combinators


data PrometheusMetrics = PrometheusMetrics {
        rrdpCode :: Vector Text Gauge,
        downloadTime :: Vector Text Gauge,
        vrpNumber :: Vector Text Gauge,
        validObjectNumber :: Vector (Text, Text) Gauge
    }
    deriving (Generic)

    
createPrometheusMetrics :: MonadIO m => m PrometheusMetrics
createPrometheusMetrics = do
    rrdpCode <- register 
            $ vector ("url" :: Text) 
            $ gauge (Info "rrdp_http_code" "HTTP code of the RRDP response")
    downloadTime <- register 
            $ vector ("url" :: Text) 
            $ gauge (Info "download_time" "Time of downloading repository (ms)")
    vrpNumber <- register 
            $ vector ("trustanchor" :: Text) 
            $ gauge (Info "vrp_number" "Number of unique VRPs")
    validObjectNumber <- register 
            $ vector ("trustanchor", "type") 
            $ gauge (Info "object_number" "Number of valid objects of different types")

    pure $ PrometheusMetrics {..}


textualMetrics :: MonadIO m => m LBS.ByteString
textualMetrics = exportMetricsAsText

updateMetrics :: (MonadIO m, MonadMonitor m) => AppMetric -> PrometheusMetrics -> m ()
updateMetrics AppMetric {..} PrometheusMetrics {..} = do 
    forM_ (Map.toList $ unMonoidMap$ unMetricMap rsyncMetrics) $ \(metricPath, metric) -> do 
        let url = NonEmpty.last $ metricPath ^. coerced                
        withLabel downloadTime url $ flip setGauge $ fromIntegral $ unTimeMs $ metric ^. #totalTimeMs

    forM_ (Map.toList $ unMonoidMap$ unMetricMap rrdpMetrics) $ \(metricPath, metric) -> do 
        let url = NonEmpty.last $ metricPath ^. coerced        
        withLabel rrdpCode url $ flip setGauge $ fromIntegral $ unHttpStatus $ metric ^. #lastHttpStatus
        withLabel downloadTime url $ flip setGauge $ fromIntegral $ unTimeMs $ metric ^. #downloadTimeMs
    forM_ (Map.toList $ unMonoidMap$ unMetricMap validationMetrics) $ \(metricPath, metric) -> do 
        let url = NonEmpty.last $ metricPath ^. coerced        
        withLabel vrpNumber url $ flip setGauge $ fromIntegral $ unCount $ metric ^. #vrpNumber
        let totalCount = metric ^. #validCertNumber + 
                         metric ^. #validRoaNumber +
                         metric ^. #validMftNumber +
                         metric ^. #validCrlNumber +
                         metric ^. #validGbrNumber
        withLabel validObjectNumber (url, "cer") $ flip setGauge $ fromIntegral $ unCount $ metric ^. #validCertNumber
        withLabel validObjectNumber (url, "roa") $ flip setGauge $ fromIntegral $ unCount $ metric ^. #validRoaNumber
        withLabel validObjectNumber (url, "mft") $ flip setGauge $ fromIntegral $ unCount $ metric ^. #validMftNumber
        withLabel validObjectNumber (url, "crl") $ flip setGauge $ fromIntegral $ unCount $ metric ^. #validCrlNumber
        withLabel validObjectNumber (url, "grb") $ flip setGauge $ fromIntegral $ unCount $ metric ^. #validGbrNumber
        withLabel validObjectNumber (url, "total") $ flip setGauge $ fromIntegral $ unCount totalCount
    