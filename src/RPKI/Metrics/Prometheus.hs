{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StrictData           #-}

module RPKI.Metrics.Prometheus where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad

import qualified Data.ByteString.Lazy             as LBS
import           Data.Text                        (Text)
import qualified Data.List.NonEmpty               as NonEmpty
import qualified Data.Map.Monoidal.Strict as MonoidalMap

import           GHC.Generics

import           Prometheus
import           Prometheus.Metric.GHC
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Metrics.Metrics


data PrometheusMetrics = PrometheusMetrics {
        rrdpCode        :: Vector Text Gauge,
        downloadTime    :: Vector Text Gauge,
        vrpCounter      :: Vector Text Gauge,
        uniqueVrpNumber :: Vector Text Gauge,
        validObjectNumberPerTa   :: Vector (Text, Text) Gauge,
        validObjectNumberPerRepo :: Vector (Text, Text) Gauge
    }
    deriving (Generic)


createPrometheusMetrics :: MonadIO m => m PrometheusMetrics
createPrometheusMetrics = do

    void $ register ghcMetrics

    rrdpCode <- register
            $ vector ("url" :: Text)
            $ gauge (Info "rrdp_http_code" "HTTP code of the RRDP response")
    downloadTime <- register
            $ vector ("url" :: Text)
            $ gauge (Info "download_time" "Time of downloading repository (ms)")
    vrpCounter <- register
            $ vector ("trustanchor" :: Text)
            $ gauge (Info "vrp_number" "Number of original VRPs")
    uniqueVrpNumber <- register
            $ vector ("trustanchor" :: Text)
            $ gauge (Info "unique_vrp_number" "Number of unique VRPs")
    validObjectNumberPerTa <- register
            $ vector ("trustanchor", "type")
            $ gauge (Info "object_number" "Number of valid objects of different types per TA")
    validObjectNumberPerRepo <- register
            $ vector ("repository", "type")
            $ gauge (Info "object_number" "Number of valid objects of different types per repository")

    pure $ PrometheusMetrics {..}


textualMetrics :: MonadIO m => m LBS.ByteString
textualMetrics = exportMetricsAsText

updatePrometheus :: (MonadIO m, MonadMonitor m) => RawMetric -> PrometheusMetrics -> m ()
updatePrometheus rm@RawMetric {..} PrometheusMetrics {..} = do
    forM_ (MonoidalMap.toList $ unMetricMap rsyncMetrics) $ \(metricScope, metric) -> do
        let url = focusToText $ NonEmpty.head $ metricScope ^. coerced
        withLabel downloadTime url $ flip setGauge $ fromIntegral $ unTimeMs $ metric ^. #totalTimeMs

    forM_ (MonoidalMap.toList $ unMetricMap rrdpMetrics) $ \(metricScope, metric) -> do
        let url = focusToText $ NonEmpty.head $ metricScope ^. coerced
        withLabel rrdpCode url $ flip setGauge $ fromIntegral $ unHttpStatus $ metric ^. #lastHttpStatus
        withLabel downloadTime url $ flip setGauge $ fromIntegral $ unTimeMs $ metric ^. #downloadTimeMs

    let grouped = groupedValidationMetric rm
    
    forM_ (MonoidalMap.toList $ grouped ^. #byTa) $ \(TaName name, metric) ->
        setObjectMetricsPerUrl validObjectNumberPerTa name metric True
    forM_ (MonoidalMap.toList $ grouped ^. #byRepository) $ \(rpkiUrl, metric) -> 
        setObjectMetricsPerUrl validObjectNumberPerRepo (unURI $ getURL rpkiUrl) metric False

  where
      
    setValidObjects prometheusVector url tag count = withLabel prometheusVector (url, tag)
            $ flip setGauge
            $ fromIntegral $ unCount count

    setObjectMetricsPerUrl prometheusVector url metric setUniqueVRPs = do
        withLabel vrpCounter url $ flip setGauge $ fromIntegral $ unCount $ metric ^. #vrpCounter

        when setUniqueVRPs $ withLabel uniqueVrpNumber url $ 
            flip setGauge $ fromIntegral $ unCount $ metric ^. #uniqueVrpNumber
            
        let totalCount = metric ^. #validCertNumber +
                         metric ^. #validRoaNumber +
                         metric ^. #validMftNumber +
                         metric ^. #validCrlNumber +
                         metric ^. #validGbrNumber
        setValidObjects prometheusVector url "cer" $ metric ^. #validCertNumber
        setValidObjects prometheusVector url "roa" $ metric ^. #validRoaNumber
        setValidObjects prometheusVector url "mft" $ metric ^. #validMftNumber
        setValidObjects prometheusVector url "crl" $ metric ^. #validCrlNumber
        setValidObjects prometheusVector url "grb" $ metric ^. #validGbrNumber
        setValidObjects prometheusVector url "allobjects" totalCount
