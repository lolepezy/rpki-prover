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

module RPKI.Metrics where

import           Codec.Serialise

import           Control.Lens                     ((^.), (%~), (&))
import           Control.Lens.Combinators
import           Control.Monad.IO.Class
import           Control.Monad

import           Data.Generics.Product.Typed

import qualified Data.ByteString.Lazy             as LBS

import qualified Data.List.NonEmpty               as NonEmpty
import qualified Data.Set                         as Set
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map

import qualified Data.Map.Monoidal.Strict as MonoidalMap

import           Data.String.Interpolate.IsString
import           Data.Text                        (Text)

import           GHC.Generics

import           Prometheus
import           Prometheus.Metric.GHC

import           Data.Map.Monoidal.Strict
import           RPKI.Domain
import           RPKI.Reporting


allTAsMetricsName :: Text
allTAsMetricsName = "alltrustanchors"

data PrometheusMetrics = PrometheusMetrics {
        rrdpCode :: Vector Text Gauge,
        downloadTime :: Vector Text Gauge,
        vrpNumber :: Vector Text Gauge,
        validObjectNumberPerTa   :: Vector (Text, Text) Gauge,
        validObjectNumberPerRepo :: Vector (Text, Text) Gauge
    }
    deriving (Generic)


createPrometheusMetrics :: MonadIO m => m PrometheusMetrics
createPrometheusMetrics = do

    register ghcMetrics

    rrdpCode <- register
            $ vector ("url" :: Text)
            $ gauge (Info "rrdp_http_code" "HTTP code of the RRDP response")
    downloadTime <- register
            $ vector ("url" :: Text)
            $ gauge (Info "download_time" "Time of downloading repository (ms)")
    vrpNumber <- register
            $ vector ("trustanchor" :: Text)
            $ gauge (Info "vrp_number" "Number of unique VRPs")
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
updatePrometheus RawMetric {..} PrometheusMetrics {..} = do
    forM_ (Map.toList $ getMonoidalMap $ unMetricMap rsyncMetrics) $ \(metricPath, metric) -> do
        let url = segmentToText $ NonEmpty.head $ metricPath ^. coerced
        withLabel downloadTime url $ flip setGauge $ fromIntegral $ unTimeMs $ metric ^. #totalTimeMs

    forM_ (Map.toList $ getMonoidalMap $ unMetricMap rrdpMetrics) $ \(metricPath, metric) -> do
        let url = segmentToText $ NonEmpty.head $ metricPath ^. coerced
        withLabel rrdpCode url $ flip setGauge $ fromIntegral $ unHttpStatus $ metric ^. #lastHttpStatus
        withLabel downloadTime url $ flip setGauge $ fromIntegral $ unTimeMs $ metric ^. #downloadTimeMs

    let normalised = normalisedValidationMetric validationMetrics
    
    forM_ (MonoidalMap.toList $ normalised ^. #perTa) $ \(TaName name, metric) ->
        setObjectMetricsPerUrl validObjectNumberPerTa name metric
    forM_ (MonoidalMap.toList $ normalised ^. #perRepository) $ \(rpkiUrl, metric) -> 
        setObjectMetricsPerUrl validObjectNumberPerRepo (unURI $ getURL rpkiUrl) metric    

  where
      
    setValidObjects prometheusVector url tag count = withLabel prometheusVector (url, tag)
            $ flip setGauge
            $ fromIntegral $ unCount count

    setObjectMetricsPerUrl prometheusVector url metric = do
        withLabel vrpNumber url $ flip setGauge $ fromIntegral $ unCount $ metric ^. #vrpNumber
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


data NormalisedValidationMetric a = NormalisedValidationMetric {
        perTa         :: MonoidalMap TaName a,
        perRepository :: MonoidalMap RpkiURL a
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise


normalisedValidationMetric :: MetricMap ValidationMetric -> NormalisedValidationMetric ValidationMetric
normalisedValidationMetric validationMetrics = NormalisedValidationMetric {..}
  where
    (perTa, perRepository) = 
        MonoidalMap.foldrWithKey combineMetrics mempty $ unMetricMap validationMetrics

    combineMetrics metricPath metric (perTa, perRepo) = (newPerTa, newPerRepo)
      where
        newPerTa =
            case take 1 $ reverse [ TaName uri | TASegment uri <- pathList metricPath ] of
                []      -> perTa
                ta' : _ -> MonoidalMap.singleton ta' metric <> perTa

        newPerRepo =
            -- take the deepest PP
            case take 1 [ pp | PPSegment pp <- pathList metricPath ] of
                []      -> perRepo
                uri : _ -> MonoidalMap.singleton uri metric <> perRepo        