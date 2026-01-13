{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module RPKI.Metrics.Prometheus where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad

import qualified Data.ByteString.Lazy     as LBS
import           Data.Text                (Text)
import qualified Data.Map.Monoidal.Strict as MonoidalMap

import           GHC.Generics

import           Prometheus
import           Prometheus.Metric.GHC
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Time
import           RPKI.Reporting
import           RPKI.Metrics.Metrics


data PrometheusMetrics = PrometheusMetrics {
        rrdpCode        :: Vector Text Gauge,
        downloadTime    :: Vector Text Gauge,
        vrpCounter      :: Vector Text Gauge,        
        vrpCounterPerRepo :: Vector Text Gauge,
        fetchStatus       :: Vector Text Gauge,
        uniqueVrpCounter  :: Vector Text Gauge,
        validObjectNumberPerTa   :: Vector (Text, Text) Gauge,
        validObjectNumberPerRepo :: Vector (Text, Text) Gauge
    }
    deriving (Generic)


createPrometheusMetrics :: MonadIO m => Config -> m PrometheusMetrics
createPrometheusMetrics Config {..} = do

    void $ register ghcMetrics

    rrdpCode <- register
            $ vector ("url" :: Text)
            $ gauge (Info (metricsPrefix <> "rrdp_http_code") "HTTP code of the RRDP response")
    downloadTime <- register
            $ vector ("url" :: Text)
            $ gauge (Info (metricsPrefix <> "download_time") "Time of downloading repository (ms)")
    vrpCounter <- register
            $ vector ("trustanchor" :: Text)
            $ gauge (Info (metricsPrefix <> "vrp_total") "Number of original VRPs")
    vrpCounterPerRepo <- register
            $ vector ("repository" :: Text)
            $ gauge (Info (metricsPrefix <> "vrp_total") "Number of original VRPs")            
    uniqueVrpCounter <- register 
            $ vector ("trustanchor" :: Text)
            $ gauge (Info (metricsPrefix <> "unique_vrp_total") "Number of unique VRPs")
    fetchStatus <- register 
            $ vector ("repository" :: Text)
            $ gauge (Info (metricsPrefix <> "repo_fetch_status") 
                          ("0 if fetch for a repositry is not necessary (not enough time has passed), " <> 
                           "1 if fetch has failed, " <> 
                           "2 if fetch was successful but there are no updates, " <> 
                           "3 if data was successfully fetched from the repository"))
    validObjectNumberPerTa <- register
            $ vector ("trustanchor", "type")
            $ gauge (Info (metricsPrefix <> "object_total") "Number of valid objects of different types per TA")
    validObjectNumberPerRepo <- register
            $ vector ("repository", "type")
            $ gauge (Info (metricsPrefix <> "object_total") "Number of valid objects of different types per repository")

    pure $ PrometheusMetrics {..}


textualMetrics :: MonadIO m => m LBS.ByteString
textualMetrics = exportMetricsAsText

updatePrometheusForRepository :: (MonadIO m, MonadMonitor m) => RpkiURL -> TimeMs -> PrometheusMetrics -> m ()
updatePrometheusForRepository rpkiUrl downloadTime_ PrometheusMetrics {..} = do
    let (URI uri) = getURL rpkiUrl
    withLabel downloadTime uri $ flip setGauge $ fromIntegral $ unTimeMs downloadTime_


updatePrometheus :: (MonadIO m, MonadMonitor m) => Metrics -> PrometheusMetrics -> WorldVersion -> m ()
updatePrometheus rm PrometheusMetrics {..} _ = do    
    let grouped = groupedValidationMetric rm
    
    forM_ (MonoidalMap.toList $ grouped ^. #byTa) $ \(TaName name, metric) ->
        setObjectMetricsPerUrl validObjectNumberPerTa name 
                metric True vrpCounter
    forM_ (MonoidalMap.toList $ grouped ^. #byRepository) $ \(rpkiUrl, metric) -> 
        setObjectMetricsPerUrl validObjectNumberPerRepo (unURI $ getURL rpkiUrl) 
                metric False vrpCounterPerRepo
  where      
    setValidObjects prometheusVector url tag count = withLabel prometheusVector (url, tag)
            $ flip setGauge
            $ fromIntegral $ unCount count

    setObjectMetricsPerUrl prometheusVector url metric setUniqueVRPs vrpCounter' = do
        withLabel vrpCounter' url $ flip setGauge $ fromIntegral $ unCount $ metric ^. #vrpCounter

        when setUniqueVRPs $ withLabel uniqueVrpCounter url $ 
            flip setGauge $ fromIntegral $ unCount $ metric ^. #uniqueVrpNumber

        let totalCount = metric ^. #validCertNumber +
                         metric ^. #validRoaNumber +
                         metric ^. #validMftNumber +
                         metric ^. #validCrlNumber +
                         metric ^. #validGbrNumber +
                         metric ^. #validAspaNumber +
                         metric ^. #validBgpNumber 
        setValidObjects prometheusVector url "cer" $ metric ^. #validCertNumber
        setValidObjects prometheusVector url "roa" $ metric ^. #validRoaNumber
        setValidObjects prometheusVector url "mft" $ metric ^. #validMftNumber
        setValidObjects prometheusVector url "crl" $ metric ^. #validCrlNumber
        setValidObjects prometheusVector url "grb" $ metric ^. #validGbrNumber
        setValidObjects prometheusVector url "aspa" $ metric ^. #validAspaNumber
        setValidObjects prometheusVector url "bgp" $ metric ^. #validBgpNumber
        setValidObjects prometheusVector url "allobjects" totalCount
