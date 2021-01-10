{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StrictData #-}

module RPKI.Metrics where

import           Control.Lens                     ((^.), (%~), (&))
import           Data.Generics.Product.Typed

import qualified Data.ByteString.Lazy             as LBS
import           Data.Text                        (Text)
import           Data.Int                         (Int64)
import qualified Data.Set                         as Set

import           Data.Hourglass
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


data PrometheusMetrics = PrometheusMetrics {
        rrdpCode :: Vector Text Gauge,
        vrpNumber :: Vector Text Gauge,
        objectNumber :: Vector (Text, Text) Gauge
    }
    deriving (Generic)

    
-- createPrometheusMetrics :: Applicative f => f ()
createPrometheusMetrics = do
    rrdpCode <- register 
            $ vector ("url" :: Text) 
            $ gauge (Info "rrdp_http_code" "HTTP code of the RRDP response")
    vrpNumber <- register 
            $ vector ("trustanchor" :: Text) 
            $ gauge (Info "vrp_number" "Number of unique VRPs")
    objectNumber <- register 
            $ vector ("trustanchor", "type") 
            $ gauge (Info "object_number" "Number of objects of different types")

    pure $ PrometheusMetrics {..}


setHttpCode :: (MonadMonitor m, MonadIO m) => PrometheusMetrics -> RrdpURL -> Int -> m ()    
setHttpCode PrometheusMetrics {..} (RrdpURL (URI u)) code = 
    withLabel rrdpCode u $ \g -> setGauge g (fromIntegral code)


textualMetrics :: MonadIO m => m LBS.ByteString
textualMetrics = exportMetricsAsText