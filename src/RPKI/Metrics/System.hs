{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RPKI.Metrics.System where

import           Control.Lens
import           GHC.Generics
import           Data.Monoid.Generic
import           Data.Text
import           RPKI.Time
import           RPKI.AppTypes
import           RPKI.Reporting
import           RPKI.Store.Base.Serialisation

data ResourceUsage = ResourceUsage {
        aggregatedCpuTime :: CPUTime,
        maxMemory         :: MaxMemory
    }
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass (TheBinary)
    deriving Semigroup via GenericSemigroup ResourceUsage   
    deriving Monoid    via GenericMonoid ResourceUsage       

newtype SystemMetrics = SystemMetrics {
        resources :: MetricMap ResourceUsage
    }
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass (TheBinary)
    deriving Semigroup via GenericSemigroup SystemMetrics   
    deriving Monoid    via GenericMonoid SystemMetrics    


data SystemInfo = SystemInfo {
        metrics   :: SystemMetrics,
        startTime :: Instant
    }   
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass (TheBinary)

newSystemInfo :: Instant -> SystemInfo
newSystemInfo = SystemInfo mempty 

cpuMemMetric :: Text -> CPUTime -> MaxMemory -> SystemMetrics
cpuMemMetric scope cpuTime maxMemory' = SystemMetrics {
        resources = updateMetricInMap 
                        (newScope scope) 
                        ((& #aggregatedCpuTime %~ (<> cpuTime)) . (& #maxMemory %~ (<> maxMemory')))
                        mempty
    }