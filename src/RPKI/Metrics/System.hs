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
import           Data.Semigroup
import           Data.Text
import           RPKI.Time
import           RPKI.AppTypes
import           RPKI.Reporting
import           RPKI.Store.Base.Serialisation


newtype AggregatedCPUTime = AggregatedCPUTime CPUTime
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass (TheBinary)
    deriving newtype (Num)
    deriving Semigroup via Sum AggregatedCPUTime
    deriving Monoid via Sum AggregatedCPUTime

newtype LatestCPUTime = LatestCPUTime CPUTime
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass (TheBinary)
    deriving newtype (Num)
    deriving Semigroup via Last LatestCPUTime

instance Monoid LatestCPUTime where
    mempty = LatestCPUTime $ CPUTime 0

data ResourceUsage = ResourceUsage {
        latestCpuTime      :: LatestCPUTime,
        aggregatedCpuTime   :: AggregatedCPUTime,
        aggregatedClockTime :: TimeMs,
        maxMemory           :: MaxMemory
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
        metrics     :: SystemMetrics,
        startUpTime :: Instant
    }   
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass (TheBinary)

newSystemInfo :: Instant -> SystemInfo
newSystemInfo = SystemInfo mempty 

cpuMemMetric :: Text -> CPUTime -> TimeMs -> MaxMemory -> SystemMetrics
cpuMemMetric scope cpuTime clockTime maxMemory' = SystemMetrics {
        resources = updateMetricInMap 
                        (newScope scope) 
                        ((& #latestCpuTime %~ (<> LatestCPUTime cpuTime)) . 
                         (& #aggregatedCpuTime %~ (<> AggregatedCPUTime cpuTime)) . 
                         (& #aggregatedClockTime %~ (<> clockTime)) .  
                         (& #maxMemory %~ (<> maxMemory')))
                        mempty
    }