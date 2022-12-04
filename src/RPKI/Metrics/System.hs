{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE OverloadedLabels     #-}

module RPKI.Metrics.System where

import           Control.Lens
import           GHC.Generics
import           Data.Monoid.Generic
import           Data.Text
import           RPKI.Time
import           RPKI.AppTypes
import           RPKI.Reporting
import           RPKI.Store.Base.Serialisation

data CpuMetrics = CpuMetrics {
        aggregatedCpuTime :: CPUTime,
        maxMemory         :: MaxMemory
    }
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass (TheBinary)
    deriving Semigroup via GenericSemigroup CpuMetrics   
    deriving Monoid    via GenericMonoid CpuMetrics    


data SystemMetrics = SystemMetrics {
        resources :: MetricMap CpuMetrics
    }
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass (TheBinary)
    deriving Semigroup via GenericSemigroup SystemMetrics   
    deriving Monoid    via GenericMonoid SystemMetrics    

cpuMemMetric :: Text -> CPUTime -> MaxMemory -> SystemMetrics
cpuMemMetric scope cpuTime maxMemory' = SystemMetrics {
        resources = updateMetricInMap 
                        (newScope scope) 
                        ((& #aggregatedCpuTime %~ (<> cpuTime)) . (& #maxMemory %~ (<> maxMemory')))
                        mempty
    }