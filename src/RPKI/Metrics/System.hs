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
import           RPKI.Reporting
import           RPKI.Store.Base.Serialisation

data CpuMetrics = CpuMetrics {
        aggregatedCpuTime :: CPUTime
    }
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass (TheBinary)
    deriving Semigroup via GenericSemigroup CpuMetrics   
    deriving Monoid    via GenericMonoid CpuMetrics    


data SystemMetrics = SystemMetrics {
        cpus :: MetricMap CpuMetrics        
    }
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass (TheBinary)
    deriving Semigroup via GenericSemigroup SystemMetrics   
    deriving Monoid    via GenericMonoid SystemMetrics    

cpuMetric :: Text -> CPUTime -> SystemMetrics
cpuMetric scope cpuTime = SystemMetrics {
        cpus = updateMetricInMap 
                    (newScope scope) 
                    (& #aggregatedCpuTime %~ (<> cpuTime)) mempty
    }