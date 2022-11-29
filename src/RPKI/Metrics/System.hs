{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StrictData           #-}

module RPKI.Metrics.System where

import           Control.Lens
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import           GHC.Generics
import           Data.Map.Monoidal.Strict
import           Data.Monoid
import           Data.Monoid.Generic
import           RPKI.Domain
import           RPKI.Time
import           RPKI.Reporting


data CpuMetrics = CpuMetrics {
        aggregatedCpuTime :: CPUTime
    }
    deriving stock (Show, Eq, Ord, Generic)    
    deriving Semigroup via GenericSemigroup CpuMetrics   
    deriving Monoid    via GenericMonoid CpuMetrics    


data SystemMetrics = SystemMetrics {
        cpus :: MetricMap CpuMetrics        
    }
    deriving stock (Show, Eq, Ord, Generic)    
    deriving Semigroup via GenericSemigroup SystemMetrics   
    deriving Monoid    via GenericMonoid SystemMetrics    
