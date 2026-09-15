{-# LANGUAGE StrictData #-}

module RPKI.Metrics.System where

import           Control.Lens
import           GHC.Generics
import           Data.Monoid.Generic
import           Data.Semigroup
import           Data.Text                 (Text)
import           RPKI.Time
import           RPKI.AppTypes
import           RPKI.Metrics.Process
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

newtype MaxSize = MaxSize Size
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)
    deriving newtype (Num, Bounded)
    deriving Semigroup via Max MaxSize
    deriving Monoid    via Max MaxSize

data AvgMemory = AvgMemory {
        totalMemory :: Sum MaxMemory,
        samples     :: Sum Int
    }
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass (TheBinary)
    deriving Semigroup via GenericSemigroup AvgMemory   

instance Monoid AvgMemory where
    mempty = AvgMemory 0 0

newAvgMemory :: MaxMemory -> AvgMemory
newAvgMemory mem = AvgMemory (Sum mem) 1

getAvgMemory :: AvgMemory -> MaxMemory
getAvgMemory (AvgMemory (Sum (MaxMemory total)) (Sum count)) = 
    if count == 0 
        then 0 
        else MaxMemory (total `div` fromIntegral count)

data ResourceUsage = ResourceUsage {
        latestCpuTime       :: LatestCPUTime,
        aggregatedCpuTime   :: AggregatedCPUTime,
        aggregatedClockTime :: TimeMs,
        -- | Largest Haskell heap any run under this scope ever reached.
        maxRtsHeap           :: MaxMemory,
        avgRtsHeap           :: AvgMemory,
        -- | Largest resident size any run under this scope ever reached.
        -- Same kind of number as 'maxRtsHeap' -- both are high-water marks over
        -- a whole run -- so the two are directly comparable.
        maxProcessRSS       :: MaxMemory,
        avgProcessRSS       :: AvgMemory,
        -- | High-water marks over the runs that happened under this scope, i.e.
        -- the most any single run of it ever downloaded or moved to and from disk.
        -- They are maxima rather than totals because that is what the per-worker
        -- limits in 'IoLimits' are set against.
        maxIncomingTraffic  :: MaxSize,
        maxDiskRead         :: MaxSize,
        maxDiskWrite        :: MaxSize
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

resourceUsageMetric :: Text -> TimeMs -> ProcessStats -> SystemMetrics
resourceUsageMetric scope clockTime ProcessStats {..} = let
        DiskIO { diskRead = readBytes, diskWrite = writtenBytes } = statDiskIO
    in SystemMetrics {
        resources = updateMetricInMap
                        (newScope scope)
                        ((#latestCpuTime %~ (<> LatestCPUTime statCpuTime)) .
                         (#aggregatedCpuTime %~ (<> AggregatedCPUTime statCpuTime)) .
                         (#aggregatedClockTime %~ (<> clockTime)) .
                         (#maxRtsHeap %~ (<> statMaxRtsHeap)) .
                         (#avgRtsHeap %~ (<> newAvgMemory statMaxRtsHeap)) .
                         (#maxProcessRSS %~ (<> statProcessRss)) .
                         (#avgProcessRSS %~ (<> newAvgMemory statProcessRss)) .
                         (#maxIncomingTraffic %~ (<> MaxSize statIncomingTraffic)) .
                         (#maxDiskRead %~ (<> MaxSize readBytes)) .
                         (#maxDiskWrite %~ (<> MaxSize writtenBytes)))
                        mempty
    }
