{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module RPKI.Config where

import Control.Lens
import GHC.Conc
import Numeric.Natural
import Data.Int
import Data.Text (Text)
import Data.Word ( Word16 )

import Data.Hourglass
import Data.Maybe (fromMaybe)
import Data.Generics.Product.Typed

import RPKI.Domain
import RPKI.AppTypes
import RPKI.Logging
import RPKI.Util (toNatural)
import RPKI.Time 
import GHC.Generics (Generic)

import RPKI.Store.Base.Serialisation

data ApiSecured a = Hidden a
                  | Public a
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (TheBinary)
    
instance Show a => Show (ApiSecured a) where
    show = show . configValue

configValue :: ApiSecured a -> a
configValue = \case 
    Hidden a -> a
    Public a -> a

data Parallelism = Parallelism {
        cpuCount         :: Natural,
        cpuParallelism   :: Natural,
        fetchParallelism :: Natural
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data FetchConfig = FetchConfig {
        rsyncTimeout             :: Seconds,
        rrdpTimeout              :: Seconds,
        erikTimeout              :: Seconds,
        fetchLaunchWaitDuration  :: Seconds,
        minFetchInterval         :: Seconds,
        maxFetchInterval         :: Seconds,
        maxFailedBackoffInterval :: Seconds
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data StorageConfig = StorageConfig {
        rwTransactionTimeout  :: Seconds,
        walCheckpointInterval :: Seconds
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)    

data Config = Config {        
        programBinaryPath         :: ApiSecured FilePath,
        rootDirectory             :: ApiSecured FilePath,
        talDirectory              :: ApiSecured FilePath,
        extraTalsDirectories      :: ApiSecured [FilePath],
        tmpDirectory              :: ApiSecured FilePath,
        cacheDirectory            :: ApiSecured FilePath,
        proverRunMode             :: ProverRunMode,
        parallelism               :: Parallelism, 
        rsyncConf                 :: RsyncConf,
        rrdpConf                  :: RrdpConf,
        erikConf                  :: ErikConf,
        validationConfig          :: ValidationConfig,
        systemConfig              :: SystemConfig,
        httpApiConf               :: HttpApiConfig,
        rtrConfig                 :: Maybe RtrConfig,
        storageConfig             :: StorageConfig,
        cacheCleanupInterval      :: Seconds,
        shortLivedCacheLifeTime   :: Seconds,
        longLivedCacheLifeTime    :: Seconds,
        versionNumberToKeep       :: Natural,
        storageCompactionInterval :: Seconds,
        rsyncCleanupInterval      :: Seconds,
        localExceptions           :: ApiSecured [FilePath],
        logLevel                  :: LogLevel,
        metricsPrefix             :: Text,
        withValidityApi           :: Bool
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data RsyncConf = RsyncConf {
        clientPath                :: Maybe (ApiSecured FilePath),
        rsyncRoot                 :: ApiSecured FilePath,
        enabled                   :: Bool,
        prefetchUrls              :: [RsyncURL],
        perHostLimit              :: Int,
        repositoryRefreshInterval :: Seconds
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data ErikConf = ErikConf {
        relays               :: [URI],
        maxSize              :: Size,
        -- | Maximum concurrent Erik work items and relay downloads per worker.
        parallelism          :: Natural,
        -- | Cap on relay downloads in flight against any single relay.
        relayParallelism     :: Natural,
        -- How many FQDN to Erik-fetch at once.
        fqdnParallelism      :: Natural,
        -- | Hard cap on one download from one relay, connection included. A relay
        --   is only worth talking to while it is fast: one that is not is better
        --   left to RRDP and rsync than waited on, and everything a relay serves
        --   is small (an index, a partition, a single object).
        downloadTimeout      :: Seconds,
        erikRefreshInterval  :: Seconds
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data RrdpConf = RrdpConf {
        tmpRoot                   :: ApiSecured FilePath,
        maxSize                   :: Size,
        enabled                   :: Bool,
        repositoryRefreshInterval :: Seconds,
        -- Minimal interval between forced snapshot fetches -- we don't want to overload repositories
        forcedSnapshotMinInterval :: Seconds
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data ManifestProcessing = RFC6486_Strict | RFC9286
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data ValidationAlgorithm = FullEveryIteration | Incremental
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data ProverRunMode = OneOffMode FilePath | ServerMode
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data ValidationConfig = ValidationConfig {    
        revalidationInterval           :: Seconds,        

        -- How often TA certificates are downloaded and validated.
        taCertificateRefreshInterval   :: Seconds,

        -- This is legacy and will be deleted at some point
        manifestProcessing             :: ManifestProcessing,

        -- Maximal object tree depth measured in number of CAs
        maxCertificatePathDepth        :: Int,

        -- How many objects we allow in the tree for one TA.
        -- There needs to be some finite number to limit total
        -- tree validation and prevent DOS attacks.
        maxTotalTreeSize               :: Int,

        -- How many different repositories we allow to add
        -- during one TA top-down validation
        maxTaRepositories              :: Int,

        -- Maximal allowed size of an individual object 
        maxObjectSize                  :: Integer,

        -- Minimal allowed size of an individual object 
        minObjectSize                  :: Integer,

        validationRFC                  :: ValidationRFC,
        validationAlgorithm            :: ValidationAlgorithm,

        minimalRevalidationInterval :: Seconds
    } 
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)


newtype HttpApiConfig = HttpApiConfig {
        port :: Word16    
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data RtrConfig = RtrConfig {
        rtrAddress :: String,
        rtrPort    :: Int16,
        rtrLogFile :: Maybe String          
    } 
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

-- | Everything one kind of worker process is bounded by: how long it may run
-- (wall-clock), how much CPU time it may burn, how much memory its RTS is
-- allowed and how much IO it may do -- all in one place instead of scattered
-- across 'RsyncConf'\/'RrdpConf'\/'ErikConf', 'ValidationConfig' and
-- 'SystemConfig' the way they used to be. A worker watches its own timeout,
-- CPU time and IO the same way ('RPKI.Worker.dieAfterTimeout',
-- 'RPKI.Worker.dieOfOveruse'); memory is enforced by the RTS itself via the
-- @-M@ flag built from 'memoryMb'.
--
-- The @max*Mb@ fields are how much IO a worker process is allowed to do
-- before it gives up and exits, the same way it gives up on CPU time or
-- wall-clock time. 'Nothing' means "don't watch this one". Note that
-- incoming traffic is only what the worker downloads itself over HTTP -- an
-- external rsync client is a separate process, so what it transfers doesn't
-- get counted as traffic, it lands in the disk IO of the worker that spawned
-- it instead.
data WorkerLimits = WorkerLimits {
        -- Named 'workerTimeout', not 'timeout' -- the latter clashes with
        -- 'System.Timeout.timeout', imported unqualified all over the place.
        workerTimeout        :: Seconds,
        cpuLimit             :: Seconds,
        memoryMb             :: Int,
        maxIncomingTrafficMb :: Maybe Int,
        maxDiskReadMb        :: Maybe Int,
        maxDiskWriteMb       :: Maybe Int
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data SystemConfig = SystemConfig {
        rsyncWorkerLimits      :: WorkerLimits,
        rrdpWorkerLimits       :: WorkerLimits,
        erikWorkerLimits       :: WorkerLimits,
        validationWorkerLimits :: WorkerLimits,
        cleanupWorkerLimits    :: WorkerLimits
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

getRtsCpuCount :: Natural 
getRtsCpuCount = fromMaybe 1 $ toNatural numCapabilities

setCpuCount :: Natural -> IO ()
setCpuCount = setNumCapabilities . fromIntegral


-- Create 2 times more asyncs/tasks than there're capabilities. In most 
-- tested cases it seems to be beneficial for the CPU utilisation ¯\_(ツ)_/¯.    
-- 
-- Hardcoded (not sure it makes sense to make it configurable). Allow for
-- that many IO operations (http downloads, database reads, etc.) at once.
--
-- TODO There should be distinction between network operations and file/database IO.
newParallelism :: Natural -> Parallelism
newParallelism cpus = makeParallelismF cpus (2 * cpus)

makeParallelismF :: Natural -> Natural -> Parallelism
makeParallelismF cpus = Parallelism cpus (2 * cpus)

defaultConfig :: Config
defaultConfig = Config {    
    programBinaryPath = Hidden "rpki-prover",
    rootDirectory = Hidden "",
    talDirectory = Hidden "",
    extraTalsDirectories = Hidden [],
    tmpDirectory = Hidden "",
    cacheDirectory = Hidden "",
    proverRunMode = ServerMode,
    parallelism = newParallelism 2,
    rsyncConf = RsyncConf {
        clientPath = Nothing,
        rsyncRoot    = Hidden "",
        enabled = True,
        prefetchUrls = [],
        perHostLimit = 5,
        repositoryRefreshInterval = 11 * minutes
    },
    rrdpConf = RrdpConf {
        enabled = True,
        tmpRoot = Hidden "",
        maxSize = Size $ 1024 * 1024 * 1024,        
        repositoryRefreshInterval  = 2 * minutes,
        forcedSnapshotMinInterval  = 12 * hours
    },
    erikConf = ErikConf {
        relays              = [],
        maxSize             = Size $ 20 * 1024 * 1024,
        parallelism         = 8,
        -- Up to fqdnParallelism * relayParallelism downloads can be in flight
        -- against one relay, 32 here. A download is mostly round trips, so a
        -- big FQDN takes time inversely proportional to relayParallelism:
        -- rpki.afrinic.net takes 69s with 3 and 27s with 8. A relay 4ms away
        -- answered in ~12ms with anything up to 16 of our downloads in flight,
        -- and started to queue them somewhere above 24.
        relayParallelism    = 8,
        fqdnParallelism     = 4,
        downloadTimeout     = Seconds 10,
        erikRefreshInterval = Seconds 30
    },
    validationConfig = ValidationConfig {
        revalidationInterval           = 15 * minutes,                
        taCertificateRefreshInterval   = 10 * minutes,        
        manifestProcessing             = RFC9286,
        maxCertificatePathDepth        = 32,
        maxTotalTreeSize               = 5_000_000,
        maxObjectSize                  = 32 * 1024 * 1024,
        -- every object contains at least 256 bytes of RSA key,
        -- couple of dates and a few extensions
        minObjectSize                  = 300,
        maxTaRepositories              = 1000,
        validationRFC                  = StrictRFC,
        validationAlgorithm            = FullEveryIteration,
        minimalRevalidationInterval    = Seconds 30
    },
    httpApiConf = HttpApiConfig {
        port = 9999
    },
    systemConfig = SystemConfig {
        rsyncWorkerLimits = WorkerLimits {
            workerTimeout  = 11 * minutes,
            cpuLimit = 30 * minutes,
            memoryMb = 1024,
            -- It never downloads anything itself, the external rsync client does that,
            -- so this is just a sanity check to stop a worker that downloads anything at all.
            maxIncomingTrafficMb = Just 0,
            maxDiskReadMb        = Just $ 6 * gigabytes,
            -- it's pretty large mainly because of the SQLite WAL (and other) amplifications
            maxDiskWriteMb       = Just $ 8 * gigabytes
        },
        rrdpWorkerLimits = WorkerLimits {
            workerTimeout  = 11 * minutes,
            cpuLimit = 30 * minutes,
            memoryMb = 1024,
            maxIncomingTrafficMb = Just $ 2 * gigabytes,
            maxDiskReadMb        = Just $ 6 * gigabytes,
            maxDiskWriteMb       = Just $ 16 * gigabytes
        },
        erikWorkerLimits = WorkerLimits {
            workerTimeout  = 15 * minutes,
            cpuLimit = 30 * minutes,
            memoryMb = 1024,
            maxIncomingTrafficMb = Just $ 2 * gigabytes,
            maxDiskReadMb        = Just $ 6 * gigabytes,
            maxDiskWriteMb       = Just $ 16 * gigabytes
        },
        validationWorkerLimits = WorkerLimits {
            workerTimeout  = 1 * hour,
            cpuLimit = 3 * hour,
            memoryMb = 2048,
            -- It downloads nothing, TA certificates are fetched by the main process
            maxIncomingTrafficMb = Just 0,
            maxDiskReadMb        = Just $ 10 * gigabytes,
            maxDiskWriteMb       = Just $ 3 * gigabytes
        },
        cleanupWorkerLimits = WorkerLimits {
            workerTimeout  = 300,
            -- Cleanup runs with 2 capabilities (-N2).
            cpuLimit = 20 * minutes,
            memoryMb = 512,
            maxIncomingTrafficMb = Nothing,
            maxDiskReadMb        = Just 32768,
            maxDiskWriteMb       = Just 32768
        }
    },
    rtrConfig                 = Nothing,
    storageConfig = StorageConfig {       
        rwTransactionTimeout = 15 * minutes,
        walCheckpointInterval = 1 * minutes
    },
    cacheCleanupInterval      = 6 * hours,    
    versionNumberToKeep       = 3,
    storageCompactionInterval = 5 * days,
    rsyncCleanupInterval      = 30 * days,
    localExceptions = Hidden [],
    logLevel = defaultsLogLevel,
    metricsPrefix = "rpki_prover_",
    withValidityApi = False,
    ..
}
  where
    shortLivedCacheLifeTime = 24 * hours
    longLivedCacheLifeTime  = 10 * days
    minutes = Seconds 60
    hour = hours
    days = 24 * hours
    hours = Seconds $ 60 * 60
    -- The max*Mb fields above are denominated in MB, so this is MB-per-GB
    gigabytes = 1024


adjustConfig :: Config -> Config
adjustConfig config = config 
        -- Cache must be cleaned up at least as often as the 
        -- lifetime of the objects in it    
        & #cacheCleanupInterval %~ (`min` (config ^. #longLivedCacheLifeTime))
        -- to accomodate for a weird case of longLivedCacheLifeTime < shortLivedCacheLifeTime
        -- we still want some correctness here, so the "short" one should be shorter
        & #shortLivedCacheLifeTime %~ (`min` (config ^. #longLivedCacheLifeTime))

adjustWorkerConfig :: Config -> Timebox -> Config
adjustWorkerConfig config (Timebox timeout) = config
        -- There's no point in having RW-transaction timeout
        -- longer than the worker timeout
        & #storageConfig . #rwTransactionTimeout %~ (`min` safeTimeout)
  where
    safeTimeout = max (Seconds 1) (timeout - Seconds 1)

defaultsLogLevel :: LogLevel
defaultsLogLevel = InfoL

defaultRtrConfig :: RtrConfig
defaultRtrConfig = RtrConfig { 
        rtrAddress = "localhost",
        rtrPort    = 8283,
        rtrLogFile = Nothing
    }
    
-- This is a heuristic list of rsync repositories that is currently out there
-- and not using them will result in too many small fetches from the same repository.
defaultPrefetchURLs :: [String]
defaultPrefetchURLs = [
        "rsync://rpki.afrinic.net/repository",
        "rsync://rpki.apnic.net/member_repository",
        "rsync://rpki-repo.registro.br/repo/",
        "rsync://repo-rpki.idnic.net/repo/",
        "rsync://0.sb/repo/",
        "rsync://rpki.co/repo/",
        "rsync://rpki-rps.arin.net/repository/",
        "rsync://rpki-repository.nic.ad.jp/ap/",
        "rsync://rsync.paas.rpki.ripe.net/repository/",
        "rsync://rpki.sub.apnic.net/repository/",
        "rsync://rpki.cnnic.cn/rpki/A9162E3D0000/",
        "rsync://rpki-repo.as207960.net/repo/"
    ]    

defaultTalUrls :: [(String, String)]
defaultTalUrls = [
        ("afrinic.tal", "https://rpki.afrinic.net/tal/afrinic.tal"),
        ("apnic.tal", "https://tal.apnic.net/tal-archive/apnic-rfc7730-https.tal"),
        ("arin.tal", "https://www.arin.net/resources/manage/rpki/arin.tal"),
        ("lacnic.tal", "https://www.lacnic.net/innovaportal/file/4983/1/lacnic.tal"),
        ("ripe.tal", "https://tal.rpki.ripe.net/ripe-ncc.tal")
    ]        
    
newFetchConfig :: Config -> FetchConfig
newFetchConfig config = let
        SystemConfig {..} = config ^. typed @SystemConfig
        rsyncTimeout = rsyncWorkerLimits ^. #workerTimeout
        rrdpTimeout  = rrdpWorkerLimits ^. #workerTimeout
        erikTimeout  = erikWorkerLimits ^. #workerTimeout
        fetchLaunchWaitDuration = Seconds 30
        minFetchInterval = Seconds 30
        maxFetchInterval = Seconds 300
        maxFailedBackoffInterval = Seconds $ 30 * 60
    in FetchConfig {..}
