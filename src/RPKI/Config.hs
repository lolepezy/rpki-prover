{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module RPKI.Config where

import GHC.Conc
import Numeric.Natural
import Data.Int
import Data.Text (Text)
import Data.Word ( Word16 )

import Data.Hourglass
import Data.Maybe (fromMaybe)

import RPKI.Domain
import RPKI.Logging
import RPKI.Util (toNatural, convert)
import GHC.Generics (Generic)

import RPKI.Store.Base.Serialisation

import Data.Version
import qualified Paths_rpki_prover as Autogen

rpkiProverVersion :: Text
rpkiProverVersion = convert $ "rpki-prover-" <> showVersion Autogen.version

data Parallelism = Parallelism {
        cpuCount         :: Natural,
        cpuParallelism   :: Natural,
        fetchParallelism :: Natural
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data FetchConfig = FetchConfig {
        rsyncTimeout       :: Seconds,
        rsyncSlowThreshold :: Seconds,
        rrdpTimeout        :: Seconds,
        rrdpSlowThreshold  :: Seconds,
        fetchLaunchWaitDuration  :: Seconds
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data Config = Config {        
        programBinaryPath         :: FilePath,
        rootDirectory             :: FilePath,
        talDirectory              :: FilePath,
        tmpDirectory              :: FilePath,
        cacheDirectory            :: FilePath,
        parallelism               :: Parallelism, 
        rsyncConf                 :: RsyncConf,
        rrdpConf                  :: RrdpConf,
        validationConfig          :: ValidationConfig,
        systemConfig              :: SystemConfig,
        httpApiConf               :: HttpApiConfig,
        rtrConfig                 :: Maybe RtrConfig,
        cacheCleanupInterval      :: Seconds,
        cacheLifeTime             :: Seconds,
        oldVersionsLifetime       :: Seconds,
        storageCompactionInterval :: Seconds,
        rsyncCleanupInterval      :: Seconds,
        lmdbSizeMb                :: Size,
        localExceptions           :: [FilePath],
        logLevel                  :: LogLevel,
        metricsPrefix             :: Text
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data RsyncConf = RsyncConf {
        rsyncClientPath   :: Maybe FilePath,
        rsyncRoot         :: FilePath,
        rsyncTimeout      :: Seconds,
        asyncRsyncTimeout :: Seconds,
        enabled           :: Bool,
        rsyncPrefetchUrls :: [RsyncURL]
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data RrdpConf = RrdpConf {
        tmpRoot          :: FilePath,
        maxSize          :: Size,
        rrdpTimeout      :: Seconds,
        asyncRrdpTimeout :: Seconds,
        enabled          :: Bool
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data ManifestProcessing = RFC6486_Strict | RFC9286
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)


data ValidationAlgorithm = FullEveryIteration | Incremental
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)



data ValidationConfig = ValidationConfig {    
        revalidationInterval           :: Seconds,
        rrdpRepositoryRefreshInterval  :: Seconds,
        rsyncRepositoryRefreshInterval :: Seconds,

        -- Do not retry to fetch a repository that failed 
        -- less than this many seconds ago
        minimalRepositoryRetryInterval :: Seconds,

        -- Maximum time for top-down validation for one TA
        topDownTimeout                 :: Seconds,
        
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

        -- Manimal allowed size of an individual object 
        minObjectSize                  :: Integer,

        validationAlgorithm            :: ValidationAlgorithm
    } 
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data AsyncFetchConfig = AsyncFetchConfig {
        -- Maximal download time of a repository beyond 
        -- which it is considered slow.    
        slowRepositoryThreshold :: Seconds,
        -- 
        checkPeriod             :: Seconds       
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data HttpApiConfig = HttpApiConfig {
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

data SystemConfig = SystemConfig {
        rsyncWorkerMemoryMb      :: Int,
        rrdpWorkerMemoryMb       :: Int,
        validationWorkerMemoryMb :: Int,
        cleanupWorkerMemoryMb    :: Int
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
-- that many IO operations (http downloads, LMDB reads, etc.) at once.
--
-- TODO There should be distinction between network operations and file/LMDB IO.
makeParallelism :: Natural -> Parallelism
makeParallelism cpus = Parallelism cpus (2 * cpus) (3 * cpus)

defaultConfig :: Config
defaultConfig = Config {    
    programBinaryPath = "rpki-prover",
    rootDirectory = "",
    talDirectory = "",
    tmpDirectory = "",
    cacheDirectory = "",
    parallelism = makeParallelism 2,
    rsyncConf = RsyncConf {
        rsyncClientPath = Nothing,
        rsyncRoot    = "",
        rsyncTimeout = 2 * 60,
        asyncRsyncTimeout = 15 * 60,
        enabled = True,
        rsyncPrefetchUrls = []
    },
    rrdpConf = RrdpConf {
        tmpRoot = "",
        maxSize = Size $ 1024 * 1024 * 1024,
        rrdpTimeout = 2 * 60,
        asyncRrdpTimeout = 10 * 60,
        enabled = True
    },
    validationConfig = ValidationConfig {
        revalidationInterval           = Seconds $ 13 * 60,
        rrdpRepositoryRefreshInterval  = Seconds 120,
        rsyncRepositoryRefreshInterval = Seconds $ 11 * 60,    
        minimalRepositoryRetryInterval = Seconds $ 10,    
        topDownTimeout                 = Seconds $ 60 * 60,    
        manifestProcessing             = RFC9286,
        maxCertificatePathDepth        = 32,
        maxTotalTreeSize               = 5_000_000,
        maxObjectSize                  = 32 * 1024 * 1024,
        -- every object contains at least 256 bytes of RSA key, 
        -- couple of dates and a few extensions
        minObjectSize                  = 300,
        maxTaRepositories              = 3000,
        validationAlgorithm            = FullEveryIteration
    },
    httpApiConf = HttpApiConfig {
        port = 9999
    },    
    systemConfig = SystemConfig {
        rsyncWorkerMemoryMb      = 1024,
        rrdpWorkerMemoryMb       = 1024,        
        validationWorkerMemoryMb = 2048,
        cleanupWorkerMemoryMb    = 512
    },
    rtrConfig                 = Nothing,
    cacheCleanupInterval      = Seconds $ 60 * 60 * 6,
    cacheLifeTime             = Seconds $ 60 * 60 * 24,
    oldVersionsLifetime       = Seconds $ 60 * 60 * 24,
    storageCompactionInterval = Seconds $ 60 * 60 * 120,
    rsyncCleanupInterval      = Seconds $ 60 * 60 * 24 * 30,
    lmdbSizeMb                = Size $ 32 * 1024,
    localExceptions = [],
    logLevel = defaultsLogLevel,
    metricsPrefix = "rpki_prover_"
}

defaultsLogLevel :: LogLevel
defaultsLogLevel = InfoL

defaultRtrConfig :: RtrConfig
defaultRtrConfig = RtrConfig { 
        rtrAddress = "localhost",
        rtrPort    = 8283,
        rtrLogFile = Nothing
    }
    
defaulPrefetchURLs :: [String]
defaulPrefetchURLs = [
        "rsync://rpki.afrinic.net/repository",
        "rsync://rpki.apnic.net/member_repository",
        "rsync://rpki-repo.registro.br/repo/",
        "rsync://repo-rpki.idnic.net/repo/",
        "rsync://0.sb/repo/",
        "rsync://rpki.co/repo/",
        "rsync://rpki-rps.arin.net/repository/",
        "rsync://rpki-repository.nic.ad.jp/ap/",
        "rsync://rsync.paas.rpki.ripe.net/repository/",
        "rsync://rpki.sub.apnic.net/repository/"
    ]    
    