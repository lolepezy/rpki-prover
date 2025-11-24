{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE RecordWildCards   #-}

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
import RPKI.Logging
import RPKI.Util (toNatural)
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
        fetchLaunchWaitDuration  :: Seconds,
        cpuLimit                 :: Seconds,
        minFetchInterval         :: Seconds,
        maxFetchInterval         :: Seconds,
        maxFailedBackoffInterval :: Seconds
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
        cacheCleanupInterval      :: Seconds,
        shortLivedCacheLifeTime   :: Seconds,
        longLivedCacheLifeTime    :: Seconds,
        versionNumberToKeep       :: Natural,
        storageCompactionInterval :: Seconds,
        rsyncCleanupInterval      :: Seconds,
        lmdbSizeMb                :: Size,
        localExceptions           :: ApiSecured [FilePath],
        logLevel                  :: LogLevel,
        metricsPrefix             :: Text,
        withValidityApi           :: Bool
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data RsyncConf = RsyncConf {
        rsyncClientPath   :: Maybe (ApiSecured FilePath),
        rsyncRoot         :: ApiSecured FilePath,
        rsyncTimeout      :: Seconds,
        cpuLimit          :: Seconds,
        enabled           :: Bool,
        rsyncPrefetchUrls :: [RsyncURL],
        rsyncPerHostLimit :: Int
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data ErikConf = ErikConf {
        maxSize     :: Size,
        parallelism :: Natural
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data RrdpConf = RrdpConf {
        tmpRoot     :: ApiSecured FilePath,
        maxSize     :: Size,
        rrdpTimeout :: Seconds,
        cpuLimit    :: Seconds,
        enabled     :: Bool
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
        rrdpRepositoryRefreshInterval  :: Seconds,
        rsyncRepositoryRefreshInterval :: Seconds,

        -- Do not retry to fetch a repository that failed 
        -- less than this many seconds ago
        minimalRepositoryRetryInterval :: Seconds,

        -- Minimal interval between forced snapshot fetches --
        -- we don't want to overload repositories
        rrdpForcedSnapshotMinInterval :: Seconds,

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
makeParallelism cpus = Parallelism cpus (2 * cpus) (2 * cpus)

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
    parallelism = makeParallelism 2,
    rsyncConf = RsyncConf {
        rsyncClientPath = Nothing,
        rsyncRoot    = Hidden "",
        rsyncTimeout = 11 * 60,
        cpuLimit = 30 * 60,    
        enabled = True,
        rsyncPrefetchUrls = [],
        rsyncPerHostLimit = 5
    },
    rrdpConf = RrdpConf {
        tmpRoot = Hidden "",
        maxSize = Size $ 1024 * 1024 * 1024,
        rrdpTimeout = 7 * minutes,
        cpuLimit = 30 * minutes,
        enabled = True
    },
    erikConf = ErikConf {
        maxSize = Size $ 20 * 1024 * 1024,
        parallelism = 10
    },
    validationConfig = ValidationConfig {
        revalidationInterval           = 15 * minutes,
        rrdpRepositoryRefreshInterval  = 2 * minutes,
        rsyncRepositoryRefreshInterval = 11 * minutes,    
        minimalRepositoryRetryInterval = Seconds 10,    
        rrdpForcedSnapshotMinInterval  = 12 * hours,                
        topDownTimeout                 = 1 * hour,
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
        rsyncWorkerMemoryMb      = 1024,
        rrdpWorkerMemoryMb       = 1024,        
        validationWorkerMemoryMb = 2048,
        cleanupWorkerMemoryMb    = 512
    },
    rtrConfig                 = Nothing,
    cacheCleanupInterval      = 6 * hours,    
    versionNumberToKeep       = 3,
    storageCompactionInterval = 5 * days,
    rsyncCleanupInterval      = 30 * days,
    lmdbSizeMb                = Size $ 32 * 1024,
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


adjustConfig :: Config -> Config
adjustConfig config = config 
        -- Cache must be cleaned up at least as often as the 
        -- lifetime of the objects in it    
        & #cacheCleanupInterval %~ (`min` (config ^. #longLivedCacheLifeTime))
        -- to accomodate for a weird case of longLivedCacheLifeTime < shortLivedCacheLifeTime
        -- we still want some correctness here, so the "short" one should be shorter
        & #shortLivedCacheLifeTime %~ (`min` (config ^. #longLivedCacheLifeTime))


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
        rsyncConfig = config ^. typed @RsyncConf
        rrdpConfig = config ^. typed @RrdpConf
        rsyncTimeout = rsyncConfig ^. #rsyncTimeout
        rrdpTimeout  = rrdpConfig ^. #rrdpTimeout        
        fetchLaunchWaitDuration = Seconds 30         
        cpuLimit = max (rrdpConfig ^. #cpuLimit) (rsyncConfig ^. #cpuLimit)        
        minFetchInterval = Seconds 30
        maxFetchInterval = Seconds 300
        maxFailedBackoffInterval = Seconds $ 30 * 60
    in FetchConfig {..}    