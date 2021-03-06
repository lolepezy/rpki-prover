{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

module RPKI.Config where

import GHC.Conc
import Numeric.Natural
import Data.Int
import Data.Word

import Data.Hourglass
import Data.Maybe (fromMaybe)
import Data.Monoid

import RPKI.Util (toNatural)
import GHC.Generics (Generic)

data Parallelism = Parallelism {
    cpuParallelism :: Natural,
    ioParallelism :: Natural
} deriving stock (Show, Eq, Ord, Generic)

data Config = Config {
    talDirectory              :: FilePath,
    tmpDirectory              :: FilePath,
    cacheDirectory            :: FilePath,
    parallelism               :: Parallelism,
    rsyncConf                 :: RsyncConf,
    rrdpConf                  :: RrdpConf,
    validationConfig          :: ValidationConfig,
    httpApiConf               :: HttpApiConfig,
    rtrConfig                 :: Maybe RtrConfig,
    cacheCleanupInterval      :: Seconds,
    cacheLifeTime             :: Seconds,
    oldVersionsLifetime       :: Seconds,
    storageCompactionInterval :: Seconds,
    lmdbSize                  :: Size
} deriving stock (Show, Eq, Ord, Generic)

data RsyncConf = RsyncConf {
    rsyncRoot    :: FilePath,
    rsyncTimeout :: Seconds
} deriving stock (Show, Eq, Ord, Generic)

newtype Size = Size Int64
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (Num)
    deriving Semigroup via Sum Size
    deriving Monoid via Sum Size

data RrdpConf = RrdpConf {
    tmpRoot     :: FilePath,
    maxSize     :: Size,
    rrdpTimeout :: Seconds
} deriving stock (Show, Eq, Ord, Generic)

data ValidationConfig = ValidationConfig {
    revalidationInterval           :: Seconds,
    rrdpRepositoryRefreshInterval  :: Seconds,
    rsyncRepositoryRefreshInterval :: Seconds,    
    dontFetch                      :: Bool
} deriving stock (Show, Eq, Ord, Generic)

data HttpApiConfig = HttpApiConfig {
    port :: Word16    
} deriving stock (Show, Eq, Ord, Generic)

data RtrConfig = RtrConfig {
    rtrAddress :: String,
    rtrPort    :: Int16
} deriving stock (Show, Eq, Ord, Generic)

getRtsCpuCount :: Natural 
getRtsCpuCount = fromMaybe 1 $ toNatural numCapabilities

setCpuCount :: Natural -> IO ()
setCpuCount = setNumCapabilities . fromIntegral


defaultConfig :: Config
defaultConfig = Config {
    talDirectory = "",
    tmpDirectory = "",
    cacheDirectory = "",
    parallelism = Parallelism 4 64,
    rsyncConf = RsyncConf {
        rsyncRoot    = "",
        rsyncTimeout = 7 * 60
    },
    rrdpConf = RrdpConf {
        tmpRoot = "",
        maxSize = Size $ 1024 * 1024 * 1024,
        rrdpTimeout = 7 * 60
    },
    validationConfig = ValidationConfig {
        revalidationInterval           = Seconds $ 13 * 60,
        rrdpRepositoryRefreshInterval  = Seconds 120,
        rsyncRepositoryRefreshInterval = Seconds $ 11 * 60,    
        dontFetch                      = False
    },
    httpApiConf = HttpApiConfig {
        port = 9999
    },
    rtrConfig                 = Nothing,
    cacheCleanupInterval      = Seconds $ 60 * 120,
    cacheLifeTime             = Seconds $ 60 * 60 * 72,
    oldVersionsLifetime       = Seconds $ 60 * 60 * 10,
    storageCompactionInterval = Seconds $ 60 * 60 * 24,
    lmdbSize                  = Size $ 8 * 1024 * 1024 * 1024
}

defaultRtrConfig :: RtrConfig
defaultRtrConfig = RtrConfig { 
        rtrAddress = "localhost",
        rtrPort    = 8283
    }