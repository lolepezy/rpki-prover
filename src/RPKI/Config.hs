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
    storageDefragmentInterval :: Seconds
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
    repositoryGracePeriod          :: Maybe Seconds
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
