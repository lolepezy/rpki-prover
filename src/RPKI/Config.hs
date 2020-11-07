{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

module RPKI.Config where

import GHC.Conc
import Numeric.Natural
import Data.Int

import Data.Hourglass
import Data.Maybe (fromMaybe)

import RPKI.Util (toNatural)
import GHC.Generics (Generic)

data Parallelism = Parallelism {
    cpuParallelism :: Natural,
    ioParallelism :: Natural
} deriving stock (Show, Eq, Ord, Generic)

data Config = Config {
    talDirectory              :: FilePath,
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
    rsyncRoot :: FilePath,
    rsyncTimeout :: Seconds
} deriving stock (Show, Eq, Ord, Generic)

newtype Size = Size Int64
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (Num)

data RrdpConf = RrdpConf {
    tmpRoot :: FilePath,
    maxSize :: Size,
    rrdpTimeout :: Seconds
} deriving stock (Show, Eq, Ord, Generic)

data ValidationConfig = ValidationConfig {
    revalidationInterval           :: Seconds,
    rrdpRepositoryRefreshInterval  :: Seconds,
    rsyncRepositoryRefreshInterval :: Seconds,    
    repositoryGracePeriod          :: Maybe Seconds
} deriving stock (Show, Eq, Ord, Generic)

data HttpApiConfig = HttpApiConfig {
    port :: Int16    
} deriving stock (Show, Eq, Ord, Generic)

data RtrConfig = RtrConfig {
    rtrAddress :: String,
    rtrPort :: Int16
} deriving stock (Show, Eq, Ord, Generic)

getRtsCpuCount :: Natural 
getRtsCpuCount = fromMaybe 1 $ toNatural numCapabilities

setCpuCount :: Natural -> IO ()
setCpuCount = setNumCapabilities . fromIntegral
