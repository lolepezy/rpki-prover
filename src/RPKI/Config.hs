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
    talDirectory         :: FilePath,
    parallelism          :: Parallelism,
    rsyncConf            :: RsyncConf,
    rrdpConf             :: RrdpConf,
    validationConfig     :: ValidationConfig,
    httpApiConf          :: HttpApiConf,
    cacheCleanupInterval :: Seconds,
    cacheLifeTime        :: Seconds,
    oldVersionsLifetime  :: Seconds
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
    revalidationInterval :: Seconds,
    rrdpRepositoryRefreshInterval :: Seconds,
    rsyncRepositoryRefreshInterval :: Seconds,
    -- allow repositories to be down for a while before completely ignoring their objects
    repositoryGracePeriod :: Seconds
} deriving stock (Show, Eq, Ord, Generic)

data HttpApiConf = HttpApiConf {
    port :: Int16    
} deriving stock (Show, Eq, Ord, Generic)

getParallelism :: Natural 
getParallelism = fromMaybe 1 $ toNatural numCapabilities

setParallelism :: Natural -> IO ()
setParallelism = setNumCapabilities . fromIntegral
