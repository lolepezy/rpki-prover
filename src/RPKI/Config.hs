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
    cacheCleanupInterval :: Seconds,
    cacheLifeTime        :: Seconds
} deriving stock (Show, Eq, Ord, Generic)

newtype RsyncConf = RsyncConf {
    rsyncRoot :: FilePath
} deriving stock (Show, Eq, Ord, Generic)

newtype Size = Size Int64
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (Num)

data RrdpConf = RrdpConf {
    tmpRoot :: FilePath,
    maxSize :: Size
} deriving stock (Show, Eq, Ord, Generic)

data ValidationConfig = ValidationConfig {
    revalidationInterval :: Seconds,
    rrdpRepositoryRefreshInterval :: Seconds,
    rsyncRepositoryRefreshInterval :: Seconds
} deriving stock (Show, Eq, Ord, Generic)

getParallelism :: Natural 
getParallelism = fromMaybe 1 $ toNatural numCapabilities
