{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module RPKI.Config where

import GHC.Conc
import Numeric.Natural

import Data.Hourglass
import Data.Maybe (fromMaybe)

import RPKI.Util (toNatural)
import GHC.Generics (Generic)

data Parallelism = Parallelism {
    cpuParallelism :: Natural,
    ioParallelism :: Natural
} deriving stock (Show, Eq, Ord, Generic)

data Config = Config {
    talDirectory :: FilePath,
    parallelism :: Parallelism,
    rsyncConf :: RsyncConf,
    rrdpConf :: RrdpConf,
    validationConfig :: ValidationConfig
} deriving stock (Show, Eq, Ord, Generic)

newtype RsyncConf = RsyncConf {
    rsyncRoot :: FilePath
} deriving stock (Show, Eq, Ord, Generic)

newtype RrdpConf = RrdpConf {
    tmpRoot :: FilePath
} deriving stock (Show, Eq, Ord, Generic)

data ValidationConfig = ValidationConfig {
    rrdpRepositoryRefreshInterval :: Seconds,
    rsyncRepositoryRefreshInterval :: Seconds
} deriving stock (Show, Eq, Ord, Generic)

getParallelism :: Natural 
getParallelism = fromMaybe 1 $ toNatural numCapabilities
