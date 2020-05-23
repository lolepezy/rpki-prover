{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module RPKI.Config where

import GHC.Conc
import Numeric.Natural

import Data.Hourglass
import Data.Maybe (fromMaybe)

import RPKI.Logging
import RPKI.Util (toNatural)
import GHC.Generics (Generic)

data Config = Config {
    parallelism :: Natural,
    rsyncConf :: RsyncConf,
    validationConfig :: ValidationConfig
} deriving stock (Show, Eq, Ord, Generic)

newtype RsyncConf = RsyncConf {
    rsyncRoot :: FilePath
} deriving stock (Show, Eq, Ord, Generic)

data ValidationConfig = ValidationConfig {
    refetchIntervalAfterRepositoryFailure :: Seconds
} deriving stock (Show, Eq, Ord, Generic)

getParallelism :: Natural 
getParallelism = fromMaybe 1 $ toNatural numCapabilities
