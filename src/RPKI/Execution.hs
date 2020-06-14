{-# LANGUAGE DerivingStrategies #-}

module RPKI.Execution where

import           GHC.Generics

import           RPKI.Logging
import           RPKI.Config
import           RPKI.Parallel
import           RPKI.Version
import           RPKI.Store.Database


data AppBottleneck = AppBottleneck {
    cpuBottleneck :: Bottleneck,
    ioBottleneck :: Bottleneck
} deriving stock (Generic)

data AppContext s = AppContext {
    logger       :: AppLogger, 
    config       :: Config,
    dynamicState :: DynamicState,
    database     :: DB s,
    appThreads   :: AppBottleneck
} deriving stock (Generic)
