{-# LANGUAGE DerivingStrategies #-}

module RPKI.Execution where

import           GHC.Generics

import           RPKI.Logging
import           RPKI.Config
import           RPKI.Parallel
import           RPKI.Version
import           RPKI.Store.Database


data AppThreads = AppThreads {
    cpuThreads :: Threads,
    ioThreads :: Threads
}

data AppContext s = AppContext {
    logger :: AppLogger, 
    config :: Config,
    dynamicState :: DynamicState,
    database :: DB s,
    appThreads :: AppThreads
} deriving stock (Generic)
