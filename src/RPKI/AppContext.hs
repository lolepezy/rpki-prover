{-# LANGUAGE DerivingStrategies #-}

module RPKI.AppContext where

import           GHC.Generics

import           RPKI.Logging
import           RPKI.Config
import           RPKI.Parallel
import           RPKI.Version
import           RPKI.Store.Database
import           RPKI.RRDP.HttpContext

data AppBottleneck = AppBottleneck {
    cpuBottleneck :: Bottleneck,
    ioBottleneck :: Bottleneck
} deriving stock (Generic)

data AppContext s = AppContext {
    logger         :: !AppLogger, 
    config         :: !Config,
    versions       :: !Versions,
    database       :: !(DB s),
    appBottlenecks :: !AppBottleneck,
    httpContext    :: !HttpContext
} deriving stock (Generic)

