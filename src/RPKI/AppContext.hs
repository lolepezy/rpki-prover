{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData         #-}

module RPKI.AppContext where

import           GHC.Generics

import           RPKI.Config
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.RRDP.HttpContext
import           RPKI.Store.Database
import           RPKI.AppState

data AppBottleneck = AppBottleneck {
    cpuBottleneck :: Bottleneck,
    ioBottleneck :: Bottleneck
} deriving stock (Generic)

data AppContext s = AppContext {
    logger         :: AppLogger, 
    config         :: Config,
    versions       :: AppState,
    database       :: DB s,
    appBottlenecks :: AppBottleneck,
    httpContext    :: HttpContext    
} deriving stock (Generic)

