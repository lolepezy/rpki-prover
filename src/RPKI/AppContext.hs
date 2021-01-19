{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData         #-}

module RPKI.AppContext where

import           Control.Concurrent.STM (TVar)
import           GHC.Generics
import           RPKI.AppState
import           RPKI.Config
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.RRDP.HttpContext
import           RPKI.Store.Database


data AppBottleneck = AppBottleneck {
        cpuBottleneck :: Bottleneck,
        ioBottleneck :: Bottleneck
    } 
    deriving stock (Generic)

data AppContext s = AppContext {
        logger         :: AppLogger, 
        config         :: Config,
        appState       :: AppState,
        database       :: TVar (DB s),
        appBottlenecks :: AppBottleneck,
        httpContext    :: HttpContext        
    } 
    deriving stock (Generic)

