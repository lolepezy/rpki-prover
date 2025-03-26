{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE RecordWildCards    #-}

module RPKI.AppContext where
    
import           Control.Concurrent.STM (TVar)
import           GHC.Generics
import           RPKI.AppTypes
import           RPKI.AppState
import           RPKI.Config
import           RPKI.Logging
import           RPKI.Logging.Types
import           RPKI.Store.Database    (DB)

data AppContext s = AppContext {
        logger            :: AppLogger, 
        config            :: Config,
        appState          :: AppState,
        database          :: TVar (DB s),
        executableVersion :: ExecutableVersion
    } 
    deriving stock (Generic)

getRtrLogger :: AppContext s -> RtrLogger
getRtrLogger AppContext { logger = AppLogger {..} } = rtrLogger    

