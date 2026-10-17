{-# LANGUAGE StrictData         #-}

module RPKI.AppContext where
    
import           Control.Concurrent.STM (TVar)
import           GHC.Generics
import           RPKI.AppTypes
import           RPKI.AppState
import           RPKI.Config
import           RPKI.Logging
import           RPKI.Store.Database     (DB)

data AppContext = AppContext {
        logger            :: AppLogger, 
        config            :: Config,
        appState          :: AppState,
        database          :: TVar DB,
        executableVersion :: ExecutableVersion
    } 
    deriving stock (Generic)

getRtrLogger :: AppContext -> RtrLogger
getRtrLogger AppContext { logger = AppLogger {..} } = rtrLogger    

