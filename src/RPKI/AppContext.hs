{-# LANGUAGE StrictData         #-}

module RPKI.AppContext where
    
import           Control.Concurrent.STM (TVar)
import           GHC.Generics
import           RPKI.AppTypes
import           RPKI.AppState
import           RPKI.Config
import           RPKI.Logging
import           RPKI.Store.Database     (DB)

-- | `s` is now a phantom type parameter retained for backward-compatibility.
-- The actual database is always SQLite.DB regardless of `s`.
data AppContext s = AppContext {
        logger            :: AppLogger, 
        config            :: Config,
        appState          :: AppState,
        database          :: TVar DB,
        executableVersion :: ExecutableVersion
    } 
    deriving stock (Generic)

getRtrLogger :: AppContext s -> RtrLogger
getRtrLogger AppContext { logger = AppLogger {..} } = rtrLogger    

