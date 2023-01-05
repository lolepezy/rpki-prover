{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE RecordWildCards    #-}

module RPKI.AppContext where
    
import           Control.Concurrent.STM (TVar)
import           GHC.Generics
import           RPKI.AppState
import           RPKI.Config
import           RPKI.Logging
import           RPKI.Store.Database

data AppContext s = AppContext {
        logger   :: AppLogger, 
        config   :: Config,
        appState :: AppState,
        database :: TVar (DB s)        
    } 
    deriving stock (Generic)

getRtrLogger :: AppContext s -> AppLogger
getRtrLogger AppContext {..} = logger    

