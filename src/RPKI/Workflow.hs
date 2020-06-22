{-# LANGUAGE DerivingStrategies #-}

module RPKI.Workflow where

import           GHC.Generics

import           RPKI.Config
import           RPKI.Domain
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Store.Database
import           RPKI.Version
import           RPKI.TopDown

import           RPKI.Store.Base.Storage
import           RPKI.AppContext
import Control.Monad (void)

data FlowTask
  = Validate WorldVersion
  | ValidateTACert WorldVersion
  | GC WorldVersion

-- runCacheGC = do
data TaskProcessor = TaskProcessor { 
    tasks :: ClosableQueue FlowTask 
}



executeTask :: Storage s => 
                AppContext s -> FlowTask -> TaName -> IO ()
executeTask appContext task taName = do
    case task of 
        ValidateTACert worldVersion -> do
            void $ validateTA appContext taName worldVersion
        