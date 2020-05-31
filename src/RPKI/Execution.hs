{-# LANGUAGE DerivingStrategies #-}

module RPKI.Execution where

import           GHC.Generics

import           RPKI.Logging
import           RPKI.Config
import           RPKI.Version
import           RPKI.Store.Database


data AppContext s = AppContext {
    logger :: AppLogger, 
    config :: Config,
    dynamicState :: DynamicState,
    database :: DB s
} deriving stock (Generic)
