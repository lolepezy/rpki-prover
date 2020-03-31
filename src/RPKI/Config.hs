{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RecordWildCards  #-}

module RPKI.Config where

import GHC.Conc

import RPKI.Logging

data Config = Config {
    parallelism :: Int
}

newtype RsyncConf = RsyncConf {
    rsyncRoot :: FilePath
}

getParallelism :: Int 
getParallelism = numCapabilities

data AppContext = AppContext {
    logger :: AppLogger, 
    config :: Config,
    rsyncConf :: RsyncConf
}