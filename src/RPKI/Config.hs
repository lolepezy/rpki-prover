{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RecordWildCards  #-}

module RPKI.Config where

import GHC.Conc
import Numeric.Natural

import RPKI.Logging
import RPKI.Util (toNatural)

data Config = Config {
    parallelism :: Natural
}

newtype RsyncConf = RsyncConf {
    rsyncRoot :: FilePath
}

getParallelism :: Natural 
getParallelism = maybe 1 id $ toNatural $ numCapabilities

data AppContext = AppContext {
    logger :: AppLogger, 
    config :: Config,
    rsyncConf :: RsyncConf
}