{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RecordWildCards  #-}

module RPKI.Config where

import GHC.Conc

data Config = Config {
    parallelism :: Int
}

newtype RsyncConf = RsyncConf {
    rsyncRoot :: FilePath
}

getParallelism :: Int 
getParallelism = numCapabilities

