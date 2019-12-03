{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RecordWildCards  #-}

module RPKI.Config where

import GHC.Conc

data Config = Config {
  parallelism :: Int
}

getParallelism :: Int 
getParallelism = numCapabilities
