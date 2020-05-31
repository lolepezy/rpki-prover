{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}

module RPKI.Execution where

import           Data.String.Interpolate

import           Control.Concurrent.STM

import           Codec.Serialise
import           GHC.Generics

import           Data.Int

import           Data.Hourglass                   (timeGetNanoSeconds)
import           Time.Types

import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Repository
import           RPKI.TAL
import           RPKI.Logging
import           RPKI.Config
import           RPKI.Time
import           RPKI.Version
import           RPKI.Store.Database
import           RPKI.Store.Base.LMDB


data AppContext s = AppContext {
    logger :: AppLogger, 
    config :: Config,
    dynamicState :: DynamicState,
    database :: DB s
} deriving stock (Generic)




-- data Task = CheckTACertificate (TAL -> Task) |
--             FetchRepository (Repository -> Task) |
--             FetchAndValidate (Repository -> Task) |
--             TaskSequence [Task] |
--             Noop

-- init :: TAL -> [Task]
-- init tal = [
--         CheckTACertificate (\_ -> Noop)
--     ]



{-
  - TA certificate valdiation triggers the whole tree re-validation

  - Adding a *new* publication point from a certificate triggers repository validation

  - Repository validation means
    * download the point (or lookup for already prefetched)
    * re-validate something -- figure out what (in RIPE NCC 
        validator it's just re-validate all relevant TAs)
        
  - Adding an object should trigger
    * Validating the object against it's parent
    * Validating the whole tree down from the object

  - 

-} 