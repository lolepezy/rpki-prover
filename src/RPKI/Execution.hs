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
import           RPKI.Validation.ObjectValidation

-- It's some sequence of versions that is equal to the current 
-- timestamp in nanoseconds.
newtype WorldVersion = WorldVersion Int64
    deriving (Show, Eq, Ord, Generic, Serialise)

newtype DynamicState = DynamicState {
    world :: TVar WorldVersion
} deriving stock (Generic)

data AppContext = AppContext {
    logger :: AppLogger, 
    config :: Config,
    dynamicState :: DynamicState
} deriving stock (Generic)

-- 
createDynamicState :: IO DynamicState
createDynamicState = do
    Now now <- thisMoment
    let NanoSeconds nano = timeGetNanoSeconds now
    DynamicState <$> atomically (newTVar $ WorldVersion nano)

-- 
updateWorldVerion :: DynamicState -> IO WorldVersion
updateWorldVerion DynamicState {..} = do
    Now now <- thisMoment
    let NanoSeconds nano = timeGetNanoSeconds now
    let wolrdVersion = WorldVersion nano
    atomically $ writeTVar world wolrdVersion
    pure wolrdVersion




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