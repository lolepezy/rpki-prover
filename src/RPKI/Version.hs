{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}

module RPKI.Version where

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
import           RPKI.Validation.ObjectValidation


-- It's some sequence of versions that is equal to the current 
-- timestamp in nanoseconds.
newtype WorldVersion = WorldVersion Int64
    deriving (Show, Eq, Ord, Generic, Serialise)

newtype DynamicState = DynamicState {
    world :: TVar WorldVersion
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

getWorldVerion :: DynamicState -> IO WorldVersion
getWorldVerion DynamicState {..} = readTVarIO world    