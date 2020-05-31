{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module RPKI.Version where

import           Control.Concurrent.STM

import           Codec.Serialise
import           GHC.Generics

import           Data.Int

import           Data.Hourglass         (timeGetNanoSeconds)
import           Time.Types

import           RPKI.Logging
import           RPKI.Time


-- It's some sequence of versions that is equal to the current 
-- timestamp in nanoseconds.
newtype WorldVersion = WorldVersion Int64
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

newtype DynamicState = DynamicState {
    world :: TVar WorldVersion
} deriving stock (Generic)


data VersionState = NewVersion | FinishedVersion
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)



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