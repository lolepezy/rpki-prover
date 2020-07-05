{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module RPKI.Version where

import           Control.Concurrent.STM

import           Codec.Serialise
import           GHC.Generics

import           Data.Int

import           Data.Hourglass         (timeGetElapsedP, timeGetNanoSeconds)
import           Time.Types

import           RPKI.Logging
import           RPKI.Time


-- It's some sequence of versions that is equal to the current 
-- timestamp in nanoseconds.
newtype WorldVersion = WorldVersion Int64
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

newtype Versions = Versions {
    world :: TVar WorldVersion
} deriving stock (Generic)

data VersionState = NewVersion | FinishedVersion
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

-- 
createDynamicState :: IO Versions
createDynamicState = do
    Now (Instant now) <- thisInstant
    let NanoSeconds nano = timeGetNanoSeconds now
    Versions <$> atomically (newTVar $ WorldVersion nano)

-- 
updateWorldVerion :: Versions -> IO WorldVersion
updateWorldVerion Versions {..} = do
    Now now <- thisInstant    
    let wolrdVersion = WorldVersion $ toNanoseconds now
    atomically $ writeTVar world wolrdVersion
    pure wolrdVersion

getWorldVerion :: Versions -> IO WorldVersion
getWorldVerion Versions {..} = readTVarIO world    

versionToMoment :: WorldVersion -> Instant
versionToMoment (WorldVersion nanos) = fromNanoseconds nanos