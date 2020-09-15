{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards            #-}

module RPKI.AppState where
    
import           Control.Concurrent.STM

import           Codec.Serialise
import           GHC.Generics

import           Data.Int

import           Data.Hourglass         (timeGetNanoSeconds)
import           Time.Types

import           RPKI.Time



-- It's some sequence of versions that is equal to the current 
-- timestamp in nanoseconds.
newtype WorldVersion = WorldVersion Int64
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

newtype AppState = AppState {
    world :: TVar WorldVersion
} deriving stock (Generic)

data VersionState = NewVersion | FinishedVersion
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

-- 
createDynamicState :: IO AppState
createDynamicState = do
    Now (Instant now) <- thisInstant
    let NanoSeconds nano = timeGetNanoSeconds now
    AppState <$> atomically (newTVar $ WorldVersion nano)

-- 
updateWorldVerion :: AppState -> IO WorldVersion
updateWorldVerion AppState {..} = do
    Now now <- thisInstant    
    let wolrdVersion = WorldVersion $ toNanoseconds now
    atomically $ writeTVar world wolrdVersion
    pure wolrdVersion

getWorldVerion :: AppState -> IO WorldVersion
getWorldVerion AppState {..} = readTVarIO world    

versionToMoment :: WorldVersion -> Instant
versionToMoment (WorldVersion nanos) = fromNanoseconds nanos

instantToVersion :: Instant -> WorldVersion
instantToVersion (Instant t) = 
    let NanoSeconds nano = timeGetNanoSeconds t
    in WorldVersion nano