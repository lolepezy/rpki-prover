{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}

module RPKI.AppState where
    
import           Control.Concurrent.STM

import           Codec.Serialise
import           GHC.Generics

import           Data.Int

import           Data.Hourglass         (timeGetNanoSeconds)
import           Time.Types

import           Data.Set
import           RPKI.Domain
import           RPKI.Time


-- It's some sequence of versions that is equal to the current 
-- timestamp in nanoseconds.
newtype WorldVersion = WorldVersion Int64
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

data AppState = AppState {
    world :: TVar WorldVersion,
    currentVrps :: TVar (Set Vrp)
} deriving stock (Generic)

data VersionState = NewVersion | FinishedVersion
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

-- 
newAppState :: IO AppState
newAppState = do
    Now (Instant now) <- thisInstant
    let NanoSeconds nano = timeGetNanoSeconds now
    atomically $ AppState <$> 
                    newTVar (WorldVersion nano) <*>
                    newTVar mempty

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


-- Block on version updates
listenWorldVersion :: AppState -> Maybe WorldVersion -> STM (WorldVersion, Set Vrp)
listenWorldVersion AppState {..} knownWorldVersion = do 
    w <- readTVar world
    case knownWorldVersion of 
        Nothing    -> (w,) <$> readTVar currentVrps
        Just known ->     
            if w > known
                then (w,) <$> readTVar currentVrps
                else retry
