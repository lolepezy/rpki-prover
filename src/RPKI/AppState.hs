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
import qualified Data.Set as Set


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

getWorldVerionIO :: AppState -> IO WorldVersion
getWorldVerionIO AppState {..} = readTVarIO world    

getWorldVerion :: AppState -> STM WorldVersion
getWorldVerion AppState {..} = readTVar world    

versionToMoment :: WorldVersion -> Instant
versionToMoment (WorldVersion nanos) = fromNanoseconds nanos

instantToVersion :: Instant -> WorldVersion
instantToVersion (Instant t) = 
    let NanoSeconds nano = timeGetNanoSeconds t
    in WorldVersion nano


-- TODO Probably redifine it to have more explicit/stable criteria
hasVrps :: AppState -> STM Bool
hasVrps AppState {..} = 
    not . Set.null <$> readTVar currentVrps

-- Block on version updates
listenWorldVersion :: AppState -> WorldVersion -> STM (WorldVersion, Set Vrp)
listenWorldVersion AppState {..} knownWorldVersion = do 
    w <- readTVar world
    if w > knownWorldVersion
        then (w,) <$> readTVar currentVrps
        else retry
