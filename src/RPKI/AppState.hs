{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}

module RPKI.AppState where
    
import           Control.Concurrent.STM

import           Codec.Serialise
import           Control.Lens

import           Data.Generics.Product.Typed
import           Data.Int
import           GHC.Generics

import           Data.Set
import           RPKI.Domain
import           RPKI.Time


-- It's a sequence of versions that is equal to some monotonic  
-- clock timestamp in nanoseconds.
newtype WorldVersion = WorldVersion Int64
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

data VersionState = NewVersion | CompletedVersion
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

data WorldState = WorldState WorldVersion VersionState
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

data AppState = AppState {
    world       :: TVar WorldState,
    currentVrps :: TVar (Set Vrp)
} deriving stock (Generic)

-- 
newAppState :: IO AppState
newAppState = do
    Now instant <- thisInstant        
    atomically $ AppState <$> 
                    newTVar (WorldState (instantToVersion instant) NewVersion) <*>
                    newTVar mempty

-- 
updateWorldVerion :: AppState -> IO WorldVersion
updateWorldVerion AppState {..} = do
    Now now <- thisInstant    
    let wolrdVersion = instantToVersion now
    atomically $ writeTVar world $ WorldState wolrdVersion NewVersion
    pure wolrdVersion

completeCurrentVersion :: AppState -> Set Vrp -> STM ()
completeCurrentVersion AppState {..} vrps = do 
    modifyTVar' world (& typed @VersionState .~ CompletedVersion)
    writeTVar currentVrps vrps

getWorldVerionIO :: AppState -> IO WorldVersion
getWorldVerionIO = atomically . getWorldVerion

getWorldVerion :: AppState -> STM WorldVersion
getWorldVerion AppState {..} = (^. typed @WorldVersion) <$> readTVar world    

versionToMoment :: WorldVersion -> Instant
versionToMoment (WorldVersion nanos) = fromNanoseconds nanos

instantToVersion :: Instant -> WorldVersion
instantToVersion = WorldVersion . toNanoseconds

-- Block on version updates
waitForNewCompleteVersion :: AppState -> WorldVersion -> STM (WorldVersion, Set Vrp)
waitForNewCompleteVersion AppState {..} knownWorldVersion = do 
    readTVar world >>= \case 
        WorldState w CompletedVersion 
            | w > knownWorldVersion -> (w,) <$> readTVar currentVrps
            | otherwise              -> retry
        _                            -> retry

waitForCompleteVersion :: AppState -> STM WorldVersion
waitForCompleteVersion AppState {..} =
    readTVar world >>= \case
            WorldState w CompletedVersion -> pure w
            _                             -> retry