{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}

module RPKI.AppState where
    
import           Control.Concurrent.STM
import           Control.Lens
import           Data.Generics.Product.Typed
import           GHC.Generics
import           Data.Set
import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.AppTypes
import           RPKI.SLURM.Types
import           RPKI.Time


data AppState = AppState {
        world           :: TVar WorldState,
        currentVrps     :: TVar Vrps,
        flatCurrentVrps :: TVar (Maybe (Set Vrp)),
        readSlurm       :: Maybe (ValidatorT IO Slurm)
    } deriving stock (Generic)

-- 
newAppState :: IO AppState
newAppState = do
    Now instant <- thisInstant        
    atomically $ AppState <$> 
                    newTVar (WorldState (instantToVersion instant) NewVersion) <*>
                    newTVar mempty <*>
                    newTVar Nothing <*>
                    pure Nothing

-- 
updateWorldVerion :: AppState -> IO WorldVersion
updateWorldVerion AppState {..} = do
    Now now <- thisInstant    
    let wolrdVersion = instantToVersion now
    atomically $ writeTVar world $ WorldState wolrdVersion NewVersion
    pure wolrdVersion

completeCurrentVersion :: AppState -> Vrps -> STM ()
completeCurrentVersion AppState {..} vrps = do 
    modifyTVar' world (& typed @VersionState .~ CompletedVersion)
    writeTVar currentVrps vrps
    writeTVar flatCurrentVrps (Just $ allVrps vrps)

getWorldVerionIO :: AppState -> IO WorldVersion
getWorldVerionIO = atomically . getWorldVerion

getWorldVerion :: AppState -> STM WorldVersion
getWorldVerion AppState {..} = (^. typed @WorldVersion) <$> readTVar world    

versionToMoment :: WorldVersion -> Instant
versionToMoment (WorldVersion nanos) = fromNanoseconds nanos

instantToVersion :: Instant -> WorldVersion
instantToVersion = WorldVersion . toNanoseconds

allCurrentVrps :: AppState -> STM (Set Vrp)
allCurrentVrps AppState {..} = do 
    readTVar flatCurrentVrps >>= \case
        Nothing -> do             
            a <- allVrps <$> readTVar currentVrps
            writeTVar flatCurrentVrps (Just a)
            pure a
        Just c -> pure c

-- Block on version updates
waitForNewCompleteVersion :: AppState -> WorldVersion -> STM (WorldVersion, Vrps)
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