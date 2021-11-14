{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}

module RPKI.AppState where
    
import           Control.Monad (join)
import           Control.Concurrent.STM
import           Control.Lens
import           Data.Generics.Product.Typed
import           GHC.Generics
import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.AppTypes
import           RPKI.SLURM.SlurmProcessing
import           RPKI.SLURM.Types
import           RPKI.Time


data AppState = AppState {
        world         :: TVar (Maybe WorldVersion),
        validatedVrps :: TVar Vrps,
        filteredVrps  :: TVar Vrps,
        readSlurm     :: Maybe (ValidatorT IO Slurm)
    } deriving stock (Generic)

-- 
newAppState :: IO AppState
newAppState = do        
    atomically $ AppState <$> 
                    -- newTVar (WorldState (instantToVersion instant) NewVersion) <*>
                    newTVar Nothing <*>
                    newTVar mempty <*>
                    newTVar mempty <*>
                    pure Nothing

-- 
-- updateWorldVersion :: AppState -> IO WorldVersion
-- updateWorldVersion AppState {..} = do
--     Now now <- thisInstant    
--     let wolrdVersion = instantToVersion now
--     -- atomically $ writeTVar world $ WorldState wolrdVersion NewVersion
--     atomically $ writeTVar world $ Just wolrdVersion
--     pure wolrdVersion

setCurrentVersion :: AppState -> WorldVersion -> STM ()
setCurrentVersion AppState {..} = writeTVar world . Just


newWorldVersion :: IO WorldVersion
newWorldVersion = instantToVersion . unNow <$> thisInstant        


completeVersion :: AppState -> WorldVersion -> Vrps -> Maybe Slurm -> STM Vrps
completeVersion AppState {..} worldVersion vrps slurm = do 
    writeTVar world $ Just worldVersion
    writeTVar validatedVrps vrps
    let slurmed = maybe vrps (`applySlurm` vrps) slurm
    writeTVar filteredVrps slurmed
    pure slurmed

getWorldVerionIO :: AppState -> IO (Maybe WorldVersion)
getWorldVerionIO AppState {..} = atomically $ readTVar world

getOrCreateWorldVerion :: AppState -> IO WorldVersion
getOrCreateWorldVerion AppState {..} = 
    join $ atomically $ do 
        readTVar world >>= \case
            Nothing -> pure $ newWorldVersion
            Just wv -> pure $ pure wv 

versionToMoment :: WorldVersion -> Instant
versionToMoment (WorldVersion nanos) = fromNanoseconds nanos

instantToVersion :: Instant -> WorldVersion
instantToVersion = WorldVersion . toNanoseconds


-- Block on version updates
waitForNewVersion :: AppState -> WorldVersion -> STM (WorldVersion, Vrps)
waitForNewVersion AppState {..} knownWorldVersion = do     
    readTVar world >>= \case 
        Just w         
            | w > knownWorldVersion -> (w,) <$> readTVar filteredVrps
            | otherwise             -> retry
        _                           -> retry

-- waitForCompleteVersion :: AppState -> STM WorldVersion
-- waitForCompleteVersion AppState {..} =
--     readTVar world >>= \case
--             WorldState w CompletedVersion -> pure w
--             _                             -> retry

waitForVersion :: AppState -> STM WorldVersion
waitForVersion AppState {..} =
    maybe retry pure =<< readTVar world