{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE OverloadedLabels   #-}

module RPKI.AppState where
    
import           Control.Lens
import           Control.Monad (join)
import           Control.Concurrent.STM
import           Data.Set
import           GHC.Generics
import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.AppTypes
import           RPKI.SLURM.SlurmProcessing
import           RPKI.SLURM.Types
import           RPKI.Time
import           RPKI.Metrics.System
import           Control.Monad.IO.Class


data AppState = AppState {
        world         :: TVar (Maybe WorldVersion),
        validatedVrps :: TVar Vrps,
        filteredVrps  :: TVar Vrps,
        validatedBgpSec :: TVar (Set BGPSecPayload),
        filteredBgpSec  :: TVar (Set BGPSecPayload),
        readSlurm     :: Maybe (ValidatorT IO Slurm),
        system        :: TVar SystemInfo
    } deriving stock (Generic)

data RtrPayloads = RtrPayloads {
        vrps   :: Set Vrp,
        bgpSec :: Set BGPSecPayload
    }
    deriving stock (Show, Eq, Generic)

-- 
newAppState :: IO AppState
newAppState = do        
    Now now <- thisInstant
    atomically $ AppState <$> 
                    newTVar Nothing <*>
                    newTVar mempty <*>
                    newTVar mempty <*>
                    newTVar mempty <*>
                    newTVar mempty <*>
                    pure Nothing <*>
                    newTVar (newSystemInfo now)

setCurrentVersion :: AppState -> WorldVersion -> STM ()
setCurrentVersion AppState {..} = writeTVar world . Just

newWorldVersion :: IO WorldVersion
newWorldVersion = instantToVersion . unNow <$> thisInstant        

completeVersion :: AppState -> WorldVersion -> Vrps -> Maybe Slurm -> STM Vrps
completeVersion AppState {..} worldVersion vrps slurm = do 
    writeTVar world $ Just worldVersion
    writeTVar validatedVrps vrps
    let slurmed = maybe vrps (`applySlurmToVrps` vrps) slurm
    writeTVar filteredVrps slurmed
    pure slurmed

getWorldVerionIO :: AppState -> IO (Maybe WorldVersion)
getWorldVerionIO AppState {..} = readTVarIO world

getOrCreateWorldVerion :: AppState -> IO WorldVersion
getOrCreateWorldVerion AppState {..} = 
    join $ atomically $ 
        maybe newWorldVersion pure <$> readTVar world

versionToMoment :: WorldVersion -> Instant
versionToMoment (WorldVersion nanos) = fromNanoseconds nanos

instantToVersion :: Instant -> WorldVersion
instantToVersion = WorldVersion . toNanoseconds


-- Block on version updates
waitForNewVersion :: AppState -> WorldVersion -> STM (WorldVersion, RtrPayloads)
waitForNewVersion appState@AppState {..} knownWorldVersion = do     
    readTVar world >>= \case 
        Just w         
            | w > knownWorldVersion -> (w,) <$> getRtrPayloads appState                
            | otherwise             -> retry
        _                           -> retry

waitForVersion :: AppState -> STM WorldVersion
waitForVersion AppState {..} =
    maybe retry pure =<< readTVar world


mergeSystemMetrics :: MonadIO m => SystemMetrics -> AppState -> m ()           
mergeSystemMetrics sm AppState {..} = 
    liftIO $ atomically $ modifyTVar' system (& #metrics %~ (<> sm))


getRtrPayloads :: AppState -> STM RtrPayloads    
getRtrPayloads AppState {..} = do 
    vrps   <- allVrps <$> readTVar filteredVrps
    bgpSec <- readTVar filteredBgpSec
    pure RtrPayloads {..}    