{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE StrictData         #-}

module RPKI.AppState where
    
import           Control.Lens
import           Control.Monad (join)
import           Control.Concurrent.STM
import           Data.Set
import           Data.Monoid.Generic
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
        world     :: TVar (Maybe WorldVersion),
        validated :: TVar RtrPayloads,
        filtered  :: TVar RtrPayloads,
        readSlurm :: Maybe (ValidatorT IO Slurm),
        system    :: TVar SystemInfo
    } deriving stock (Generic)

data RtrPayloads = RtrPayloads {
        vrps     :: Vrps,
        -- Compute it only if required by RTR or API, otherwise don't allocate space for it
        flatVrps :: ~(Set Vrp),
        bgpSec   :: Set BGPSecPayload
    }
    deriving stock (Show, Eq, Generic)
    deriving Semigroup via GenericSemigroup RtrPayloads   
    deriving Monoid    via GenericMonoid RtrPayloads           

mkRtrPayloads :: Vrps -> Set BGPSecPayload -> RtrPayloads
mkRtrPayloads vrps bgpSec = RtrPayloads { flatVrps = allVrps vrps, .. }

-- 
newAppState :: IO AppState
newAppState = do        
    Now now <- thisInstant
    atomically $ AppState <$> 
                    newTVar Nothing <*>
                    newTVar mempty <*>
                    newTVar mempty <*>
                    pure Nothing <*>
                    newTVar (newSystemInfo now)

setCurrentVersion :: AppState -> WorldVersion -> STM ()
setCurrentVersion AppState {..} = writeTVar world . Just

newWorldVersion :: IO WorldVersion
newWorldVersion = instantToVersion . unNow <$> thisInstant        

completeVersion :: AppState -> WorldVersion -> RtrPayloads -> Maybe Slurm -> STM RtrPayloads
completeVersion AppState {..} worldVersion rtrPayloads slurm = do 
    writeTVar world $ Just worldVersion
    modifyTVar' validated (<> rtrPayloads)
    let slurmed = maybe rtrPayloads (filterWithSLURM rtrPayloads) slurm
    modifyTVar' filtered (<> maybe rtrPayloads (filterWithSLURM rtrPayloads) slurm)
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
            | w > knownWorldVersion -> (w,) <$> readTVar filtered
            | otherwise             -> retry
        _                           -> retry

waitForVersion :: AppState -> STM WorldVersion
waitForVersion AppState {..} =
    maybe retry pure =<< readTVar world


mergeSystemMetrics :: MonadIO m => SystemMetrics -> AppState -> m ()           
mergeSystemMetrics sm AppState {..} = 
    liftIO $ atomically $ modifyTVar' system (& #metrics %~ (<> sm))


readRtrPayloads :: AppState -> STM RtrPayloads    
readRtrPayloads AppState {..} = readTVar filtered

filterWithSLURM :: RtrPayloads -> Slurm -> RtrPayloads 
filterWithSLURM RtrPayloads {..} slurm =     
    mkRtrPayloads (slurm `applySlurmToVrps` vrps) (slurm `applySlurmBgpSec` bgpSec)