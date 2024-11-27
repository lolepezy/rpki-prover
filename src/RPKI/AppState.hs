{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE StrictData         #-}

module RPKI.AppState where
    
import           Control.Concurrent.STM    
import           Control.Lens hiding (filtered)
import           Control.Monad (join)
import           Control.Monad.IO.Class

import qualified Data.ByteString                  as BS

import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics
import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.AppTypes
import           RPKI.SLURM.SlurmProcessing
import           RPKI.SLURM.Types
import           RPKI.Time
import           RPKI.Metrics.System
import           RPKI.RTR.Types
import           RPKI.Resources.Validity


data AppState = AppState {
        -- current world version
        world     :: TVar (Maybe WorldVersion),

        -- Sunset of the last validated payloads that 
        -- is feasible for RTR (VRPs, BGPSec certificates)
        validated :: TVar RtrPayloads,

        -- The same but filtered through SLURM 
        filtered  :: TVar RtrPayloads,

        -- Full RTR state sent to every RTR client.
        -- It is serialised once and sent to every new client 
        -- requesting the full state
        cachedBinaryPdus :: TVar (Maybe BS.ByteString),

        -- Function that re-reads SLURM file(s) after every re-validation
        readSlurm   :: Maybe (ValidatorT IO Slurm),

        -- Metadata about RTR server
        rtrState    :: TVar (Maybe RtrState),

        -- System metrics 
        system      :: TVar SystemInfo,

        -- Index for searching VRPs by a prefix used
        -- by the validity check
        prefixIndex :: TVar (Maybe PrefixIndex)

    } deriving stock (Generic)


uniqVrps :: Vrps -> Set AscOrderedVrp 
uniqVrps = mconcat . Prelude.map (Set.map AscOrderedVrp) . allVrps

mkRtrPayloads :: Vrps -> Set BGPSecPayload -> RtrPayloads
mkRtrPayloads vrps bgpSec = RtrPayloads { uniqueVrps = uniqVrps vrps, .. }

-- 
newAppState :: IO AppState
newAppState = do        
    Now now <- thisInstant
    atomically $ do 
        world       <- newTVar Nothing
        validated   <- newTVar mempty
        filtered    <- newTVar mempty        
        rtrState    <- newTVar Nothing
        readSlurm   <- pure Nothing
        system      <- newTVar (newSystemInfo now)        
        prefixIndex <- newTVar Nothing
        cachedBinaryPdus <- newTVar Nothing
        pure AppState {..}
                    

newWorldVersion :: IO WorldVersion
newWorldVersion = instantToVersion . unNow <$> thisInstant        

completeVersion :: AppState -> WorldVersion -> RtrPayloads -> Maybe Slurm -> STM RtrPayloads
completeVersion AppState {..} worldVersion rtrPayloads slurm = do 
    writeTVar world $ Just worldVersion
    writeTVar validated rtrPayloads
    let slurmed = maybe rtrPayloads (filterWithSLURM rtrPayloads) slurm
    writeTVar filtered slurmed
    -- invalidate serialised PDU cache with every new version
    writeTVar cachedBinaryPdus Nothing
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
            | w > knownWorldVersion -> (w,) <$> readRtrPayloads appState
            | otherwise             -> retry
        _                           -> retry

waitForAnyVersion :: AppState -> STM WorldVersion
waitForAnyVersion AppState {..} =
    maybe retry pure =<< readTVar world

mergeSystemMetrics :: MonadIO m => SystemMetrics -> AppState -> m ()           
mergeSystemMetrics sm AppState {..} = 
    liftIO $ atomically $ modifyTVar' system (& #metrics %~ (<> sm))

readRtrPayloads :: AppState -> STM RtrPayloads    
readRtrPayloads AppState {..} = readTVar filtered

filterWithSLURM :: RtrPayloads -> Slurm -> RtrPayloads
filterWithSLURM RtrPayloads {..} slurm =     
    mkRtrPayloads (slurm `applySlurmToVrps` vrps) 
                  (slurm `applySlurmBgpSec` bgpSec)

-- TODO Make it more generic for things that need to be recomoputed for each version 
-- and things that are computed on-demand.
cachedPduBinary :: AppState -> (RtrPayloads -> BS.ByteString) -> STM BS.ByteString
cachedPduBinary appState@AppState {..} f = 
    readTVar cachedBinaryPdus >>= \case     
        Nothing -> do            
            bs <- f <$> readRtrPayloads appState 
            writeTVar cachedBinaryPdus $ Just bs
            pure bs
        Just bs -> pure bs
