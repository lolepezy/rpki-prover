{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE StrictData         #-}

module RPKI.AppState where
    
import           Control.Concurrent.STM    
import           Control.DeepSeq
import           Control.Lens hiding (filtered)
import           Control.Monad (join)
import           Control.Monad.IO.Class

import qualified Data.ByteString                  as BS

import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import qualified Data.Map.Strict                  as Map
import qualified Data.Vector                      as V
import           GHC.Generics
import           System.Process.Typed
import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.AppTypes
import           RPKI.Logging
import           RPKI.SLURM.SlurmProcessing
import           RPKI.SLURM.Types
import           RPKI.Repository
import           RPKI.Time
import           RPKI.Metrics.System
import           RPKI.RTR.Protocol
import           RPKI.RTR.Types
import           RPKI.Resources.Validity


data AppState = AppState {
        -- current world version
        world     :: TVar (Maybe WorldVersion),

        -- Subset of the last validated payloads that 
        -- is feasible for RTR (VRPs, BGPSec certificates)
        validated :: TVar RtrPayloads,

        -- The same but filtered through SLURM 
        filtered  :: TVar RtrPayloads,

        -- Full binary RTR state sent to every RTR client.
        -- It is serialised once per RTR protocol version 
        -- and sent to every new client requesting the full state.
        -- It is an optimisation to avoid serialising the same 
        -- RTR state for every new client.
        cachedBinaryRtrPdus :: TVar (Map.Map ProtocolVersion BS.ByteString),

        -- Function that re-reads SLURM file(s) after every re-validation
        readSlurm   :: Maybe (ValidatorT IO Slurm),

        -- Metadata about RTR server
        rtrState    :: TVar (Maybe RtrState),

        -- System metrics 
        system      :: TVar SystemInfo,

        -- Index for searching VRPs by a prefix used
        -- by the validity check
        prefixIndex :: TVar (Maybe PrefixIndex),

        runningRsyncClients :: TVar (Map.Map Pid WorkerInfo),

        fetcheables :: TVar Fetcheables
        
    } deriving stock (Generic)


uniqVrps :: Vrps -> V.Vector AscOrderedVrp 
uniqVrps vrps = let 
        s = Set.fromList $ V.toList $ unVrps vrps
    in V.fromListN (Set.size s) $ Prelude.map AscOrderedVrp $ Set.toList s

mkRtrPayloads :: PerTA Vrps -> Set BGPSecPayload -> RtrPayloads
mkRtrPayloads vrps bgpSec = RtrPayloads { uniqueVrps = uniqVrps $ allTAs vrps, .. }

-- 
newAppState :: IO AppState
newAppState = do        
    Now now <- thisInstant
    atomically $ do 
        world       <- newTVar Nothing
        validated   <- newTVar mempty
        filtered    <- newTVar mempty        
        rtrState    <- newTVar Nothing        
        system      <- newTVar (newSystemInfo now)        
        prefixIndex <- newTVar Nothing
        cachedBinaryRtrPdus <- newTVar mempty
        runningRsyncClients <- newTVar mempty
        fetcheables <- newTVar mempty
        let readSlurm = Nothing
        pure AppState {..}
                    

newWorldVersion :: IO WorldVersion
newWorldVersion = instantToVersion . unNow <$> thisInstant        

completeVersion :: AppState -> WorldVersion -> RtrPayloads -> Maybe Slurm -> STM RtrPayloads
completeVersion AppState {..} worldVersion rtrPayloads slurm = do 
    writeTVar world $! Just $! worldVersion
    writeTVar validated rtrPayloads
    let slurmed = maybe rtrPayloads (filterWithSLURM rtrPayloads) slurm
    writeTVar filtered slurmed        

    -- invalidate serialised PDU cache with every new version
    writeTVar cachedBinaryRtrPdus mempty
    pure $! slurmed

updatePrefixIndex :: AppState -> RtrPayloads -> STM ()
updatePrefixIndex AppState {..} rtrPayloads = 
    writeTVar prefixIndex $! 
        force $ Just $ createPrefixIndex $ rtrPayloads ^. #uniqueVrps

getOrCreateWorldVerion :: AppState -> IO WorldVersion
getOrCreateWorldVerion AppState {..} = 
    join $ atomically $ 
        maybe newWorldVersion pure <$> readTVar world

versionToInstant :: WorldVersion -> Instant
versionToInstant (WorldVersion nanos) = fromNanoseconds nanos

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
    liftIO $ atomically $ modifyTVar' system (#metrics %~ (<> sm))

updateRsyncClient :: MonadIO m => WorkerMessage -> AppState -> m ()           
updateRsyncClient message AppState {..} =     
    liftIO $ atomically $ modifyTVar' runningRsyncClients $ 
        case message of 
            AddWorker wi     -> Map.insert (wi ^. #workerPid) wi
            RemoveWorker pid -> Map.delete pid
        

removeExpiredRsyncProcesses :: MonadIO m => AppState -> m [(Pid, WorkerInfo)]
removeExpiredRsyncProcesses AppState {..} = liftIO $ do 
    Now now <- thisInstant
    atomically $ do 
        clients <- readTVar runningRsyncClients
        let expired = filter (\(_, WorkerInfo {..}) -> endOfLife < now) $ Map.toList clients        
        writeTVar runningRsyncClients $ foldr (\(pid, _) m -> Map.delete pid m) clients expired 
        pure expired  

readRtrPayloads :: AppState -> STM RtrPayloads    
readRtrPayloads AppState {..} = readTVar filtered

filterWithSLURM :: RtrPayloads -> Slurm -> RtrPayloads
filterWithSLURM RtrPayloads {..} slurm =     
    mkRtrPayloads (slurm `applySlurmToVrps` vrps) 
                  (slurm `applySlurmBgpSec` bgpSec)

-- TODO Make it more generic for things that need to be recomoputed for each version 
-- and things that are computed on-demand.
cachedPduBinary :: AppState -> ProtocolVersion -> (RtrPayloads -> BS.ByteString) -> STM BS.ByteString
cachedPduBinary appState@AppState {..} protocolVersion makeBs = do 
    (Map.lookup protocolVersion <$> readTVar cachedBinaryRtrPdus) >>= \case
        Nothing -> do            
            bs <- makeBs <$> readRtrPayloads appState 
            modifyTVar' cachedBinaryRtrPdus $ Map.insert protocolVersion bs
            pure bs
        Just bs -> pure bs
