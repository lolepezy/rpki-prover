{-# LANGUAGE StrictData #-}

module RPKI.AppState where
    
import           Effectful
import           Control.Concurrent.STM    
import           Control.DeepSeq
import           Control.Lens hiding (filtered)
import           Control.Monad (join, unless)
import           Data.Maybe (fromMaybe)

import qualified Data.ByteString                  as BS

import           Data.Set                         (Set)
import qualified Data.Map.Strict                  as Map
import           GHC.Generics
import           System.Posix.Types
import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.AppTypes
import           RPKI.Logging
import           RPKI.SLURM.SlurmProcessing
import           RPKI.SLURM.Types
import           RPKI.Repository
import           RPKI.Time
import           Data.Hourglass (Seconds(..))
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

        -- Health of the Erik relays, as reported by the fetch workers.
        -- Kept in the root process so that a worker started later already
        -- knows which relays are not worth trying.
        erikRelayHealth :: TVar (Map.Map URI RelayHealth),

        -- Function that re-reads SLURM file(s) after every re-validation.
        -- Pinned to the concrete `AppEffects` stack rather than the polymorphic
        -- `ValidatorIO es => Eff es Slurm`, because a constrained type cannot be
        -- stored in a record field without impredicativity. Its only consumer
        -- (`Workflow.reReadSlurm`) runs it with `runValidatorIO`, which expects
        -- exactly this stack.
        readSlurm :: Maybe (Eff AppEffects Slurm),

        -- Metadata about RTR server
        rtrState    :: TVar (Maybe RtrState),

        -- System metrics 
        system      :: TVar SystemInfo,

        -- Index for searching VRPs by a prefix used
        -- by the validity check
        prefixIndex :: TVar (Maybe PrefixIndex),

        runningWorkers :: TVar (Map.Map CPid WorkerInfo),

        fetcheables :: TVar Fetcheables,

        systemState :: TVar SystemState
        
    } deriving stock (Generic)


mkRtrPayloads :: PerTA Vrps -> Set BGPSecPayload -> RtrPayloads
mkRtrPayloads vrps bgpSec = RtrPayloads { uniqueVrps = uniqVrpsPackedBy cmpPacked4 cmpPacked6 $ allTAs vrps, .. }

-- 
{- | What the root process knows about a relay, folded from worker reports.

   A relay is considered unusable once it has failed and served nothing since,
   and stays that way until 'relayRetryInterval' has passed -- long enough that
   workers stop paying its timeout on every fetch, short enough that a relay
   coming back is picked up quickly.
-}
data RelayHealth = RelayHealth {
        consecutiveFailures :: Int,
        lastFailure         :: Maybe Instant,
        totalServed         :: Int,
        totalFailed         :: Int
    }
    deriving stock (Show, Eq, Generic)

newRelayHealth :: RelayHealth
newRelayHealth = RelayHealth 0 Nothing 0 0

-- | How long a relay stays benched after it stops answering.
relayRetryInterval :: Seconds
relayRetryInterval = Seconds 300

{- | How many workers must independently report a relay as failing before it is
     benched.

     Counting here rather than inside a single worker is the point: a transient
     404 or a slow object produces one report from one worker and is ignored,
     while a relay that is actually down is reported by every worker that tries
     it and crosses this within the first seconds of a fetch round.
-}
relayDeadAfterReports :: Int
relayDeadAfterReports = 3

-- | Fold one worker's reports into the health map.
updateErikRelayHealth :: MonadIO m => AppState -> [ErikRelayReport] -> m ()
updateErikRelayHealth AppState {..} reports = liftIO $ do
    Now now <- thisInstant
    atomically $ modifyTVar' erikRelayHealth $ \health ->
        foldr (fold_ now) health reports
  where
    fold_ now ErikRelayReport {..} =
        Map.alter (Just . bump . fromMaybe newRelayHealth) relay
      where
        bump h@RelayHealth {..} = h {
                -- A relay that served anything at all is working, and that
                -- clears the bench. Workers only report a failure once they
                -- are convinced the relay is dead (see 'deadRelayThreshold'),
                -- so a single report is enough to bench it.
                consecutiveFailures = if served > 0 then 0 else consecutiveFailures + failed,
                lastFailure  = if served > 0 then Nothing else
                                 if failed > 0 then Just now else lastFailure,
                totalServed  = totalServed + served,
                totalFailed  = totalFailed + failed
            }

{- | The relays worth handing to a new fetch worker, best first.

   Relays that have been failing are dropped, unless they have been benched
   long enough to deserve another go. If that would leave nothing, the full
   list is returned -- better to try a bad relay than to not fetch at all.
-}
usableErikRelays :: MonadIO m => AppState -> [URI] -> m [URI]
usableErikRelays AppState {..} configured = liftIO $ do
    Now now <- thisInstant
    health <- readTVarIO erikRelayHealth
    let usable = filter (worthTrying now health) configured
    pure $! if null usable then configured else usable
  where
    worthTrying now health relay =
        case Map.lookup relay health of
            Nothing -> True
            Just RelayHealth {..}
                | consecutiveFailures < relayDeadAfterReports -> True
                | otherwise ->
                    case lastFailure of
                        Nothing -> True
                        Just t  -> not $ closeEnoughMoments (Earlier t) (Later now) relayRetryInterval

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
        runningWorkers <- newTVar mempty
        fetcheables <- newTVar mempty
        systemState <- newTVar $ SystemState DbOperational
        erikRelayHealth <- newTVar mempty
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
        force $ Just $ createPrefixIndex $ vrpsToList $ rtrPayloads ^. #uniqueVrps

getOrCreateWorldVerion :: AppState -> IO WorldVersion
getOrCreateWorldVerion AppState {..} = 
    join $ atomically $ 
        maybe newWorldVersion pure <$> readTVar world

versionToInstant :: WorldVersion -> Instant
versionToInstant = Instant . versionToInt

instantToVersion :: Instant -> WorldVersion
instantToVersion (Instant nanos) = asVersion nanos

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

updateRunningWorkers :: MonadIO m => WorkerMessage -> AppState -> m ()           
updateRunningWorkers message AppState {..} =     
    liftIO $ atomically $ modifyTVar' runningWorkers $ 
        case message of 
            AddWorker wi     -> Map.insert (wi ^. #workerPid) wi
            RemoveWorker pid -> Map.delete pid

updateSystemStatus :: MonadIO m => SystemStatusMessage -> AppState -> m ()           
updateSystemStatus (SystemStatusMessage ss) AppState {..} =     
    -- TODO Do something smarter here
    liftIO $ atomically $ writeTVar systemState ss

waitForStuckDb :: AppState -> STM ()
waitForStuckDb AppState {..} = do
    SystemState {..} <- readTVar systemState
    unless (dbState == DbStuck) retry

        
removeExpiredWorkers :: MonadIO m => AppState -> m [WorkerInfo]
removeExpiredWorkers AppState {..} = liftIO $ do 
    Now now <- thisInstant
    atomically $ do         
        workers <- readTVar runningWorkers        
        let expired = filter (\WorkerInfo {..} -> endOfLife < now) $ Map.elems workers        
        writeTVar runningWorkers $ foldr (\WorkerInfo {..} m -> Map.delete workerPid m) workers expired 
        pure expired  

getRunningWorkers :: MonadIO m => AppState -> m [WorkerInfo]
getRunningWorkers AppState {..} = 
    liftIO $ atomically $ Map.elems <$> readTVar runningWorkers

removeAllRunningWorkers :: MonadIO m => AppState -> m [WorkerInfo]
removeAllRunningWorkers AppState {..} = 
    liftIO $ atomically $ do 
        clients <- Map.elems <$> readTVar runningWorkers
        writeTVar runningWorkers mempty
        pure clients

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
    cached <- readTVar cachedBinaryRtrPdus
    case Map.lookup protocolVersion cached of
        Nothing -> do            
            bs <- makeBs <$> readRtrPayloads appState 
            modifyTVar' cachedBinaryRtrPdus $ Map.insert protocolVersion bs
            pure bs
        Just bs -> pure bs
