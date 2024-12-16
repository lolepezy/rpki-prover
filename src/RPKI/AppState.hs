{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE StrictData         #-}

module RPKI.AppState where
    
import           Control.Lens hiding (filtered)
import           Control.Monad (join)
import           Control.Concurrent.STM

import qualified Data.ByteString                  as BS

import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           GHC.Generics
import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.AppTypes
import           RPKI.SLURM.SlurmProcessing
import           RPKI.SLURM.Types
import           RPKI.Time
import           RPKI.Metrics.System
import           RPKI.RTR.Protocol
import           RPKI.RTR.Types
import           Control.Monad.IO.Class


data AppState = AppState {
        world     :: TVar (Maybe WorldVersion),
        validated :: TVar RtrPayloads,
        filtered  :: TVar RtrPayloads,
        cachedBinaryPdus :: TVar (Map.Map ProtocolVersion BS.ByteString),
        readSlurm :: Maybe (ValidatorT IO Slurm),
        rtrState  :: TVar (Maybe RtrState),
        system    :: TVar SystemInfo
    } deriving stock (Generic)


uniqVrps :: Vrps -> Set AscOrderedVrp 
uniqVrps = mconcat . Prelude.map (Set.map AscOrderedVrp) . allVrps

mkRtrPayloads :: Vrps -> Set BGPSecPayload -> RtrPayloads
mkRtrPayloads vrps bgpSec = RtrPayloads { uniqueVrps = uniqVrps vrps, .. }

-- 
newAppState :: IO AppState
newAppState = do        
    Now now <- thisInstant
    atomically $ AppState <$> 
                    newTVar Nothing <*>
                    newTVar mempty <*>
                    newTVar mempty <*>
                    newTVar mempty <*>
                    pure Nothing <*>                    
                    newTVar Nothing <*>
                    newTVar (newSystemInfo now)

-- World versions are nanosecond-timestamps
newWorldVersion :: IO WorldVersion
newWorldVersion = instantToVersion . unNow <$> thisInstant        

completeVersion :: AppState -> WorldVersion -> RtrPayloads -> Maybe Slurm -> STM RtrPayloads
completeVersion AppState {..} worldVersion rtrPayloads slurm = do 
    writeTVar world $ Just worldVersion
    writeTVar validated rtrPayloads
    let slurmed = maybe rtrPayloads (filterWithSLURM rtrPayloads) slurm
    writeTVar filtered slurmed
    -- invalidate serialised PDU cache with every new version
    writeTVar cachedBinaryPdus mempty
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
cachedPduBinary :: AppState -> ProtocolVersion -> (RtrPayloads -> BS.ByteString) -> STM BS.ByteString
cachedPduBinary appState@AppState {..} protocolVersion f = do 
    z <- readTVar cachedBinaryPdus
    case Map.lookup protocolVersion z of 
        Nothing -> do            
            bs <- f <$> readRtrPayloads appState 
            modifyTVar' cachedBinaryPdus $ Map.insert protocolVersion bs
            pure bs
        Just bs -> pure bs
