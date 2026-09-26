{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The state of a top-down validation run and the bookkeeping on it: which
-- objects are used, what is counted, the payloads, when something expires.
module RPKI.Validation.TopDown.Context where

import           Effectful
import           Control.Concurrent.STM

import           Control.Lens hiding (children)

import           Barbies

import           GHC.Generics

import           Data.Foldable
import           Data.IORef
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.String.Interpolate.IsString
import           Data.Tuple.Strict

import           RPKI.AppContext
import           RPKI.AppState
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Repository
import           RPKI.Resources.Types

import           RPKI.Store.Database    (roTxT, rwTxT)
import qualified RPKI.Store.Database    as DB
import           RPKI.Time
import           RPKI.Util
import           RPKI.Validation.Types
import           RPKI.Validation.TopDown.Shortcuts


data PayloadBuilder = PayloadBuilder {
        vrps     :: IORef [T2 VrpsPerAs ObjectKey],
        spls     :: IORef [SplPayload],        
        aspas    :: IORef [Aspa],
        gbrs     :: IORef [T2 Hash Gbr],
        bgpCerts :: IORef [BGPSecPayload]
    }
    deriving stock (Generic)        

-- Auxiliarry structure used in top-down validation. It has a lot of global variables 
-- but it's lifetime is limited to one top-down validation run.
data TopDownContext = TopDownContext {
        verifiedResources       :: Maybe (VerifiedRS PrefixesAndAsns),
        taName                  :: TaName,
        allTas                  :: AllTasTopDownContext,
        currentPathDepth        :: Int,
        interruptedByLimit      :: TVar Limited,
        payloadBuilder          :: PayloadBuilder,
        overclaimingHappened    :: Bool,
        fetcheables             :: TVar Fetcheables,
        earliestNotValidAfter   :: TVar EarliestToExpire,        
        visitedAkis             :: TVar (Set AKI)
    }
    deriving stock (Generic)

data AllTasTopDownContext = AllTasTopDownContext {
        now                  :: Now,
        worldVersion         :: WorldVersion,
        visitedKeys          :: TVar (Set ObjectKey),        
        publicationPoints    :: PublicationPoints,
        shortcutQueue        :: ClosableQueue MftShortcutOp,
        topDownCounters      :: TopDownCounters IORef,
        -- | Objects published at more than one location, read once for the
        -- whole run. Validating a shortcut needs to know whether its object is
        -- one of these, and asking per object was ~440k queries per round to
        -- find the handful that are (4 of 793516 in a real cache).
        multiLocationKeys    :: Set ObjectKey,
        -- | Validating a CA with all its sub-tree, or a chunk of objects, 
        -- is a task in this pool, for all the TAs together
        workPool             :: WorkPool
    }
    deriving stock (Generic)

data TopDownCounters f = TopDownCounters {
        originalCa   :: f Int,
        shortcutCa   :: f Int,
        originalMft  :: f Int,
        shortcutMft  :: f Int,
        originalCrl  :: f Int,
        shortcutCrl  :: f Int,        
        originalRoa  :: f Int,
        originalSpl  :: f Int,
        originalAspa :: f Int,        
        shortcutRoa  :: f Int,
        shortcutSpl  :: f Int,
        shortcutAspa :: f Int,        
        shortcutTroubled    :: f Int,
        newChildren         :: f Int,
        overlappingChildren :: f Int,
        updateMftMeta       :: f Int,
        updateMftChildren   :: f Int,
        readOriginal :: f Int,
        readParsed   :: f Int,
        repeatedAki  :: f Int
    }
    deriving stock (Generic)
    deriving (FunctorB, TraversableB, ApplicativeB, ConstraintsB)

deriving instance AllBF Show f TopDownCounters => Show (TopDownCounters f)

data Limited = CanProceed | FirstToHitLimit | AlreadyReportedLimit
    deriving stock (Show, Eq, Ord, Generic)


newPayloadBuilder :: IO PayloadBuilder 
newPayloadBuilder = PayloadBuilder <$> 
            newIORef mempty <*>
            newIORef mempty <*>
            newIORef mempty <*>
            newIORef mempty <*>
            newIORef mempty

newTopDownContext :: MonadIO m =>
                    TaName
                    -> AllTasTopDownContext
                    -> m TopDownContext
newTopDownContext taName allTas = 
    liftIO $ do 
        payloadBuilder <- newPayloadBuilder
        atomically $ do
            let verifiedResources = Nothing
                currentPathDepth = 0
                overclaimingHappened = False       
            interruptedByLimit      <- newTVar CanProceed                 
            fetcheables             <- newTVar mempty                 
            earliestNotValidAfter   <- newTVar mempty
            visitedAkis             <- newTVar mempty
            pure $! TopDownContext {..}

newAllTasTopDownContext :: MonadIO m =>
                        WorldVersion
                        -> PublicationPoints 
                        -> ClosableQueue MftShortcutOp
                        -> Set ObjectKey
                        -> WorkPool
                        -> m AllTasTopDownContext
newAllTasTopDownContext worldVersion publicationPoints shortcutQueue multiLocationKeys workPool = liftIO $ do 
    let now = Now $ versionToInstant worldVersion
    topDownCounters <- newTopDownCounters
    atomically $ do        
        visitedKeys    <- newTVar mempty 
        pure $! AllTasTopDownContext {..}

newTopDownCounters :: IO (TopDownCounters IORef)
newTopDownCounters = do 
    originalCa <- newIORef 0
    shortcutCa <- newIORef 0
    originalMft <- newIORef 0
    shortcutMft <- newIORef 0
    originalCrl <- newIORef 0
    shortcutCrl <- newIORef 0
    newChildren <- newIORef 0
    overlappingChildren <- newIORef 0
    updateMftMeta     <- newIORef 0
    updateMftChildren <- newIORef 0    

    shortcutRoa  <- newIORef 0        
    shortcutSpl  <- newIORef 0        
    shortcutAspa <- newIORef 0            
    shortcutTroubled <- newIORef 0        

    originalRoa  <- newIORef 0        
    originalSpl  <- newIORef 0        
    originalAspa <- newIORef 0        

    readOriginal <- newIORef 0   
    readParsed   <- newIORef 0             
    repeatedAki  <- newIORef 0
   
    pure TopDownCounters {..}

verifyLimit :: STM Bool -> TVar Limited -> STM Limited
verifyLimit hitTheLimit limit =
    readTVar limit >>= \case
        CanProceed -> do
            h <- hitTheLimit
            if h then do
                writeTVar limit FirstToHitLimit
                pure FirstToHitLimit
            else
                pure CanProceed
        FirstToHitLimit -> do
            writeTVar limit AlreadyReportedLimit
            pure AlreadyReportedLimit
        AlreadyReportedLimit ->
            pure AlreadyReportedLimit


-- Do whatever is required to notify other subsystems that the object was touched 
-- during top-down validation. It doesn't mean that the object is valid, just that 
-- we read it from the database and looked at it. It will be used to decide when 
-- to GC this object from the cache -- if it's not visited for too long, it is 
-- removed.
markAsUsed :: ValidatorIO es => TopDownContext -> ObjectKey -> Eff es ()
markAsUsed TopDownContext { allTas = AllTasTopDownContext {..} } k = 
    liftIO $ atomically $ modifyTVar' visitedKeys (Set.insert k)

markAsUsedByHash :: ValidatorIO es => 
                    AppContext s -> TopDownContext -> Hash -> Eff es ()
markAsUsedByHash AppContext {..} topDownContext hash = do
    key <- roTxT database $ \tx -> DB.getKeyByHash tx hash
    for_ key $ markAsUsed topDownContext              

bumpCounterBy :: (MonadIO m, Num a) =>
                s -> Getting (IORef a) s (IORef a) -> a -> m ()
bumpCounterBy counters counterLens n = liftIO $     
    atomicModifyIORef' (counters ^. counterLens) $ \c -> (c + n, ())        

rememberNotValidAfter :: MonadIO m => TopDownContext -> Instant -> m ()
rememberNotValidAfter TopDownContext {..} notAfter = 
    liftIO $ atomically $ modifyTVar' earliestNotValidAfter (<> EarliestToExpire notAfter)

rememberCrlNextUpdate :: MonadIO m => TopDownContext -> Validated CrlObject -> m ()
rememberCrlNextUpdate topDownContext (Validated (CrlObject { signCrl = SignCRL {..}})) = liftIO $ 
    rememberNotValidAfter topDownContext nextUpdateTime


-- | Where a valid child comes from: its object validated in full, or its shortcut.
data ChildSource = FromObject | FromShortcut

-- | Count a valid manifest child that is not a CA certificate and keep its
-- payload. It's done here only, the same for a child validated in full and
-- for a child taken from its shortcut.
acceptLeaf :: ValidatorIO es => TopDownContext -> ChildSource -> MftChild -> Eff es ()
acceptLeaf topDownContext source = \case
    RoaChild r _ -> do
        oneMoreRoa
        moreVrps $ Count $ fromIntegral $ length (roaV4 r.roaPayload) + length (roaV6 r.roaPayload)
        count (.originalRoa) (.shortcutRoa)
        keep #vrps $ T2 r.roaPayload r.key
    SplChild s _ -> do
        oneMoreSpl
        count (.originalSpl) (.shortcutSpl)
        keep #spls s.splPayload
    AspaChild a _ -> do
        oneMoreAspa
        count (.originalAspa) (.shortcutAspa)
        keep #aspas a.aspa
    BgpSecChild b _ -> do
        oneMoreBgp
        keep #bgpCerts b.bgpSec
    GbrChild g _ -> do
        oneMoreGbr
        keep #gbrs g.gbr
    -- A CA gives the payloads of its sub-tree
    CaChild {} -> pure ()
    -- A troubled child is validated in full and accepted as what that gives
    TroubledChild _ -> pure ()
  where
    counters = topDownContext.allTas.topDownCounters
    count original shortcut = increment $ case source of
        FromObject   -> original counters
        FromShortcut -> shortcut counters

    -- These lists live until the TA is validated, so what goes into them is 
    -- evaluated first, not a thunk that holds on to the whole shortcut
    keep :: MonadIO m => Getting (IORef [a]) PayloadBuilder (IORef [a]) -> a -> m ()
    keep field !a = liftIO $
        atomicModifyIORef' (topDownContext.payloadBuilder ^. field) $ \as -> (a : as, ())

oneMoreCert, oneMoreRoa, oneMoreMft, oneMoreCrl :: Validator es => Eff es ()

oneMoreGbr, oneMoreAspa, oneMoreBgp, oneMoreSpl :: Validator es => Eff es ()

oneMoreMftShort :: Validator es => Eff es ()

oneMoreCert = updateMetric @ValidationMetric @_ (#validCertNumber %~ (+1))

oneMoreRoa  = updateMetric @ValidationMetric @_ (#validRoaNumber %~ (+1))

oneMoreSpl  = updateMetric @ValidationMetric @_ (#validSplNumber %~ (+1))

oneMoreMft  = updateMetric @ValidationMetric @_ (#validMftNumber %~ (+1))

oneMoreCrl  = updateMetric @ValidationMetric @_ (#validCrlNumber %~ (+1))

oneMoreGbr  = updateMetric @ValidationMetric @_ (#validGbrNumber %~ (+1))

oneMoreAspa = updateMetric @ValidationMetric @_ (#validAspaNumber %~ (+1))

oneMoreBgp  = updateMetric @ValidationMetric @_ (#validBgpNumber %~ (+1))

oneMoreMftShort = updateMetric @ValidationMetric @_ (#mftShortcutNumber %~ (+1))

moreVrps :: Validator es => Count -> Eff es ()
moreVrps n = updateMetric @ValidationMetric @_ (#vrpCounter %~ (+n))


-- | Mark validated objects in the database, i.e.
applyValidationSideEffects :: (MonadIO m) =>
                              AppContext s -> AllTasTopDownContext -> m ()
applyValidationSideEffects
    appContext@AppContext {..}
    AllTasTopDownContext {..} = liftIO $ do        
    (visitedSize, elapsed) <- timedMS $ do
        vks <- readTVarIO visitedKeys            
        rwTxT database $ \tx -> DB.markAsValidated tx vks worldVersion        
        pure $! Set.size vks
    
    liftIO $ reportCounters appContext topDownCounters        
    logDebug logger [i|Marked #{visitedSize} objects as used, took #{elapsed}ms.|]

-- This is to be able to print all counters as Int, not Identity Int
newtype IdenticalShow a = IdenticalShow a
    deriving stock (Generic)
    deriving (Functor)

instance Show a => Show (IdenticalShow a) where
    show (IdenticalShow a) = show a

reportCounters :: AppContext s -> TopDownCounters IORef -> IO ()
reportCounters AppContext {..} counters = do
    c <- btraverse (fmap IdenticalShow . readIORef) counters
    logDebug logger $ fmtGen c
