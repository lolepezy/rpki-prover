{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The state of a top-down validation run and the bookkeeping on it: which
-- objects are used, what is counted, the payloads, when something expires.
module RPKI.Validation.TopDown.Context where

import           Effectful
import           Control.Concurrent.STM
import           Control.Exception                (evaluate)
import           Control.Monad                    (unless)

import           Control.Lens hiding (children)

import           Barbies

import           GHC.Generics

import           Data.Foldable
import           Data.IORef
import           Data.Maybe                       (fromMaybe, isJust)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Map.Monoidal.Strict         (MonoidalMap)
import qualified Data.Map.Monoidal.Strict         as MonoidalMap
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.Text                        (Text)
import           Data.String.Interpolate.IsString
import           Data.Tuple.Strict
import           Data.X509.Validation             (SignatureVerification)

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
import           RPKI.Validation.Common           (createVerifiedResources)
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
        -- | See `verifiedResourcesHash`
        resourcesHash           :: Hash,
        taName                  :: TaName,
        allTas                  :: AllTasTopDownContext,
        currentPathDepth        :: Int,
        interruptedByLimit      :: TVar Limited,
        treeSize                :: TVar Int,
        payloadBuilder          :: PayloadBuilder,
        overclaimingHappened    :: Bool,
        fetcheables             :: TVar Fetcheables,
        earliestNotValidAfter   :: TVar EarliestValidityChange,        
        -- | The certificates of every key so far, and what the manifest of 
        -- the key was validated under, see `claimKeyPass`
        keyStates               :: TVar (Map AKI KeyState),
        -- | The manifest of the CA is validated not for the first time, under 
        -- another certificate with its key
        anotherPassOfKey        :: Bool,
        -- | The validation of the manifest of the CA is the one in this run that 
        -- uses and writes its shortcut, see `claimShortcut`
        ownsShortcut            :: Bool,
        -- | RRDP repositories to fetch from a snapshot next time, with why: 
        -- the cache doesn't have what the manifests of a CA there say, 
        -- and none of them could be used
        snapshotCandidates      :: TVar (MonoidalMap RrdpURL (Set Text))
    }
    deriving stock (Generic)

data AllTasTopDownContext = AllTasTopDownContext {
        now               :: Now,
        worldVersion      :: WorldVersion,
        visitedKeys       :: TVar (Set ObjectKey),        
        -- | The keys whose manifest shortcut is used in this run
        shortcutOwners    :: TVar (Set AKI),
        -- | Signatures of objects by the key of their CA, see `knownSignature`
        knownSignatures   :: TVar (Map (ObjectKey, SKI) SignatureVerification),
        -- | The keys that had more than one valid CA certificate the last time,
        -- see `claimKeyPass`
        keysSharedBefore  :: Set SKI,
        -- | What the children of a key need from a certificate of the key,
        -- for the keys it's needed for, see `keyNeeds`
        knownKeyNeeds     :: TVar (Map AKI PrefixesAndAsns),
        publicationPoints :: PublicationPoints,
        shortcutQueue     :: ClosableQueue MftShortcutOp,
        topDownCounters   :: TopDownCounters IORef,
        multiLocationKeys :: Set ObjectKey,
        workPool          :: WorkPool
    }
    deriving stock (Generic)

data TopDownCounters f = TopDownCounters {
        originalCa   :: f Int,
        shortcutCa   :: f Int,
        originalMft  :: f Int,
        shortcutMft  :: f Int,
        -- manifests with the AKI of a CA that the CA did not sign
        foreignMft   :: f Int,
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
        repeatedAki  :: f Int,
        -- | Validations of a manifest under one more certificate with its key
        anotherKeyPass :: f Int,
        -- | Certificates with a key that other certificates have, under which
        -- the manifest of the key is not validated
        skippedKeyPass :: f Int,
        -- | Signatures known from validating them under another certificate
        rememberedSignature :: f Int
    }
    deriving stock (Generic)
    deriving (FunctorB, TraversableB, ApplicativeB, ConstraintsB)

deriving instance AllBF Show f TopDownCounters => Show (TopDownCounters f)

data Limited = CanProceed | FirstToHitLimit | AlreadyReportedLimit
    deriving stock (Show, Eq, Ord, Generic)

-- | The certificates with one key, and what the manifest of the key was 
-- validated under (see `validationContextHash`), with the smallest depth 
-- of a certificate it was validated under with it.
data KeyState = KeyState {
        certificates :: Set ObjectKey,
        passes       :: Map Hash Int,
        -- | Validations of the manifest so far, also the ones under the
        -- same as before at a smaller depth
        passCount    :: Int
    }
    deriving stock (Generic)

data PassOfKey = FirstPassOfKey | AnotherPassOfKey
    deriving stock (Show, Eq, Generic)

-- | What `claimKeyPass` is told about a certificate
data KeyPassCheck =
      -- | Nothing is known about it
      NotChecked
      -- | It has what the key was validated under last time, so it's
      -- the one to validate the manifest of the key under first
    | AsBefore
      -- | It has resources that the children of the key need
    | CoversChildren
    deriving stock (Show, Eq, Generic)

-- | Whether the manifest of a key is validated under a certificate with it
data KeyPass =
      ValidateUnder PassOfKey
      -- | It was validated under the same at the same depth or a smaller one
    | ValidatedAlready
      -- | It's up to what the children of the key need, see `KeyPassCheck`
    | ChildrenDecide
      -- | It was validated under that many certificates already
    | TooManyPasses Int
    deriving stock (Show, Eq, Generic)


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
                resourcesHash = noResourcesHash
                currentPathDepth = 0
                overclaimingHappened = False       
            interruptedByLimit      <- newTVar CanProceed                 
            treeSize                <- newTVar 0
            fetcheables             <- newTVar mempty                 
            earliestNotValidAfter   <- newTVar mempty
            keyStates               <- newTVar mempty
            snapshotCandidates      <- newTVar mempty
            let anotherPassOfKey = False
                ownsShortcut     = True
            pure $! TopDownContext {..}

newAllTasTopDownContext :: MonadIO m =>
                        WorldVersion
                        -> PublicationPoints 
                        -> ClosableQueue MftShortcutOp
                        -> Set ObjectKey
                        -> Set SKI
                        -> WorkPool
                        -> m AllTasTopDownContext
newAllTasTopDownContext worldVersion publicationPoints shortcutQueue multiLocationKeys keysSharedBefore workPool = liftIO $ do
    let now = Now $ versionToInstant worldVersion
    topDownCounters <- newTopDownCounters
    atomically $ do        
        visitedKeys    <- newTVar mempty 
        shortcutOwners <- newTVar mempty
        knownSignatures <- newTVar mempty
        knownKeyNeeds   <- newTVar mempty
        pure $! AllTasTopDownContext {..}

newTopDownCounters :: IO (TopDownCounters IORef)
newTopDownCounters = do 
    originalCa <- newIORef 0
    shortcutCa <- newIORef 0
    originalMft <- newIORef 0
    shortcutMft <- newIORef 0
    foreignMft <- newIORef 0
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
    anotherKeyPass <- newIORef 0
    skippedKeyPass <- newIORef 0
    rememberedSignature <- newIORef 0
   
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


{- | Whether the manifest of a key is to be validated under a certificate with
   the key, and remember it if it is.

   Nothing stops several certificates from having one key: e.g. issued by
   different parents with different resources, or a copy of a certificate
   issued by anyone, with their own resources. The children of the key are
   valid under any of them that has their resources. But under a copy with
   other resources than the children have, there's nothing to validate them
   for, and the copies can be many.

   So the manifest is validated under a certificate with the key only if its
   resources cover something that the children need, the caller tells that
   (`CoversChildren`), and under so many of them at most. Under one with the
   same resources and location of the manifest as before (see
   `validationContextHash`) only at a smaller depth (for the limit of it), so
   duplicates of a certificate, the same certificate on another path, or a
   cycle, don't take passes of their own.

   It's not checked for the first certificate of the key in the run if it has
   what the key was validated under the last time (`AsBefore`), so that it
   costs nothing for a key with one certificate, every key in the RPKI as it
   is. Unless it had more than one the last time: then the first one may be
   the one that was validated under first because it came first, a copy.
-}
claimKeyPass :: TVar (Map AKI KeyState) -> AKI -> Hash -> Int -> KeyPassCheck -> Int -> STM KeyPass
claimKeyPass keyStates aki contextHash depth checked maxPasses = do
    state@KeyState {..} <- keyStateOf keyStates aki
    let claim = do
            modifyTVar' keyStates $ Map.insert aki $! state {
                    passes    = Map.insertWith min contextHash depth passes,
                    passCount = passCount + 1
                }
            pure $ ValidateUnder $ if Map.null passes then FirstPassOfKey else AnotherPassOfKey
    if | maybe False (<= depth) (Map.lookup contextHash passes) -> pure ValidatedAlready
       | passCount >= maxPasses -> pure $ TooManyPasses passCount
       | otherwise ->
            case checked of
                CoversChildren             -> claim
                AsBefore | Map.null passes -> claim
                _                          -> pure ChildrenDecide

-- | Remember a certificate with the key. If it's a new one, give the ones before it.
rememberCertificateOfKey :: TVar (Map AKI KeyState) -> AKI -> Maybe ObjectKey -> STM (Maybe [ObjectKey])
rememberCertificateOfKey keyStates aki certificate = do
    state@KeyState {..} <- keyStateOf keyStates aki
    if maybe True (`Set.notMember` certificates) certificate
        then do
            for_ certificate $ \c ->
                modifyTVar' keyStates $ Map.insert aki $! state { certificates = Set.insert c certificates }
            pure $ Just $ Set.toList certificates
        else
            pure Nothing

keyStateOf :: TVar (Map AKI KeyState) -> AKI -> STM KeyState
keyStateOf keyStates aki =
    fromMaybe (KeyState mempty mempty 0) . Map.lookup aki <$> readTVar keyStates

-- | Whether the validation of the manifest of a key is the one in this run
-- that uses and writes its shortcut: the first one. The others don't use it,
-- they can be under certificates with the key that have other resources, and
-- what they validate goes to the shortcut of the key otherwise (see 
-- `validationContextHash`).
claimShortcut :: TVar (Set AKI) -> AKI -> STM Bool
claimShortcut shortcutOwners aki = do
    owners <- readTVar shortcutOwners
    if Set.member aki owners
        then pure False
        else do
            writeTVar shortcutOwners $! Set.insert aki owners
            pure True


-- | The signature of an object by the key of its CA. It doesn't depend on which 
-- certificate with the key the object is validated under, so under one more of 
-- them (`anotherPassOfKey`) it's remembered for the run and checked only once.
-- Otherwise it's what is given, checked when it's needed. 
knownSignature :: MonadIO m => 
                  TopDownContext -> ObjectKey -> SKI -> SignatureVerification -> m SignatureVerification
knownSignature TopDownContext { allTas = AllTasTopDownContext {..}, anotherPassOfKey } key ski signature
    | anotherPassOfKey = liftIO $ 
        Map.lookup (key, ski) <$> readTVarIO knownSignatures >>= \case
            Just known -> do
                increment topDownCounters.rememberedSignature
                pure known
            Nothing    -> do
                checked <- evaluate signature
                atomically $ modifyTVar' knownSignatures $ Map.insert (key, ski) checked
                pure checked
    | otherwise = pure signature


-- | The context of validating a TA with its certificate
taContext :: WithResources c => TopDownContext -> c -> TopDownContext
taContext topDownContext taCert = topDownContext 
    & #verifiedResources ?~ createVerifiedResources taCert
    & #resourcesHash     .~ taResourcesHash (getResources taCert)

-- | The context of validating a child CA with the resources of its certificate
-- and their hash, verified the way `validateChildParentResources` does it
childCaContext :: TopDownContext 
               -> AllResources 
               -> Hash
               -> (VerifiedRS PrefixesAndAsns, Maybe (Overclaiming PrefixesAndAsns)) 
               -> TopDownContext
childCaContext topDownContext resources ownHash verified@(childVerifiedResources, overclaiming) = 
    topDownContext 
        & #verifiedResources    ?~ childVerifiedResources
        & #resourcesHash        .~ verifiedResourcesHash resources ownHash topDownContext.resourcesHash verified
        & #currentPathDepth     %~ (+ 1)
        & #overclaimingHappened .~ isJust overclaiming


-- Do whatever is required to notify other subsystems that the object was touched 
-- during top-down validation. It doesn't mean that the object is valid, just that 
-- we read it from the database and looked at it. It will be used to decide when 
-- to GC this object from the cache -- if it's not visited for too long, it is 
-- removed.
markAsUsed :: ValidatorIO es => TopDownContext -> ObjectKey -> Eff es ()
markAsUsed TopDownContext { allTas = AllTasTopDownContext {..}, treeSize = taTreeSize } k = 
    liftIO $ atomically $ do
        visited <- readTVar visitedKeys
        unless (Set.member k visited) $ do
            writeTVar visitedKeys $! Set.insert k visited
            modifyTVar' taTreeSize (+ 1)

-- | Same as `markAsUsed`, also gives the key of the object
markAsUsedByHash :: ValidatorIO es => 
                    AppContext s -> TopDownContext -> Hash -> Eff es (Maybe ObjectKey)
markAsUsedByHash AppContext {..} topDownContext hash = do
    key <- roTxT database $ \tx -> DB.getKeyByHash tx hash
    for_ key $ markAsUsed topDownContext              
    pure key

bumpCounterBy :: (MonadIO m, Num a) =>
                s -> Getting (IORef a) s (IORef a) -> a -> m ()
bumpCounterBy counters counterLens n = liftIO $     
    atomicModifyIORef' (counters ^. counterLens) $ \c -> (c + n, ())        

rememberNotValidAfter :: MonadIO m => TopDownContext -> Instant -> m ()
rememberNotValidAfter TopDownContext {..} notAfter = 
    liftIO $ atomically $ modifyTVar' earliestNotValidAfter (<> EarliestValidityChange notAfter)

-- | The repository is to be fetched from a snapshot next time.
rememberSnapshotCandidate :: MonadIO m => TopDownContext -> RrdpURL -> Set Text -> m ()
rememberSnapshotCandidate TopDownContext {..} rrdpUrl reasons =
    liftIO $ atomically $ modifyTVar' snapshotCandidates (<> MonoidalMap.singleton rrdpUrl reasons)

rememberCrlNextUpdate :: MonadIO m => TopDownContext -> Validated CrlObject -> m ()
rememberCrlNextUpdate topDownContext (Validated (CrlObject { signCrl = SignCRL {..}})) = liftIO $ 
    rememberNotValidAfter topDownContext nextUpdateTime


-- | Where a valid child comes from: its object validated in full, or its shortcut.
data ChildSource = FromObject | FromShortcut

-- | Count a valid manifest child that is not a CA certificate and keep its
-- payload. It's done here only, the same for a child validated in full and
-- for a child taken from its shortcut.
--
-- When it expires is a reason to revalidate. For a child taken from its 
-- shortcut it's remembered where the shortcut is checked (`validateShortcut`),
-- for a child validated in full it's remembered here.
acceptLeaf :: ValidatorIO es => TopDownContext -> ChildSource -> MftChild -> Eff es ()
acceptLeaf topDownContext source child = do
    case source of
        FromObject   -> for_ (childValidityPeriod child) $ \period ->
                            rememberNotValidAfter topDownContext period.notAfter
        FromShortcut -> pure ()
    case child of
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
