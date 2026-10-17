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
        -- the key was validated under, see `claimKey`
        keyStates               :: TVar (Map AKI KeyState),
        -- | The manifest of the CA is validated not for the first time, under 
        -- another certificate with its key
        anotherPassOfKey        :: Bool,
        -- | The validation of the manifest of the CA is the one in this run that 
        -- uses and writes its shortcut, see `claimShortcut`
        ownsShortcut            :: Bool,
        -- | Where the first validation of the manifest of the CA keeps the 
        -- children that are not valid only because of their resources
        recordingChildren       :: Maybe (TVar KeyChildren),
        -- | Only these children of the manifest are validated, see `KeyChildren`
        onlyChildren            :: Maybe (Set ObjectKey),
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

-- | What happened to a key in a run: the certificates with it, what its 
-- manifest was validated under (see `validationContextHash`), and how far
-- its validation is.
data KeyState = KeyState {
        certificates  :: Set ObjectKey,
        contextHashes :: Set Hash,
        -- | Validations of children of the key again, under other certificates
        rechecks      :: Int,
        stage         :: KeyStage
    }
    deriving stock (Generic)

data KeyStage =
      -- | The manifest of the key is not validated yet, or it failed
      NotValidatedYet
      -- | It's being validated under the first certificate with the key, the
      -- certificates that come meanwhile wait for it
    | ValidatingFirst (TVar KeyChildren) [KeyArrival]
      -- | It's validated under the first certificate with the key
    | ValidatedFirst (TVar KeyChildren)
    deriving stock (Generic)

{- | The children of a key that can be valid under another certificate with
   the key, when they are not valid under the first one.

   Validity of a child under a certificate depends only on the resources of
   the certificate, the rest of it (the signature, revocation, the times) is 
   the same under any certificate with the key. So under another certificate 
   it's the children that are not valid only because of their resources, and
   the CA children with resources that depend on the certificate: the ones 
   that inherit them, and every one with the reconsidered algorithm.
-}
data KeyChildren = KeyChildren {
        -- | The ones that are not valid only because of their resources, with 
        -- what they need from a certificate to be valid, see `objectNeeds`
        pending      :: Map ObjectKey PrefixesAndAsns,
        -- | What all the pending ones need, when it's known
        pendingNeeds :: Maybe PrefixesAndAsns,
        -- | The CA children that depend on the certificate, when it's known, 
        -- see `dependentCaChildren`
        dependent    :: Maybe DependentChildren
    }
    deriving stock (Generic)

data DependentChildren = DependentChildren {
        children     :: [DependentChild],
        -- | What of the resources of a certificate with the key can make a 
        -- difference to any of them
        ownResources :: PrefixesAndAsns
    }
    deriving stock (Generic)

data DependentChild = DependentChild {
        key         :: ObjectKey,
        certificate :: WellStructuredCaCert
    }
    deriving stock (Generic)

-- | A certificate with a key that comes to the children of the key
data KeyArrival = KeyArrival {
        context     :: TopDownContext,
        ca          :: Ca,
        ski         :: SKI,
        contextHash :: Hash,
        -- | The same certificate again
        sameAgain   :: Bool,
        scopes      :: Scopes
    }
    deriving stock (Generic)

data KeyClaim =
      ValidateFirst (TVar KeyChildren)
      -- | It waits for the first validation, which takes it after
    | WaitForFirst
    | ValidateAgain (TVar KeyChildren)
      -- | It was validated under the same already
    | ValidatedAlready


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
            let anotherPassOfKey  = False
                ownsShortcut      = True
                recordingChildren = Nothing
                onlyChildren      = Nothing
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


{- | What a certificate with a key does about the children of the key.

   Nothing stops several certificates from having one key: e.g. issued by
   different parents with different resources, or a copy of a certificate
   issued by anyone, with their own resources. The children of the key are
   valid under any of them that has their resources.

   So the manifest of the key is validated under the first certificate with
   it, and under every other one, only the children that can be valid under
   it are validated again (see `KeyChildren`), each one at most once. A 
   certificate with resources that none of them need costs nothing then.
   Certificates that come while the first validation goes on wait for it,
   it takes them after.

   The same certificate again, or one with the same resources and location
   of the manifest (see `validationContextHash`), changes nothing.
-}
claimKey :: TVar (Map AKI KeyState) -> AKI -> KeyArrival -> STM KeyClaim
claimKey keyStates aki arrival = do
    state@KeyState {..} <- keyStateOf keyStates aki
    let update = modifyTVar' keyStates . Map.insert aki
    if Set.member arrival.contextHash contextHashes
        then pure ValidatedAlready
        else case stage of
            NotValidatedYet -> do
                children <- newTVar $ KeyChildren mempty Nothing Nothing
                update $! state { contextHashes = Set.insert arrival.contextHash contextHashes,
                                  stage    = ValidatingFirst children [] }
                pure $ ValidateFirst children
            ValidatingFirst children waiting -> do
                update $! state { stage = ValidatingFirst children (arrival : waiting) }
                pure WaitForFirst
            ValidatedFirst children -> do
                update $! state { contextHashes = Set.insert arrival.contextHash contextHashes }
                pure $ ValidateAgain children

-- | The first validation of the manifest of the key is done, give the 
-- certificates that waited for it. If it failed, the next one is the first.
finishFirstValidation :: TVar (Map AKI KeyState) -> AKI -> Bool -> STM [KeyArrival]
finishFirstValidation keyStates aki succeeded = do
    state@KeyState {..} <- keyStateOf keyStates aki
    case stage of
        ValidatingFirst children waiting -> do
            modifyTVar' keyStates $ Map.insert aki $! state {
                    stage = if succeeded then ValidatedFirst children else NotValidatedYet
                }
            pure $ reverse waiting
        _ -> pure []

-- | Whether children of the key can be validated once more, under another 
-- certificate with the key that has resources they need. Only so many times.
claimRecheck :: TVar (Map AKI KeyState) -> AKI -> Int -> STM (Maybe Int)
claimRecheck keyStates aki maxRechecks = do
    state@KeyState {..} <- keyStateOf keyStates aki
    if rechecks >= maxRechecks
        then pure $ Just rechecks
        else do
            modifyTVar' keyStates $ Map.insert aki $! state { rechecks = rechecks + 1 }
            pure Nothing

-- | A child that is not valid only because of its resources, under the first
-- certificate of the key that this is the validation of, see `KeyChildren`
recordPendingChild :: MonadIO m => TopDownContext -> ObjectKey -> PrefixesAndAsns -> m ()
recordPendingChild TopDownContext { recordingChildren } childKey needs = 
    for_ recordingChildren $ \children -> 
        liftIO $ atomically $ modifyTVar' children $ \c -> 
            c { pending = Map.insert childKey needs c.pending, pendingNeeds = Nothing }

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
    fromMaybe (KeyState mempty mempty 0 NotValidatedYet) . Map.lookup aki <$> readTVar keyStates

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
        -- These are about the manifest of the parent
        & #recordingChildren    .~ Nothing
        & #onlyChildren         .~ Nothing


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
