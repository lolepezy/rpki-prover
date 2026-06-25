{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module RPKI.Validation.Types where

import           Control.Concurrent.STM
import           Data.Aeson.Types
import qualified Data.Map.Strict             as Map
import           Data.Set                    (Set)
import           Data.Text                   (Text)
import           Data.Vector                 (Vector)
import           Data.Tuple.Strict
import           Data.IORef
import           GHC.Generics

import           Barbies

import           Data.Proxy
import           Data.Swagger hiding (url)

import           RPKI.Orphans.Swagger
import           RPKI.AppTypes
import           RPKI.Time
import           RPKI.Domain
import           RPKI.Parallel
import           RPKI.Repository
import           RPKI.Resources.Types
import           RPKI.Store.Base.Serialisation
import           RPKI.Store.Base.Storable


-- It is to simplify the definition of Payload handlers        
data Payload = VrpsP (Vector Vrp)
            | AspaP Aspa
            | BgpSecP BGPSecPayload
            | SplP SplPayload
            | GbrP (T2 Hash Gbr)
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data Change a = Added a | Deleted a
    deriving (Show, Eq, Ord, Generic)  
    deriving anyclass (TheBinary)


data MftChild = CaChild CaShortcut Serial
              | RoaChild RoaShortcut Serial
              | SplChild SplShortcut Serial
              | AspaChild AspaShortcut Serial
              | BgpSecChild BgpSecShortcut Serial
              | GbrChild GbrShortcut Serial
              -- Invalid, revoked or an object of unknown type
              | TroubledChild ObjectKey
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)


data MftEntry = MftEntry {        
        fileName :: Text,
        child    :: MftChild
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)


data CrlShortcut = CrlShortcut {
        key            :: {-# UNPACK #-} ObjectKey,
        notValidBefore :: Instant,
        notValidAfter  :: Instant        
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data MftShortcut = MftShortcut { 
        key            :: {-# UNPACK #-} ObjectKey,
        nonCrlEntries  :: Map.Map ObjectKey MftEntry,
        notValidBefore :: Instant,
        notValidAfter  :: Instant,        
        serial         :: {-# UNPACK #-} Serial,
        manifestNumber :: {-# UNPACK #-} Serial,
        crlShortcut    :: CrlShortcut        
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data CaShortcut = CaShortcut { 
        key            :: {-# UNPACK #-} ObjectKey,
        ski            :: SKI,
        ppas           :: PublicationPointAccess,
        notValidBefore :: Instant,
        notValidAfter  :: Instant,
        resources      :: AllResources
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data Ca = CaShort CaShortcut
        | CaFull (Located CaCerObject)
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)


data RoaShortcut = RoaShortcut {
        key            :: {-# UNPACK #-} ObjectKey,        
        vrps           :: Vector Vrp,
        notValidBefore :: Instant,
        notValidAfter  :: Instant,
        resources      :: AllResources
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data SplShortcut = SplShortcut {
        key            :: {-# UNPACK #-} ObjectKey,        
        splPayload     :: SplPayload,
        notValidBefore :: Instant,
        notValidAfter  :: Instant,
        resources      :: AllResources
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data AspaShortcut = AspaShortcut {
        key            :: {-# UNPACK #-} ObjectKey,
        aspa           :: Aspa,
        notValidBefore :: Instant,
        notValidAfter  :: Instant,
        resources      :: AllResources
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data BgpSecShortcut = BgpSecShortcut {
        key            :: {-# UNPACK #-} ObjectKey,
        bgpSec         :: BGPSecPayload,
        notValidBefore :: Instant,
        notValidAfter  :: Instant,
        resources      :: AllResources
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data GbrShortcut = GbrShortcut {
        key            :: {-# UNPACK #-} ObjectKey,    
        gbr            :: T2 Hash Gbr,
        notValidBefore :: Instant,
        notValidAfter  :: Instant,
        resources      :: AllResources
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)



data PayloadBuilder = PayloadBuilder {
        vrps     :: IORef [T2 (Vector Vrp) ObjectKey],        
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
        earliestNotValidAfter   :: TVar EarliestToExpire
    }
    deriving stock (Generic)


data AllTasTopDownContext = AllTasTopDownContext {
        now                  :: Now,
        worldVersion         :: WorldVersion,
        visitedKeys          :: TVar (Set ObjectKey),        
        publicationPoints    :: PublicationPoints,
        shortcutQueue        :: ClosableQueue MftShortcutOp,
        topDownCounters      :: TopDownCounters IORef        
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
        readParsed   :: f Int
    }
    deriving stock (Generic)
    deriving (FunctorB, TraversableB, ApplicativeB, ConstraintsB)

deriving instance AllBF Show f TopDownCounters => Show (TopDownCounters f)

data Limited = CanProceed | FirstToHitLimit | AlreadyReportedLimit
    deriving stock (Show, Eq, Ord, Generic)

-- This is to be able to print all counters as Int, not Identity Int
newtype IdenticalShow a = IdenticalShow a
    deriving stock (Generic)
    deriving (Functor)

instance Show a => Show (IdenticalShow a) where
    show (IdenticalShow a) = show a


-- Some DTOs for storing MFT shortcuts
data MftShortcutMeta = MftShortcutMeta {
        key            :: ObjectKey,        
        notValidBefore :: Instant,
        notValidAfter  :: Instant,        
        serial         :: Serial,
        manifestNumber :: Serial,
        crlShortcut    :: CrlShortcut
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

newtype MftShortcutChildren = MftShortcutChildren {
        nonCrlEntries :: Map.Map ObjectKey MftEntry
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data MftShortcutOp = UpdateMftShortcut AKI (Verbatim (Compressed MftShortcutMeta))
                   | UpdateMftShortcutChildren AKI (Verbatim (Compressed MftShortcutChildren))            
                   | DeleteMftShortcut AKI            


instance {-# OVERLAPPING #-} WithValidityPeriod CaShortcut where
    getValidityPeriod CaShortcut {..} = (notValidBefore, notValidAfter)

instance {-# OVERLAPPING #-} WithValidityPeriod MftShortcut where
    getValidityPeriod MftShortcut {..} = (notValidBefore, notValidAfter)

instance {-# OVERLAPPING #-} WithValidityPeriod CrlShortcut where
    getValidityPeriod CrlShortcut {..} = (notValidBefore, notValidAfter)

instance {-# OVERLAPPING #-} WithValidityPeriod RoaShortcut where
    getValidityPeriod RoaShortcut {..} = (notValidBefore, notValidAfter)

instance {-# OVERLAPPING #-} WithValidityPeriod SplShortcut where
    getValidityPeriod SplShortcut {..} = (notValidBefore, notValidAfter)

instance {-# OVERLAPPING #-} WithValidityPeriod AspaShortcut where
    getValidityPeriod AspaShortcut {..} = (notValidBefore, notValidAfter)

instance {-# OVERLAPPING #-} WithValidityPeriod BgpSecShortcut where
    getValidityPeriod BgpSecShortcut {..} = (notValidBefore, notValidAfter)

instance {-# OVERLAPPING #-} WithValidityPeriod GbrShortcut where
    getValidityPeriod GbrShortcut {..} = (notValidBefore, notValidAfter)

instance ToJSON CrlShortcut
instance ToJSON GbrShortcut
instance ToJSON BgpSecShortcut
instance ToJSON AspaShortcut
instance ToJSON RoaShortcut
instance ToJSON SplShortcut

instance ToSchema CrlShortcut

instance ToSchema MftChild where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)


getMftChildSerial :: MftChild -> Maybe Serial     
getMftChildSerial = \case 
    CaChild _ serial     -> Just serial 
    RoaChild _ serial    -> Just serial 
    SplChild _ serial    -> Just serial 
    AspaChild _ serial   -> Just serial 
    BgpSecChild _ serial -> Just serial 
    GbrChild _ serial    -> Just serial 
    _                    -> Nothing
              
getResources :: Ca -> AllResources
getResources = \case 
    CaShort CaShortcut {..} -> resources
    CaFull (getRawCert -> RawResourceCertificate {..}) -> resources
