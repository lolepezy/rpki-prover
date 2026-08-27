{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module RPKI.Validation.Types where

import           Data.Aeson.Types
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import           Data.Tuple.Strict
import           GHC.Generics

import           Data.Proxy
import           Data.Swagger hiding (url)

import           RPKI.Orphans.Swagger ()
import           RPKI.Time
import           RPKI.Domain
import           RPKI.Repository
import           RPKI.Resources.Types
import           RPKI.Store.Base.Serialisation


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
        notBefore :: {-# UNPACK #-} Instant,
        notAfter  :: {-# UNPACK #-} Instant        
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data MftShortcut = MftShortcut { 
        key            :: {-# UNPACK #-} ObjectKey,
        nonCrlEntries  :: Map.Map ObjectKey MftEntry,
        notBefore :: {-# UNPACK #-} Instant,
        notAfter  :: {-# UNPACK #-} Instant,        
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
        notBefore :: {-# UNPACK #-} Instant,
        notAfter  :: {-# UNPACK #-} Instant,
        resources      :: AllResources
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data Ca = CaShort CaShortcut
    | CaFull (Located WellStructuredCaCert)
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)


data RoaShortcut = RoaShortcut {
        key        :: {-# UNPACK #-} ObjectKey,
        roaPayload :: VrpsPerAs,
        notBefore  :: {-# UNPACK #-} Instant,
        notAfter   :: {-# UNPACK #-} Instant,
        resources  :: AllResources
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data SplShortcut = SplShortcut {
        key            :: {-# UNPACK #-} ObjectKey,        
        splPayload     :: SplPayload,
        notBefore :: {-# UNPACK #-} Instant,
        notAfter  :: {-# UNPACK #-} Instant,
        resources      :: AllResources
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data AspaShortcut = AspaShortcut {
        key            :: {-# UNPACK #-} ObjectKey,
        aspa           :: Aspa,
        notBefore :: {-# UNPACK #-} Instant,
        notAfter  :: {-# UNPACK #-} Instant,
        resources      :: AllResources
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data BgpSecShortcut = BgpSecShortcut {
        key            :: {-# UNPACK #-} ObjectKey,
        bgpSec         :: BGPSecPayload,
        notBefore :: {-# UNPACK #-} Instant,
        notAfter  :: {-# UNPACK #-} Instant,
        resources      :: AllResources
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data GbrShortcut = GbrShortcut {
        key            :: {-# UNPACK #-} ObjectKey,    
        gbr            :: T2 Hash Gbr,
        notBefore :: {-# UNPACK #-} Instant,
        notAfter  :: {-# UNPACK #-} Instant,
        resources      :: AllResources
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)


instance {-# OVERLAPPING #-} WithValidityPeriod CaShortcut where
    getValidityPeriod CaShortcut {..} = ValidityPeriod notBefore notAfter

instance {-# OVERLAPPING #-} WithValidityPeriod MftShortcut where
    getValidityPeriod MftShortcut {..} = ValidityPeriod notBefore notAfter

instance {-# OVERLAPPING #-} WithValidityPeriod CrlShortcut where
    getValidityPeriod CrlShortcut {..} = ValidityPeriod notBefore notAfter

instance {-# OVERLAPPING #-} WithValidityPeriod RoaShortcut where
    getValidityPeriod RoaShortcut {..} = ValidityPeriod notBefore notAfter

instance {-# OVERLAPPING #-} WithValidityPeriod SplShortcut where
    getValidityPeriod SplShortcut {..} = ValidityPeriod notBefore notAfter

instance {-# OVERLAPPING #-} WithValidityPeriod AspaShortcut where
    getValidityPeriod AspaShortcut {..} = ValidityPeriod notBefore notAfter

instance {-# OVERLAPPING #-} WithValidityPeriod BgpSecShortcut where
    getValidityPeriod BgpSecShortcut {..} = ValidityPeriod notBefore notAfter

instance {-# OVERLAPPING #-} WithValidityPeriod GbrShortcut where
    getValidityPeriod GbrShortcut {..} = ValidityPeriod notBefore notAfter

instance WithResources CaShortcut where
    getResources CaShortcut { resources } = resources

instance WithResources RoaShortcut where
    getResources RoaShortcut { resources } = resources

instance WithResources SplShortcut where
    getResources SplShortcut { resources } = resources

instance WithResources AspaShortcut where
    getResources AspaShortcut { resources } = resources

instance WithResources BgpSecShortcut where
    getResources BgpSecShortcut { resources } = resources

instance WithResources GbrShortcut where
    getResources GbrShortcut { resources } = resources

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

instance WithResources Ca where
    getResources = \case 
        CaShort CaShortcut {..} -> resources
        CaFull (Located _ ca)   -> RPKI.Domain.getResources ca

