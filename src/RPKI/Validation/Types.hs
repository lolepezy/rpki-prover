{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DerivingVia                #-}

module RPKI.Validation.Types where

import           Data.Aeson.Types
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import           Data.Tuple.Strict
import           GHC.Generics

import           Data.Proxy
import           Data.Swagger hiding (url)

import           RPKI.Orphans.Json
import           RPKI.Orphans.Swagger
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
        key            :: ObjectKey,
        notValidBefore :: Instant,
        notValidAfter  :: Instant        
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data MftShortcut = MftShortcut { 
        key            :: ObjectKey,
        nonCrlEntries  :: Map.Map ObjectKey MftEntry,
        notValidBefore :: Instant,
        notValidAfter  :: Instant,        
        serial         :: Serial,
        manifestNumber :: Serial,
        crlShortcut    :: CrlShortcut        
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data CaShortcut = CaShortcut { 
        key            :: ObjectKey,        
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
        key            :: ObjectKey,        
        vrps           :: [Vrp],
        notValidBefore :: Instant,
        notValidAfter  :: Instant,
        resources      :: AllResources
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data SplShortcut = SplShortcut {
        key            :: ObjectKey,        
        splPayload     :: SplPayload,
        notValidBefore :: Instant,
        notValidAfter  :: Instant,
        resources      :: AllResources
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data AspaShortcut = AspaShortcut {
        key            :: ObjectKey,
        aspa           :: Aspa,
        notValidBefore :: Instant,
        notValidAfter  :: Instant,
        resources      :: AllResources
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data BgpSecShortcut = BgpSecShortcut {
        key            :: ObjectKey,
        bgpSec         :: BGPSecPayload,
        notValidBefore :: Instant,
        notValidAfter  :: Instant,
        resources      :: AllResources
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data GbrShortcut = GbrShortcut {
        key            :: ObjectKey,    
        gbr            :: T2 Hash Gbr,
        notValidBefore :: Instant,
        notValidAfter  :: Instant,
        resources      :: AllResources
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)


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
