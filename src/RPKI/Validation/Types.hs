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

import qualified Data.Map.Strict             as Map
import qualified Data.Text                   as Text
import qualified Data.Set                    as Set
import           GHC.Generics

import           RPKI.Time
import           RPKI.Domain
import           RPKI.Repository
import           RPKI.Store.Base.Serialisation
import           RPKI.Store.Types


-- Shortcuts

data MftChild = CaChild CaShortcut Serial
              | RoaChild RoaShortcut Serial
              | AspaChild AspaShortcut Serial
              | BgpSecChild BgpSecShortcut Serial
              | GbrChild GbrShortcut Serial
              | TroubledChild ObjectKey
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary


data MftEntry = MftEntry {        
        fileName :: Text.Text,
        child    :: MftChild
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

data MftShortcut = MftShortcut { 
        key            :: ObjectKey,
        entries        :: Map.Map ObjectKey MftEntry
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

data CrlShortcut = CrlShortcut { 
        key            :: ObjectKey,        
        revokedSerials :: Set.Set Integer
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

data CaShortcut = CaShortcut { 
        key            :: ObjectKey,        
        ski            :: SKI,
        ppas           :: PublicationPointAccess,
        notValidBefore :: Instant,
        notValidAfter  :: Instant
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

data Ca = CaShort CaShortcut
        | CaFull CaCerObject
    deriving stock (Show, Eq, Generic)
    deriving anyclass TheBinary


data RoaShortcut = RoaShortcut {
        key            :: ObjectKey,        
        vrps           :: [Vrp],
        notValidBefore :: Instant,
        notValidAfter  :: Instant
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

data AspaShortcut = AspaShortcut {
        key            :: ObjectKey,
        aspa           :: Aspa,
        notValidBefore :: Instant,
        notValidAfter  :: Instant
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

data BgpSecShortcut = BgpSecShortcut {
        key            :: ObjectKey,
        bgpSec         :: BGPSecPayload,
        notValidBefore :: Instant,
        notValidAfter  :: Instant
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

data GbrShortcut = GbrShortcut {
        key            :: ObjectKey,    
        gbr            :: (Hash, Gbr),
        notValidBefore :: Instant,
        notValidAfter  :: Instant
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary


instance WithValidityPeriod RoaShortcut where
    getValidityPeriod RoaShortcut {..} = (notValidBefore, notValidAfter)

instance WithValidityPeriod AspaShortcut where
    getValidityPeriod AspaShortcut {..} = (notValidBefore, notValidAfter)

instance WithValidityPeriod BgpSecShortcut where
    getValidityPeriod BgpSecShortcut {..} = (notValidBefore, notValidAfter)

instance WithValidityPeriod GbrShortcut where
    getValidityPeriod GbrShortcut {..} = (notValidBefore, notValidAfter)