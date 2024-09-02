{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE OverloadedStrings     #-}

module RPKI.Store.Types where

import           Control.DeepSeq
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Short    as BSS
import           Data.Monoid.Generic
import qualified Data.Set                 as Set
import           Data.Tuple.Strict
import           Data.Kind
import           Data.Store               hiding (Size)

import           GHC.Generics
import           RPKI.TAL

import           RPKI.Time                (Instant)

import           RPKI.Reporting
import           RPKI.Repository
import           RPKI.AppTypes
import           RPKI.Domain
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Serialisation

data StorableTA = StorableTA {
        tal                 :: TAL,
        taCert              :: CaCerObject,
        fetchStatus         :: FetchStatus,
        initialRepositories :: PublicationPointAccess
    } 
    deriving (Show, Eq, Generic, TheBinary)

data ObjectMeta = ObjectMeta {
        insertedBy :: {-# UNPACK #-} WorldVersion,
        objectType :: RpkiObjectType
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary, NFData)

data MftTimingMark = MftTimingMark Instant Instant 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

newtype SafeUrlAsKey = SafeUrlAsKey BSS.ShortByteString 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)        

data Keyed a = Keyed { 
        object :: a,
        key    :: {-# UNPACK #-} ObjectKey
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)        

newtype ObjectOriginal = ObjectOriginal BS.ByteString
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)        

data DBFileStats = DBFileStats {
        fileSize :: Size
    } 
    deriving stock (Show, Eq, Generic)

data TotalDBStats = TotalDBStats {
        storageStats :: StorageStats,
        total        :: SStats,
        fileStats    :: DBFileStats
    } 
    deriving stock (Show, Eq, Generic)

newtype AsIs a = AsIs { unAsIs :: a }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)


data FlatPayloads = FlatPayloads {
        roas        :: Roas,
        vrps        :: Vrps,
        spls        :: Set.Set SplN,
        aspas       :: Set.Set Aspa,
        gbrs        :: Set.Set (T2 Hash Gbr),
        bgpCerts    :: Set.Set BGPSecPayload,
        validations :: Validations,
        metrics     :: RawMetric,
        traces      :: Set.Set Trace
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)


data PayloadsDiff = PayloadsDiff {
        roas        :: GeneralDiff Roas,
        vrps        :: GeneralDiff Vrps,
        spls        :: GeneralDiff (Set.Set SplN),
        aspas       :: GeneralDiff (Set.Set Aspa),
        gbrs        :: GeneralDiff (Set.Set (T2 Hash Gbr)),
        bgpCerts    :: GeneralDiff (Set.Set BGPSecPayload),
        validations :: GeneralDiff Validations,
        metrics     :: GeneralDiff RawMetric,
        traces      :: GeneralDiff (Set.Set Trace)
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)


data GeneralDiff a = GeneralDiff {
        added   :: a,
        deleted :: a
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)        
