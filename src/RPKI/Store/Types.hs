{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLabels     #-}

module RPKI.Store.Types where

import           Control.Lens
import           Control.DeepSeq
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Short    as BSS

import           GHC.Generics
import           RPKI.TAL

import           RPKI.Time                (Instant)

import           RPKI.Repository
import           RPKI.AppTypes
import           RPKI.Domain
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Serialisation

data StorableTA = StorableTA {
        tal                 :: TAL,
        taCert              :: CaCerObject,
        fetchStatus         :: FetchStatus,
        initialRepositories :: PublicationPointAccess,
        actualUrl           :: RpkiURL
    } 
    deriving (Show, Eq, Generic, TheBinary)

data ObjectMeta = ObjectMeta {
        insertedBy :: {-# UNPACK #-} WorldVersion,
        objectType :: RpkiObjectType
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary, NFData)

data MftMeta = MftMeta { 
        key       :: {-# UNPACK #-} ObjectKey,
        mftNumber :: {-# UNPACK #-} Serial,
        thisTime  :: {-# UNPACK #-} Instant,
        nextTime  :: {-# UNPACK #-} Instant 
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)

instance Ord MftMeta where
    compare a b = compare (a ^. #thisTime) (b ^. #thisTime) <>
                  compare (a ^. #nextTime) (b ^. #nextTime) <> 
                  compare (a ^. #mftNumber) (b ^. #mftNumber)

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
} deriving stock (Show, Eq, Generic)

data TotalDBStats = TotalDBStats {
    storageStats :: StorageStats,
    total        :: SStats,
    fileStats    :: DBFileStats
} deriving stock (Show, Eq, Generic)