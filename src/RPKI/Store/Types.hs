{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE StrictData            #-}

module RPKI.Store.Types where

import           Control.DeepSeq
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Short    as BSS
import           Data.List.NonEmpty          (NonEmpty (..))

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

data MftTimingMark = MftTimingMark Instant Instant 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data MftRef = MftRealRef ObjectKey
            | MftShortcutRef ObjectKey
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary) 

data AnMft = AnMft { 
        ref        :: MftRef,
        thisUpdate :: {-# UNPACK #-} Instant,
        nextUpdate :: {-# UNPACK #-} Instant
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

newtype Mfts = Mfts (NonEmpty AnMft)
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

newtype DBFileStats = DBFileStats {
    fileSize :: Size
} deriving stock (Show, Eq, Generic)

data TotalDBStats = TotalDBStats {
    storageStats :: StorageStats,
    total        :: SStats,
    fileStats    :: DBFileStats
} deriving stock (Show, Eq, Generic)