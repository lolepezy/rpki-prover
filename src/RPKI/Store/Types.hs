{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData        #-}

module RPKI.Store.Types where

import           Control.Lens
import           Control.DeepSeq
import qualified Data.ByteString          as BS

import           GHC.Generics
import           RPKI.TAL

import           RPKI.Time                (Instant)

import           RPKI.Repository
import           RPKI.AppTypes
import           RPKI.Domain
import           RPKI.Reporting           (ValidationState)
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Serialisation

data StorableTA = StorableTA {
        tal                 :: TAL,
    taCert              :: WellStructuredCaCert,
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

data Keyed a = Keyed { 
        object :: a,
        key    :: {-# UNPACK #-} ObjectKey
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)        

newtype ObjectOriginal = ObjectOriginal BS.ByteString
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)        


-- | Lifecycle state stored in the 'objects' LMDB map.
--
-- 'OriginalRO' covers both parse failures and prevalidation failures;
-- in both cases the raw bytes are retained alongside the merged
-- ValidationState that records why the object was not promoted.
--
-- 'ValidatedRO' is produced only when both parsing AND 'prevalidateObject'
-- complete without any validation errors.
data RpkiObjectLifecycle
    = OriginalRO ObjectOriginal ValidationState Hash RpkiObjectType
    | ValidatedRO ValidatedRpkiObject
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)

instance {-# OVERLAPPING #-} WithHash RpkiObjectLifecycle where
    getHash (OriginalRO _ _ h _) = h
    getHash (ValidatedRO vro)    = getHash vro

instance WithRpkiObjectType RpkiObjectLifecycle where
    getRpkiObjectType (OriginalRO _ _ _ t) = t
    getRpkiObjectType (ValidatedRO vro)    = getRpkiObjectType vro


-- data 

data DBFileStats = DBFileStats {
    fileSize :: Size
} deriving stock (Show, Eq, Generic)

data TotalDBStats = TotalDBStats {
    storageStats :: StorageStats,
    total        :: SStats,
    fileStats    :: DBFileStats
} deriving stock (Show, Eq, Generic)