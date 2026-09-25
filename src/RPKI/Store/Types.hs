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
import           RPKI.Store.Base.Serialisation

data StorableTA = StorableTA {
        tal                 :: TAL,
        taCertKey           :: ObjectKey,
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


-- | Lifecycle state stored in the 'objects' table.
--
-- 'OriginalRO' covers both parse failures and prevalidation failures;
-- in both cases the raw bytes are retained alongside the merged
-- ValidationState that records why the object was not promoted.
--
-- 'WellStructuredRO' is produced only when both parsing AND 'prevalidateObject'
-- complete without any validation errors.
data RpkiObjectLifecycle
    = OriginalRO ObjectOriginal ValidationState Hash RpkiObjectType
    | WellStructuredRO WellStructuredRpkiObject
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)

instance {-# OVERLAPPING #-} WithHash RpkiObjectLifecycle where
    getHash (OriginalRO _ _ h _)   = h
    getHash (WellStructuredRO vro) = getHash vro

instance WithRpkiObjectType RpkiObjectLifecycle where
    getRpkiObjectType (OriginalRO _ _ _ t)   = t
    getRpkiObjectType (WellStructuredRO vro) = getRpkiObjectType vro


-- | One object reduced to exactly what its rows in 'objects' and the index
-- tables need, so that storing it is only the INSERTs. The object itself is
-- gone by this point: nothing here needs decoding again.
data PreparedObject = PreparedObject {
        hash       :: Hash,
        objectType :: RpkiObjectType,
        -- | The compressed, serialised lifecycle: the `data` column.
        payload    :: BS.ByteString,
        -- | Raw bytes, only for an object that did not make it to well-structured.
        original   :: Maybe BS.ByteString,
        indexEntry :: Maybe ObjectIndexEntry
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary, NFData)

data ObjectIndexEntry
    = CertificateIndex SKI (Maybe AKI)
    -- | The rest of 'MftMeta', which also needs the object key the INSERT assigns.
    | ManifestIndex AKI Serial Instant Instant
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary, NFData)


-- data 

data DBFileStats = DBFileStats {
    fileSize :: Size
} deriving stock (Show, Eq, Generic)

data TotalDBStats = TotalDBStats {    
    fileStats    :: DBFileStats
} deriving stock (Show, Eq, Generic)