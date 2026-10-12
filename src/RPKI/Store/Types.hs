{-# LANGUAGE StrictData #-}

module RPKI.Store.Types where

import           Control.Lens
import           Control.DeepSeq
import           Control.Monad            (when)
import           Control.Monad.IO.Class
import qualified Data.ByteString          as BS
import           Data.Foldable            (for_)
import           Data.IORef
import           Data.Maybe               (isJust)

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
        nextTime  :: {-# UNPACK #-} Instant,
        -- | `manifestContentDigest` of the manifest
        contentDigest :: Hash
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

-- | What inserting an object did.
data Inserted = Inserted {
        key         :: ObjectKey,
        -- | The type of the object, if it wasn't stored before
        newObject   :: Maybe RpkiObjectType,
        -- | Only for a well-structured manifest that wasn't stored before
        newManifest :: Maybe NewManifest
    }
    deriving stock (Show, Eq, Generic)

-- | A manifest that a fetch stored.
data NewManifest = NewManifest {
        key           :: ObjectKey,
        aki           :: AKI,
        thisTime      :: Instant,
        contentDigest :: Hash
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

-- | The manifests a fetch stored: at most `manifestUpdatesCap` of them, past
-- that only the fact that there were many.
data ManifestUpdates = StoredManifests [NewManifest]
                     | ManyManifests
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

manifestUpdatesCap :: Int
manifestUpdatesCap = 1000

instance Semigroup ManifestUpdates where
    StoredManifests a <> StoredManifests b
        | not $ null $ drop manifestUpdatesCap ab = ManyManifests
        | otherwise                                = StoredManifests ab
      where
        ab = a <> b
    _ <> _ = ManyManifests

instance Monoid ManifestUpdates where
    mempty = StoredManifests []

-- | What a fetch stored that can be a reason to revalidate.
data FetchUpdates = FetchUpdates {
        manifests  :: ManifestUpdates,
        -- | Whether it stored a new object other than a manifest or a CRL
        newObjects :: Bool
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

instance Semigroup FetchUpdates where
    FetchUpdates m1 o1 <> FetchUpdates m2 o2 = FetchUpdates (m1 <> m2) (o1 || o2)

instance Monoid FetchUpdates where
    mempty = FetchUpdates mempty False

-- | Remember what an insert stored, if it stored anything.
rememberInserted :: MonadIO m => IORef FetchUpdates -> Inserted -> m ()
rememberInserted stored Inserted { newObject, newManifest } = liftIO $
    for_ newObject $ \objectType -> do
        let other = objectType `notElem` [MFT, CRL]
        FetchUpdates { newObjects } <- readIORef stored
        -- Most of the objects of a snapshot change nothing here
        when (isJust newManifest || other && not newObjects) $
            atomicModifyIORef' stored $ \updates ->
                (updates <> FetchUpdates (foldMap (StoredManifests . pure) newManifest) other, ())

data ObjectIndexEntry
    = CertificateIndex SKI (Maybe AKI)
    -- | The rest of 'MftMeta', which also needs the object key the INSERT assigns.
    | ManifestIndex AKI Serial Instant Instant Hash
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary, NFData)


-- data 

data DBFileStats = DBFileStats {
    fileSize :: Size
} deriving stock (Show, Eq, Generic)

data TotalDBStats = TotalDBStats {    
    fileStats    :: DBFileStats
} deriving stock (Show, Eq, Generic)