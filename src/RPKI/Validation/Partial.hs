{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE UndecidableInstances       #-}

module RPKI.Validation.Partial where

import           Data.Aeson.Types
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import           Data.Tuple.Strict
import           GHC.Generics

import           Data.Proxy
import           Data.Swagger hiding (url)

import           RPKI.Orphans.Swagger
import           RPKI.Time
import           RPKI.Domain
import           RPKI.Repository
import           RPKI.Resources.Types
import           RPKI.Store.Base.Serialisation
import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))
import           RPKI.Store.Base.SafeMap  (SafeMap)
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage



-- Types

data UpdateHappened = ObjectUpdate AddedObject
                    | RepositoryUpdate RpkiURL
                    | TaUpdate TaName
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data AddedObject = AddedObject {
        objectKey :: ObjectKey,
        aki       :: AKI        
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)


-- Database

data KIMeta = KIMeta {
        caCertificate :: {-# UNPACK #-} ObjectKey,
        parentKI      :: {-# UNPACK #-} KI,
        expiresAt     :: {-# UNPACK #-} Instant        
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)


data Store s = Store {
        kiMetas   :: SMap "ki-meta" s KI KIMeta,
        expiresAt :: SMultiMap "expires-at" s Instant ObjectKey
    }
    deriving (Generic)


-- Actual validation    

xxx :: Storage s => [UpdateHappened] -> Store s -> IO ()
xxx updates store = do 
    
    pure ()
