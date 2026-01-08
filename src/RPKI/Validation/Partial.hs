{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE UndecidableInstances       #-}

module RPKI.Validation.Partial where

import           GHC.Generics

import           RPKI.Time
import           RPKI.Domain
import           RPKI.Store.Base.Serialisation
import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))
import           RPKI.Store.Base.Storage
import           RPKI.Validation.Types (MftShortcut)


{- 

Validate objects partially, i.e. only those that were affected by changes in a repository. 
Try to minimize the amount of work needed.

When an update happens (object added/deleted, repository updated, TA updated) create a 
list of UpdateHappened 

For each repository update count how many objects were added/deleted (already happening). 
Based on that decide if the whole repository needs to be re-validated or only the sub-tree 
related to the changed objects: 

Create re-validation tasks of the kind

- Added ParentKI ObjectKey
- Or WholeRepository RepositoryId
- Or WholeTA TaName

While going down the tree create a diff of payloads, i.e. the result of validation must 
be Diff Payload.

The whole machinery of shortcuts should be in place, except for refactoring manifest 
shortcuts into the form where MFT shortcut only stores ObjectKeys of children and not 
children themselves

Do periodic full re-validation of all TAs "just in case"?

Keep track of the objects that are "about to expire" (i.e. a multi-map of 
(expirationDate, ObjectKey)) and do partial re-validation for them 
specifically, not the whole TA.

Create a tree of CAs and payloads, i.e store
    KI -> KIMeta (parentKI, caCertificate, expiresAt) -- index to find a CA by its KI
    CertKey -> MftKey -- certificate to its current manifest mapping 
    MftKey -> MftShortcut -- manifest shortcuts

That creates the tree of all valid (validated) CAs and their validated payloads 
that were under these CAs. It is a snapshot of the current TA tree.

When an update happens, we can find the affected CAs by their KIs and 
then traverse down the tree to re-validate only affected objects.


Open questions:

    * How to keep track of which objects are to be deleted and to be kept? 
      Current idea with visited objects wouldn't work (or would it?)
       - One idea: periodically dump the whole tree. If the union of 
         last N dumps contains an ObjectKey that means it is still 
         useful, otherwise it is to be deleted.

    * What happens when the validator is launched after a long time of not running? 
       Nothing special, but
        - Updates can be big and hit the threshold of "validate whole repository" 
          or "validate whole TA"
        - Run expiration check first?
        - 

    * What happens if a validation process was interrupted?
        Idea: 
        - have a persistent log of changes WorldVersion -> [UpdateHappened]
        - have a persisten log of appled changes WorldVersion -> [Diff Payload]
        - on startup compare the two logs and apply missing changes
        - in one transaction 
            + write applied changes to the log
            + update the tree store (KI -> KIMeta, CertKey -> MftKey)
            + update all manifest shortcuts (or not? they might be written asynchronously)


    Main parts: 

        data Payload = VrpsP [Vrp]                      
                     | AspaP Aspa
                     | BgpSecP BgpSec
                     | SplP Spl
                     | GbrP Gbr
            deriving (Show, Eq, Ord, Generic)
            deriving anyclass (TheBinary)

        -- validate top-down with shortcuts is the function to be called 
        -- when found payloads
        validateCA :: Bla -> CertKey -> STM Payload -> ValidatorT IO ()
        validateCA bla certKey onPayload = do ... pure ()


        traversePayloads :: Bla -> ObjectKey -> STM Payload -> ValidatorT IO ()
        traversePayloads bla objectKey onPayload = pure ()


        -- to delete payloads 



        ** Bla is the usual "Storage s => AppContext s -> TopDownContext"

-}


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
        caCertificate :: {-# UNPACK #-} CertKey,
        parentKI      :: {-# UNPACK #-} KI,
        expiresAt     :: {-# UNPACK #-} Instant        
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)


data Store s = Store {
        kiMetas   :: SMap "ki-meta" s KI KIMeta,
        expiresAt :: SMultiMap "expires-at" s Instant ObjectKey,
        cert2mft  :: SMap "cert-to-mft" s CertKey MftKey,
        mftShorts :: SMap "mft-shorts" s MftKey MftShortcut
    }
    deriving (Generic)


-- Actual validation    

xxx :: Storage s => [UpdateHappened] -> Store s -> IO ()
xxx updates store = do 
    
    pure ()
