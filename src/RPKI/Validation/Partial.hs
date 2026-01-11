{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE UndecidableInstances       #-}

module RPKI.Validation.Partial where

import           Control.Concurrent.STM

import           GHC.Generics

import           RPKI.AppMonad
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

While going down the tree create a diff of payloads, i.e. 
result of validation is Diff Payload.

The whole machinery of shortcuts should be in place, probably add to it 
refactoring manifest shortcuts into the form where they store only ObjectKeys 
of children and not children themselves (TODO It will make sense only 
if fileName can also be stored outside of the MFT shortcut).

Do periodic full re-validation of all TAs "just in case"? (: No)

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


Ideas:

    * How to keep track of which objects are to be deleted and to be kept? 
      Current idea with visited objects wouldn't work (or would it?)

      Idea: periodically dump the whole tree. If the union of 
         last N dumps contains an ObjectKey that means it is still 
         useful, otherwise it is to be deleted.        


    * What happens if a validation process was interrupted?
        Idea: 
        - have a persistent log of changes WorldVersion -> [UpdateHappened]
        - have a persisten log of appled changes WorldVersion -> [Diff Payload]
        - on startup compare the two logs and apply missing changes
        - in one transaction 
            + write applied changes to the log
            + update the tree store (KI -> KIMeta, CertKey -> MftKey)
        - MFT shortcuts should be written asynchronously as already implemented


    * How to deal expiring objects? Idea:
      - Keep a mutimap Instant -> ObjectKey for it
      - Scan expiresAt, expire all the object for the given timestamp
      - Expire object means deleting all payloads found under that object
      - Expired objects stay in the tree and are filtered out by their validity 
        period, they become forever skipped, but it's easier to do that than
        actually modify MFT shortcuts


    * What happens when the validator is launched after a long time of not running? 
       Nothing special, but
        - Updates can be big and hit the threshold of "validate whole repository" 
          or "validate whole TA"
        - Run expiration check first?


    * Pre-group of filter update log? 
        - Exlude repeated updates of the same repository/TA?
-}


-- Types

data UpdateHappened = ObjectUpdate AddedObject
                    | RepositoryUpdate RpkiURL
                    | TaUpdate TaName
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data AddedObject = AddedObject {
        objectKey :: ObjectKey,
        ki        :: KI        
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)


-- It is to simplify the definition of Payload handlers        
data Payload = VrpsP [Vrp]                      
            | AspaP Aspa
            | BgpSecP BGPSecPayload
            | SplP SplPayload
            | GbrP Gbr
    deriving (Show, Eq, Ord, Generic)
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




-- Validate top-down with shortcuts and the function to be called when found payloads.
-- Look at the KI -> KIMeta and update it if needed after validation for CA succeeds
-- 
validateCA :: CertKey -> STM Payload -> ValidatorT IO ()
validateCA certKey onPayload = do
    -- 
    pure ()

-- Filter will be used to 
--   * Pick up only CAs that are on somebody's path to the top
--   * Pick up payloads (or their shortcuts) that are in the set up updates
--    
validateCAPartially :: CertKey -> STM Payload -> (ObjectKey -> Bool) -> ValidatorT IO ()
validateCAPartially certKey onPayload filter = do
    pure ()


traversePayloads :: ObjectKey -> STM Payload -> Bool -> ValidatorT IO ()
traversePayloads objectKey onPayload includeExpired = do     
    pure ()


-- Find all start CAs based on the list of updates happened
findStartCas :: [UpdateHappened] -> ValidatorT IO [CertKey]
findStartCas updates = do
    pure []
  where         
    findStartCa = \case 
        ObjectUpdate AddedObject {..} -> do
            pure $ Just (objectKey, ki)

        RepositoryUpdate repoUrl -> do
            -- Get the first from the top CA in the hierarchy 
            -- that points to this repository
            pure Nothing

        TaUpdate taName -> do
            -- Get the TA certificate
            pure Nothing