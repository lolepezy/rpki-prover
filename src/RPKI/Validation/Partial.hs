{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE UndecidableInstances       #-}

module RPKI.Validation.Partial where

import           Control.Lens
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class

import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set
import           Data.Maybe (catMaybes)

import           GHC.Generics

import           RPKI.AppMonad
import           RPKI.Time
import           RPKI.Domain
import           RPKI.Store.Base.Serialisation
import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))
import           RPKI.Store.Base.Storage
import           RPKI.Validation.Types (MftShortcut)
import           RPKI.Validation.ObjectValidation

import qualified RPKI.Store.Base.Map      as M
import qualified RPKI.Store.Base.MultiMap as MM


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
      Idea: No more "visited object", periodically dump the whole tree. 
        If the union of last N dumps contains an ObjectKey that means 
        it is still useful, otherwise it is to be deleted.

    * What happens if a validation process was interrupted?
       Idea: 
        - have a persistent log of changes WorldVersion -> [UpdateHappened]
        - have a persistenr log of appled changes WorldVersion -> [Diff Payload]
        - on startup compare the two logs and apply missing changes
        - in one transaction 
            + write applied changes to the log
            + update the tree store (KI -> KIMeta, CertKey -> MftKey)
        - MFT shortcuts should be written asynchronously as already implemented


    * How to deal with expiring objects? Idea:
      - Keep a mutimap Instant -> ObjectKey for it
      - Scan expiresAt, expire all the object for the given timestamp
      - Expire object means deleting all payloads found under that object
      - Expired objects stay in the tree and are filtered out by their validity 
        period, they become forever skipped, but it's easier to do that than
        actually modify MFT shortcuts
      - Similar applies to object that will be valid in the future (not yet valid). 
        Store an index for them "maturesAt" and scan it periodically the same way as 
        expiresAt. Generate object updates for these objects when they mature.
 

    * What happens when the validator is launched after a long time of not running? 
       Nothing special, but
        - Updates can be big and hit the threshold of "validate whole repository" 
          or "validate whole TA"
        - Run expiration check first? (No, we skip stuff out of validity period 
          when traversing anything at all)


    * Pre-group of filter update log? 
        - Exclude repeated updates of the same repository/TA?
        - Do not write TA/repository task in the log if there already one?


    * How to stop updating repositories that are not referred by CAs?
        Idea: 
         - store multimaps RepositoryKey -> ObjectKey and ObjectKey -> RepositoryKey
         - each repository maintaining thread can see if there are any ObjectKeys 
           referring to its repository, if none, it can stop

    * How to associate repository to a key? 
      - RRDP - simple
      - rsync - add key to the leafs of the tree
      - it might be that we'll never need to have an operation of "find repository by key", 
        so no need to have indexes for it
-}



-- Types

data UpdateHappened = ObjectUpdate AddedObject
                    | RepositoryUpdate RpkiURL
                    | TaUpdate TaName
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data AddedObject = AddedObject {
        objectKey :: {-# UNPACK #-} ObjectKey,
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

data Change a = Added a | Deleted a
    deriving (Show, Eq, Ord, Generic)    

-- Database

data KIMeta = KIMeta {
        caCertificate  :: {-# UNPACK #-} CertKey,
        parentKI       :: {-# UNPACK #-} KI,
        notValidBefore :: {-# UNPACK #-} Instant,
        notValidAfter  :: {-# UNPACK #-} Instant
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

instance {-# OVERLAPPING #-} WithValidityPeriod KIMeta where 
    getValidityPeriod KIMeta {..} = (notValidBefore, notValidAfter)


data ValidityPeriodIndex = VP_CA CertKey 
                         | VP_MFT MftKey
                         | VP_CRL ObjectKey MftKey
                         | VP_Child ObjectKey MftKey
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data Store s = Store {
        kiMetas   :: SMap "ki-meta" s KI KIMeta,        
        cert2mft  :: SMap "cert-to-mft" s CertKey MftKey,
        mftShorts :: SMap "mft-shorts" s MftKey MftShortcut,

        expiresAt :: SMultiMap "expires-at" s Instant ValidityPeriodIndex,
        maturesAt :: SMultiMap "matures-at" s Instant ValidityPeriodIndex,

        -- TODO Might be PP -> object?
        repository2object :: SMultiMap "repo-key-to-obj-keys" s RepositoryKey ObjectKey
    }
    deriving (Generic)



instance Storage s => WithStorage s (Store s) where
    storage Store {..} = storage kiMetas


{- 
    - Validate top-down with shortcuts and the function to be called when found payloads.
    - Look at the KI -> KIMeta and update it if needed after validation for CA succeeds
    - Generate "Delete Payload" for payloads corresponding to the removed MFT children
-} 
validateCA :: CertKey -> STM (Change Payload) -> ValidatorT IO ()
validateCA certKey onPayload = do
    validateCAPartially certKey onPayload (const True)

{- 
    Filter will be used to 
      * Pick up only CAs that are on somebody's path to the top
      * Pick up payloads (or their shortcuts) that are in the set up updates
-}   
validateCAPartially :: CertKey -> STM (Change Payload) -> (ObjectKey -> Bool) -> ValidatorT IO ()
validateCAPartially certKey onPayload objectFilter = do
    -- get from cache CA by certKey

    -- get from cache MFT shortcut, do the dance with comparing MFT to its shortcut

    -- Calculate MFT diff
    -- * for each added payload call onPayload (Added Payload)
    -- * for each deleted payload call onPayload (Deleted Payload)    
    -- * for each added CA validateCAPartially recursively
    -- * for each deleted CA call traversePayloads and delete payloads

    pure ()


traversePayloads :: ObjectKey -> STM Payload -> Bool -> ValidatorT IO ()
traversePayloads objectKey onPayload includeExpired = do     
    pure ()


-- Find all start CAs based on the list of updates happened
findStartCas :: Storage s => [UpdateHappened] -> Store s -> IO [KIMeta]
findStartCas updates store@Store {..} = do
    now <- thisInstant
    startCas <- fmap (Map.fromList . catMaybes) $ forM updates $ findStartCa now

    -- Find paths to the top CAs for each start CA found
    -- While going up, check that the CA is still valid (not expired)

    pure []
  where             
    findStartCa now = \case 
        ObjectUpdate AddedObject {..} -> do
            z <- roTx store $ \tx -> M.get tx kiMetas ki
            pure $! case z of
                Just km
                    | isWithinValidityPeriod now km -> Just (ki, km)
                    | otherwise       -> Nothing
                Nothing -> 
                    -- Orphaned object, no parent CA found
                    Nothing            

        RepositoryUpdate repoUrl -> do
            -- Get the first from the top CA in the hierarchy 
            -- that points to this repository
            pure Nothing

        TaUpdate taName -> do
            -- Get the TA certificate
            pure Nothing

stepUp readFromCache accept (ki, kiMeta) startCas path ignore = do
    let parentKI = kiMeta ^. #parentKI

    -- if it's the TA stop                    
    if ki == parentKI then
        pure (startCas, path, ignore)
    else do
        z <- readFromCache parentKI
        case z of
            Just parent
                | accept parent -> do 
                    let parentCa = parent ^. #caCertificate
                    let path' = Set.insert parentCa path 
                    let ignore' = 
                            if parentCa `Set.member` startCas 
                                then Set.insert (kiMeta ^. #caCertificate) ignore
                                else ignore

                    stepUp readFromCache accept 
                        (parentKI, parent) 
                        (Set.insert (kiMeta ^. #caCertificate) startCas) 
                        path' ignore'

                | otherwise       -> 
                    pure (startCas, path, ignore)

            Nothing -> 
                -- No parent, stop here
                -- TODO Figure out what to do 
                pure (startCas, path, ignore)            
