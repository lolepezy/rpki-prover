{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE UndecidableInstances       #-}

module RPKI.Validation.Partial where

import           Control.Lens
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Concurrent.Async

import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set
import           Data.Maybe (catMaybes)
import           Data.Coerce
import           Data.Tuple.Strict

import           GHC.Generics

import           RPKI.AppMonad
import           RPKI.Time
import           RPKI.Domain
import           RPKI.Util (ifJustM)
import           RPKI.Store.Database
import           RPKI.Store.Base.Serialisation
import           RPKI.Store.Base.Map      (SMap (..))
import           RPKI.Store.Base.MultiMap (SMultiMap (..))
import           RPKI.Store.Base.Storage
import           RPKI.Validation.Types
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
      - Keep a multimap Instant -> ObjectKey for it
      - Scan expiresAt, expire all the object for the given timestamp
      - Expire object means deleting all payloads found under that object
      - Expired objects stay in the tree and are filtered out by their validity 
        period, they become forever skipped, but it's easier to do that than
        actually modify MFT shortcuts
      - Similar applies to object that will be valid in the future (not yet valid). 
        IndexStore an index for them "maturesAt" and scan it periodically the same way as 
        expiresAt. Generate object updates for these objects when they mature.
 

    * What happens when the validator is launched after a long time of not running? 
       Nothing special, but
        - Updates can be big and hit the threshold of "validate whole repository" 
          or "validate whole TA"
        - Run expiration check first? (No, we skip stuff out of validity period 
          when traversing anything at all)

    * How do deal with invalid objects?
     - Complain when an object is found to be invalid
       - invalid manifest means going through the manifest selection process again
       - invalid CRL means the same as invalid manifest
       - invalid CA?
     - Wrap it in TroubledChild on the manifest shortcut

    * Is there going to be issues with key rollovers?


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
            | GbrP (T2 Hash Gbr)
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data Change a = Added a | Deleted a
    deriving (Show, Eq, Ord, Generic)    


{- 
    - Validate top-down with shortcuts and the function to be called when found payloads.
    - Look at the KI -> KIMeta and update it if needed after validation for CA succeeds
    - Generate "Delete Payload" for payloads corresponding to the removed MFT children
-} 
validateCA :: Storage s => IndexStore s -> CertKey -> STM (Change Payload) -> ValidatorT IO ()
validateCA store certKey onPayload = do
    validateCAPartially store certKey onPayload (const True)

{- 
    Filter will be used to 
      * Pick up only CAs that are on somebody's path to the top
      * Pick up payloads (or their shortcuts) that are in the set up updates
-}   
validateCAPartially :: Storage s 
                    => IndexStore s 
                    -> CertKey 
                    -> STM (Change Payload) 
                    -> (ObjectKey -> Bool) 
                    -> ValidatorT IO ()
validateCAPartially store@IndexStore {..} certKey onPayload objectFilter = do
    -- get CA from cache by certKey 
    caShort <- liftIO $ roTx caShortcuts $ \tx -> M.get tx caShortcuts certKey
    case caShort of
        Nothing -> 
            -- no shortcut, full validation needed
            -- validateCAFully store certKey onPayload objectFilter
            -- roTx store $ 
            pure ()

        Just ca@CaShortcut {..} -> do
            pure ()

    -- get MFT shortcut from cache, do the dance with comparing MFT to its shortcut

    -- Calculate MFT diff
    -- * for each added payload call onPayload (Added Payload)
    -- * for each deleted payload call onPayload (Deleted Payload)    
    -- * for each added CA validateCAPartially recursively
    -- * for each deleted CA call traversePayloads and delete payloads

    pure ()


traversePayloads :: Storage s => IndexStore s -> CertKey -> (Payload -> STM ()) -> Bool -> IO ()
traversePayloads store@IndexStore {..} certKey onPayload includeExpired = do     
    now <- thisInstant
    let ifNotExpired :: forall a . WithValidityPeriod a => a -> IO () -> IO ()
        ifNotExpired object f  = if includeExpired 
                            then f
                            else when (isWithinValidityPeriod now object) f
    
    ifJustM (roTx cert2mft $ \tx -> M.get tx cert2mft certKey) $ \mftKey -> 
        ifJustM (roTx mftShorts $ \tx -> M.get tx mftShorts mftKey) $ \mftShort -> do
            forM_ (Map.elems (mftShort ^. #nonCrlEntries)) $ \MftEntry {..} -> do
                case child of
                    CaChild s@CaShortcut {..} _ ->                     
                        ifNotExpired s $ traversePayloads store (coerce key) onPayload includeExpired                                            
                    RoaChild r@RoaShortcut {..} _ -> 
                        ifNotExpired r $ atomically $ onPayload $ VrpsP vrps
                    AspaChild a@AspaShortcut {..} _ -> 
                        ifNotExpired a $ atomically $ onPayload $ AspaP aspa                        
                    SplChild s@SplShortcut {..} _ -> 
                        ifNotExpired s $ atomically $ onPayload $ SplP splPayload                    
                    BgpSecChild b@BgpSecShortcut {..} _ -> 
                        ifNotExpired b $ atomically $ onPayload $ BgpSecP bgpSec
                    GbrChild g@GbrShortcut {..} _ -> 
                        ifNotExpired g $ atomically $ onPayload $ GbrP gbr
                    _ -> 
                        pure ()                                
              


findStartCas readFromCache accept newObjects = do
    cas <- fmap catMaybes $ forM newObjects $ \o -> do
                let ki = o ^. #ki
                mkiMeta <- readFromCache ki
                pure $ case mkiMeta of
                    Just kiMeta
                        | accept ki kiMeta -> Just (ki, kiMeta)
                        | otherwise     -> Nothing
                    Nothing -> Nothing

    let startCas = Set.fromList [ kiMeta ^. #caCertificate | (_, kiMeta) <- cas ]    

    (paths, ignored) <- 
        fmap mconcat $ forConcurrently cas $ \ca -> do 
            -- TODO Here we should complain when nothing is found
            findPathUp readFromCache accept ca startCas
            
    pure (Set.difference startCas ignored, paths)


findPathUp readFromCache accept (ki, kiMeta) startCas = 
    go readFromCache accept (ki, kiMeta) startCas mempty mempty 
  where 
    go readFromCache accept (ki, kiMeta) startCas paths ignored = do
        let parentKI = kiMeta ^. #parentKI
        let certKey = kiMeta ^. #caCertificate

        let paths' = Set.insert certKey paths

        -- if it's the root stop                    
        if ki == parentKI then
            pure (paths', ignored)
        else 
            readFromCache parentKI >>= \case
                Just parent
                    | accept parentKI parent -> do 
                        let parentCa = parent ^. #caCertificate                        
                        let ignored' = 
                                if parentCa `Set.member` startCas 
                                    then ignored <> paths'
                                    else ignored

                        go readFromCache accept 
                            (parentKI, parent) startCas paths' ignored'

                    | otherwise -> 
                        -- Parent that is not acceptable (expire or not valid yet)
                        -- it means no path, so ignore the whole path
                        pure (mempty, paths')

                Nothing -> 
                    -- No parent, again it means no path, 
                    -- ignore the whole path
                    pure (mempty, paths')