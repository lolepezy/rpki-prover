{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

module RPKI.Validation.Partial where

import           Control.Lens hiding (ignored)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Concurrent.Async

import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set
import qualified Data.Vector              as V
import           Data.Traversable
import           Data.Maybe (catMaybes)
import           Data.Coerce
import           Data.Tuple.Strict

import           Data.Generics.Product.Fields
import           Data.String.Interpolate.IsString

import           GHC.Generics

import           RPKI.AppTypes
import           RPKI.AppMonad
import           RPKI.AppContext
import           RPKI.Time
import           RPKI.Domain
import           RPKI.TAL
import           RPKI.Logging
import           RPKI.Reporting
import           RPKI.Util (ifJustM, increment)
import           RPKI.Store.Database (DB)
import qualified RPKI.Store.Database as DB
import           RPKI.Fetch.Common
import           RPKI.Store.Base.Storage
import           RPKI.Validation.Types
import           RPKI.Validation.ObjectValidation

import qualified RPKI.Store.Base.Map      as M
import qualified RPKI.Store.Base.MultiMap as MM
import UnliftIO (pooledForConcurrentlyN)



{- 

Validate objects partially, i.e. only those that were affected by changes in a repository. 
Try to minimize the amount of work needed.

When an update happens (object added/deleted, repository updated, TA updated) create a 
list of Update 

For each repository update count how many objects were added/deleted (already happening). 
Based on that decide if the whole repository needs to be re-validated or only the sub-tree 
related to the changed objects: 

Create re-validation tasks of the kind

- Added AKI ObjectKey
- Or WholeRepository RepositoryId
- Or WholeTA TaName

While going down the tree create a diff of payloads, i.e. 
result of validation is Diff Payload.

The whole machinery of shortcuts should be in place, probably add to it 
refactoring manifest shortcuts into the form where they store only ObjectKeys 
of children and not children themselves (TODO It will make sense only 
if fileName can also be stored outside of the MFT shortcut, which is 
probably not the case).

Create a tree of CAs and payloads, i.e store
    KI -> KIMeta (aki, caCertificate, expiresAt) -- index to find a CA by its KI
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
        - have a persistent log of changes WorldVersion -> [Update]
        - have a persistent log of applied changes WorldVersion -> [Diff Payload]
        - on startup compare the two logs and apply missing changes
        - in one transaction 
            + write applied changes to the log
            + update the tree store (KI -> KIMeta, CertKey -> MftKey)
        - MFT shortcuts could be written asynchronously (using the queue as already implemented), 
          since we don't want a very big treansaction and to hold all manifest shortcuts in memory.


    * How to deal with expiring objects? Idea:
      - Keep a multimap Instant -> ObjectKey for it
      - Scan expiresAt, expire all the object for the given timestamp
      - Expire object means deleting all payloads found under that object
      - Expired objects stay in the tree and are filtered out by their validity 
        period, they are forever skipped on every scan, but it's easier to do 
        that than actually modify MFT shortcuts
      - Similar applies to object that will be valid in the future (not yet valid). 
        IndexStore an index for them "maturesAt" and scan it periodically the same way as 
        expiresAt. Generate object updates for these objects when they mature.
      - data for expiration: 
        + timestamp -> objectKey
        + objectKey -> AKI (from ObjectMeta)
        + AKI -> CA
        + retire all payloads under CA for the key
      - data for maturing:
        + timestamp -> objectKey
        + objectKey -> AKI
        + run revalidation for (objectKey, AKI), something similar to the case when (objectKey, AKI) is added.
 

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
       - If there was a valid manifest (we know it by looking at cert2mft)
         and now it's not there anymore, we need to delete all payloads under it.   
     - Wrap it in TroubledChild on the manifest shortcut and skip when scanning.

    * How to deal with fencing limits?
     - probably the same as in top-down validation?

    * Is there going to be issues with key rollovers?
     - probably not, there will be a diff on the parent manifest of each
       CA with changed key, so there will be an empty diff of payloads

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

    * How to attribute errors and warnings to scopes? How to attribute metrics to scopes?
      - How many certificate/ROAs/etc. in the repository R?
      - How many certificate/ROAs/etc. per TA?
      Idea:
        - Analyze manifest diffs, increment/decrement stats for each valid/invalid object in the diff.
        - Decrement all stats when a manifest becomes invalid
      
-}

validateUpdates :: Storage s 
                => AppContext s  
                -> TopDownContext 
                -> WorldVersion
                -> [Update] 
                -> IO ()
validateUpdates appContext@AppContext {..} topDownContext worldVersion updates = do 
    db <- readTVarIO database

    let tals = Set.fromList [ tal | TaUpdate tal <- updates ]

    taCerts <- 
        if not $ Set.null tals then do 
            fmap catMaybes 
                $ forConcurrently (Set.toList tals) 
                $ \tal -> do
                -- Expect TAs to be prepared, i.e. TA certificates downloaded and validated.
                -- If it's not the case, it's an error here.    
                let taName = getTaName tal
                z <- roTx db $ \tx -> DB.getTA tx db taName
                case z of
                    Nothing -> do 
                        logError logger [i|TA #{taName} does not exist in the cache.|]
                        -- TODO Complain more by emitting appError/appWarn here?
                        pure Nothing
                        
                    Just (ta, taCert) -> 
                        pure $ Just (ta ^. #taCertKey, taCert)
        else 
            pure []
    

    changes <- newTVarIO []

    let taValidations = flip map taCerts $ \(certKey, _) -> 
            runValidatorT (newScopes' ObjectFocus (coerce certKey)) $ 
                validateCAFrom appContext topDownContext certKey 
                    (\change -> modifyTVar' changes $ \c -> change : c)
                    -- For TA validation, look at all objects
                    (\_ -> True)

    -- TODO They should be unique in any imaginable circumstances
    let newObjects = mconcat [ V.toList o | ObjectUpdate o <- updates ]    
    starts <- findStartCas db newObjects

    let caValidations = flip map (Set.toList $ starts ^. #tops) $ \certKey -> 
            runValidatorT (newScopes' ObjectFocus (coerce certKey)) $ 
                validateCAFrom appContext topDownContext certKey 
                    (\change -> modifyTVar' changes $ \c -> change : c)
                    (\object -> Set.member (coerce object) (starts ^. #paths))        

    let par = fromIntegral $ config ^. #parallelism . #cpuParallelism
    vs <- pooledForConcurrentlyN par (taValidations <> caValidations) id    

    changes_ <- readTVarIO changes
    saveChanges db worldVersion changes_ (mconcat $ map snd vs)
  where    
    saveChanges db worldVersion changes vs = do 
        let DB.IndexStore {..} = db ^. #objectStore . #indexStore
        rwTx db $ \tx -> do             
            M.put tx payloadLog worldVersion changes
            M.put tx vsLog worldVersion vs



{- 
    - Validate top-down with shortcuts and the function to be called when found payloads.
    - Look at the KI -> KIMeta and update it if needed after validation for CA succeeds
    - Generate "Delete Payload" for payloads corresponding to the removed MFT children
-} 
validateCA :: Storage s 
            => AppContext s 
            -> TopDownContext 
            -> CertKey 
            -> (Change Payload -> STM ()) 
            -> ValidatorT IO ()
validateCA appContext topDownContext certKey onPayload = do
    validateCAFrom appContext topDownContext certKey onPayload (const True)

{- 
    Filter will be used to 
      * Pick up only CAs that are on somebody's path to the top
      * Pick up payloads (or their shortcuts) that are in the set up updates
-}   
validateCAFrom :: Storage s 
                => AppContext s 
                -> TopDownContext 
                -> CertKey 
                -> (Change Payload -> STM ()) 
                -> (ObjectKey -> Bool) 
                -> ValidatorT IO ()
validateCAFrom AppContext {..} 
    topDownContext@TopDownContext { allTas = AllTasTopDownContext {..}, .. }
    certKey onPayload relevant = do
    db <- liftIO $ readTVarIO database
    let DB.IndexStore {..} = db ^. #objectStore . #indexStore

    -- TODO ADd processing of limits similar to TopDown.validateCa

    caShort <- liftIO $ roTx db $ \tx -> M.get tx caShortcuts certKey
    case caShort of
        Nothing -> do
            -- no shortcut, full validation needed
            z <- liftIO $ roTx db $ \tx -> DB.getLocatedByKey tx db (coerce certKey)
            case z of 
                Just located@Located { payload = CerRO c, .. } -> do
                    vFocusOn LocationFocus (getURL $ pickLocation locations) $ do
                        increment $ topDownCounters ^. #originalCa
                        -- validateLocationForShortcut (c ^. #key)                    
                        pure ()

                _Z -> do 
                    -- complain and bail out, it's an integrity error
                    pure ()

        Just ca@CaShortcut {..} -> do
            -- validate the shortcut
            vFocusOn ObjectFocus (ca ^. #key) $ do
                pure ()

        Just _q -> do            
            -- integrity error, shortcut is not a CA shortcut, complain and bail out
            pure ()

    -- get MFT shortcut from cache, do the dance with comparing MFT to its shortcut

    -- Calculate MFT diff
    -- * for each added payload call onPayload (Added Payload)
    -- * for each deleted payload call onPayload (Deleted Payload)    
    -- * for each added CA validateCAFrom recursively
    -- * for each deleted CA call traversePayloads and delete payloads

    pure ()


traversePayloads :: Storage s 
                => DB s 
                -> CertKey 
                -> (ObjectKey -> Payload -> STM ()) 
                -> Bool 
                -> IO ()
traversePayloads db certKey onPayload includeExpired = do     
    now <- thisInstant
    let ifNotExpired :: forall a . WithValidityPeriod a => a -> IO () -> IO ()
        ifNotExpired object f  = if includeExpired 
                            then f
                            else when (isWithinValidityPeriod now object) f
    
    let DB.IndexStore {..} = db ^. #objectStore . #indexStore
    ifJustM (roTx cert2mft $ \tx -> M.get tx cert2mft certKey) $ \mftKey -> 
        ifJustM (roTx mftShorts $ \tx -> M.get tx mftShorts mftKey) $ \mftShort -> do
            forM_ (Map.elems (mftShort ^. #nonCrlEntries)) $ \MftEntry {..} -> do
                case child of
                    CaChild s@CaShortcut {..} _ ->                     
                        ifNotExpired s $ traversePayloads db (coerce key) onPayload includeExpired                                            
                    RoaChild r@RoaShortcut {..} _ -> 
                        ifNotExpired r $ atomically $ onPayload key $ VrpsP vrps
                    AspaChild a@AspaShortcut {..} _ -> 
                        ifNotExpired a $ atomically $ onPayload key $ AspaP aspa                        
                    SplChild s@SplShortcut {..} _ -> 
                        ifNotExpired s $ atomically $ onPayload key $ SplP splPayload                    
                    BgpSecChild b@BgpSecShortcut {..} _ -> 
                        ifNotExpired b $ atomically $ onPayload key $ BgpSecP bgpSec
                    GbrChild g@GbrShortcut {..} _ -> 
                        ifNotExpired g $ atomically $ onPayload key $ GbrP gbr
                    _ -> 
                        pure ()                                
              

expireObjects :: Storage s => DB s -> Instant -> IO (Maybe Instant)
expireObjects db now = do
    let DB.IndexStore {..} = db ^. #objectStore . #indexStore
    
    T2 expired nextToExpire <- 
        roTx expiresAt $ \tx -> 
            MM.fold tx expiresAt (\(T2 expired next) t objectKey -> do
                let next' = 
                        if t > now then Just $ maybe t (min t) next                        
                        else next
                pure $! if t <= now 
                            then T2 (Set.insert objectKey expired) next'
                            else T2 expired next') 
                (T2 mempty Nothing)

    -- traversePayloads db (coerce <$> expired) (\_ _ -> pure ()) False        

    pure nextToExpire


data StartCas k = StartCas {
        tops  :: Set.Set k, 
        paths :: Set.Set k
    }
    deriving stock (Show, Generic)

findStartCas :: Storage s
               => DB s 
               -> [AddedObject]
               -> IO (StartCas CertKey)
findStartCas db newObjects = do    
    now <- thisInstant
    akis <- fmap catMaybes $ roTx db $ \tx -> 
                for newObjects $ \(AddedObject objectKey) ->
                    M.get tx objectAKIs objectKey
    findStartCasGen readFromCache (\_ -> isWithinValidityPeriod now) akis
  where
    DB.IndexStore {..} = db ^. #objectStore . #indexStore
    DB.RpkiObjectStore {..} = db ^. #objectStore
    readFromCache (AKI ki) = 
        roTx kiMetas $ \tx -> M.get tx kiMetas ki


findStartCasGen :: (Eq a2, Ord a, HasField' "caCertificate" t2 a, HasField' "aki" t2 a2) 
                => (a2 -> IO (Maybe t2)) 
                -> (a2 -> t2 -> Bool) 
                -> [a2]
                -> IO (StartCas a)
findStartCasGen readFromCache accept akis = do
    cas <- fmap catMaybes $ forM akis $ \aki -> do
                mkiMeta <- readFromCache aki 
                pure $ case mkiMeta of
                    Just kiMeta
                        | accept aki kiMeta -> Just (aki, kiMeta)
                        | otherwise         -> Nothing
                    Nothing -> Nothing

    let startCas = Set.fromList [ kiMeta ^. #caCertificate | (_, kiMeta) <- cas ]    

    (paths, ignored) <- 
        fmap mconcat $ forConcurrently cas $ \ca -> do 
            -- TODO Here we should complain when nothing is found
            findPathUp readFromCache accept ca startCas
            
    pure $! StartCas (startCas `Set.difference` ignored) paths


findPathUp readFromCache accept (ki, kiMeta) startCas = 
    go readFromCache accept (ki, kiMeta) mempty mempty 
  where 
    go readFromCache accept (ki, kiMeta) paths ignored = do
        let aki = kiMeta ^. #aki
        let certKey = kiMeta ^. #caCertificate

        let paths' = Set.insert certKey paths

        -- if it's the root stop                    
        if ki == aki then
            pure (paths', ignored)
        else 
            readFromCache aki >>= \case
                Just parent
                    | accept aki parent -> do 
                        let parentCa = parent ^. #caCertificate
                        -- If parent is one of the CAs we started from, 
                        -- We can ignore the whole path until now, since it is going 
                        -- to be validated anyway starting from `parent`
                        let ignored' = 
                                if parentCa `Set.member` startCas 
                                    then ignored <> paths'
                                    else ignored

                        go readFromCache accept (aki, parent) paths' ignored'

                    | otherwise -> 
                        -- Parent that is not acceptable (expired or not valid yet)
                        -- means no path
                        pure (mempty, paths')

                Nothing -> 
                    -- No parent, again it means no path, 
                    -- ignore the whole path
                    pure (mempty, paths')


-- toObjectUpdates :: (MonadIO m, Storage s) => Tx s mode -> DB s -> [Update] -> m [AddedObject]
-- toObjectUpdates tx db updates = liftIO $ 
--     fmap (Set.toList . Set.unions) $ forM updates $ \case
--         ObjectUpdate o     -> pure $ Set.singleton $ AddedObject o
--         TaUpdate taName -> do
--             mta <- DB.getTA tx db taName
--             pure $ case mta of
--                 Nothing         -> mempty
--                 Just (ta, _cert) ->
--                     let certKey = ta ^. #taCertKey
--                     in Set.singleton $ AddedObject (coerce certKey)


    