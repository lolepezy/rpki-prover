{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DerivingStrategies         #-}

module RPKI.TopDown where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader

import Data.Bifunctor

import           GHC.Generics

import qualified Control.Concurrent.STM.TBQueue   as Q
import           Data.Bifunctor
import           Data.Has
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NonEmpty
import           Data.Foldable
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (catMaybes, fromMaybe)
import           Data.Either                      (partitionEithers)
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text

import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Logging
import           RPKI.Parallel                    (parallel)
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.Resources.Types
import           RPKI.RRDP.Update
import           RPKI.Rsync
import           RPKI.Store.Base.Storage
import           RPKI.Store.Data
import           RPKI.Store.Stores
import           RPKI.Store.Repository
import           RPKI.TAL
import           RPKI.Util                        (fmtEx)
import           RPKI.Validation.ObjectValidation


data TACertValidationResult = 
        SameTACert !CerObject !(NonEmpty Repository) |
        UpdatedTACert !CerObject !(NonEmpty Repository)
    deriving (Show, Eq, Generic)

storageError :: SomeException -> AppError
storageError = StorageE . StorageError . fmtEx


-- Auxiliarry structure used in top-down validation
data TopDownContext = TopDownContext {    
    verifiedResources :: Maybe (VerifiedRS PrefixesAndAsns),
    resultQueue       :: TBQueue (Maybe VResult),    
    publicationPoints :: TVar PublicationPoints,
    ppWaitingList     :: TVar (Map PublicationPoint (Set Hash)),
    taName            :: TaName, 
    now               :: Now
}

emptyTopDownContext :: MonadIO m => TaName -> PublicationPoints -> m TopDownContext
emptyTopDownContext taName publicationPoints = liftIO $ do 
    now <- thisMoment
    atomically $ TopDownContext Nothing <$> 
        (newTBQueue 100) <*>
        (newTVar publicationPoints) <*>
        (newTVar Map.empty) <*>
        pure taName <*> 
        pure now   


-- | Initial bootstrap of the TA: do everything needed to start up the validator, 
-- | * download and parse TA certificate
-- | * fetch the repositories
bootstrapTA :: (Has AppContext env, Storage s) => 
                env -> TAL -> DB s -> IO ()
bootstrapTA env tal database = do
    (r, v) <- runValidatorT taContext $ do         
        next <- validateTAFromTAL env tal database 
        case next of
            SameTACert taCert repos     -> fetchAndValidate taCert repos
            UpdatedTACert newCert repos -> fetchAndValidate newCert repos
    
    pure ()
    where
        fetchAndValidate taCert repos = do
            -- TODO do it in parallel of course            
            statuses <- parallel (parallelism config) repos $ \repo -> do 
                logDebugM logger [i|Bootstrap, fetching #{repo} |]
                fetchRepository appContext database repo                
            
            -- update repositiory statuses and save them            
            let flatStatuses = flip NonEmpty.map statuses $ \case 
                    FetchFailure r s _ -> (r, s)
                    FetchSuccess r s _ -> (r, s)            

            case partitionFailedSuccess (NonEmpty.toList statuses) of 
                ([], _) -> do
                    pubPoints <- roAppTxEx database storageError $ \tx -> 
                            getTaPublicationPoints tx (repositoryStore database) taName'                    

                    topDownContext <- emptyTopDownContext taName' pubPoints

                    fromTry (UnspecifiedE . fmtEx) $             
                        validateCA env taCertURI database topDownContext taCert                    

                    -- after all is done, save publiocation points to the database                                        
                    let updatedPPs = updateStatuses pubPoints flatStatuses
                    liftIO $ atomically $ writeTVar (publicationPoints topDownContext) updatedPPs                

                    -- save it to the database
                    let changeSet' = changeSet pubPoints updatedPPs
                    rwAppTxEx database storageError $ \tx -> 
                        applyChangeSet tx (repositoryStore database) changeSet' taName'

                (broken, _) -> do
                    let brokenUrls = map (\(r, _, _) -> repositoryURI r) broken
                    logErrorM logger [i|Will not proceed, repositories '#{brokenUrls}' failed to download|]
            
        taCertURI = vContext $ NonEmpty.head $ certLocations tal
        taName' = getTaName tal
        appContext@AppContext {..} = getter env
        taContext = vContext $ getTaURI tal


-- | Valiidate TA starting from the TAL.
validateTAFromTAL :: (Has AppContext env, Has VContext vc, Storage s) => 
                        env -> TAL -> DB s -> ValidatorT vc IO TACertValidationResult
validateTAFromTAL env tal DB {..} = do    
    (uri', ro) <- fetchTACertificate appContext tal
    newCert    <- vHoist $ validateTACert tal uri' ro      
    rwAppTxEx taStore storageError
        $ \tx -> do
            r <- getTA tx taStore taName'
            case r of
                Nothing -> do
                    -- it's a new TA, store it and trigger all the other actions
                    let c = cwsX509certificate $ getCertWithSignature $ newCert
                    logInfoM logger [i| Storing new #{taName'}, 
                        getRrdpNotifyUri newCert = #{getRrdpNotifyUri c}, 
                        getRepositoryUri newCert = #{getRepositoryUri c}|]
                                                            
                    storeTaCert tx newCert

                Just (STA {..}) ->
                    if (getSerial taCert /= getSerial newCert) 
                        then do            
                            logInfoM logger [i| Updating TA certificate for #{taName'} |]                            
                            storeTaCert tx newCert
                        else 
                            pure $ SameTACert taCert initialRepositories

    where        
        storeTaCert tx newCert = 
            case createRepositoriesFromTAL tal newCert of
                Left e      -> appError $ ValidationE e
                Right repos -> do 
                    putTA tx taStore (STA tal newCert repos)
                    pure $ UpdatedTACert newCert repos

        taName' = getTaName tal
        appContext@AppContext {..} = getter env
     

data FetchResult = 
    FetchSuccess !Repository !RepositoryStatus !Validations | 
    FetchFailure !Repository !RepositoryStatus !Validations
    deriving stock (Show, Eq, Generic)

-- | Download repository
fetchRepository :: (MonadIO m, Storage s) => 
                AppContext -> DB s -> Repository -> m FetchResult
fetchRepository appContext@AppContext {..} DB {..} repo = liftIO $ do
    Now now <- thisMoment
    (r, v) <- runValidatorT (vContext $ repositoryURI repo) $ 
        case repo of
            RsyncR r -> 
                first RsyncR <$> updateObjectForRsyncRepository appContext r objectStore                                
            RrdpR r -> 
                first RrdpR <$> updateObjectForRrdpRepository appContext r objectStore                    

    case r of
        Left e -> do                        
            logErrorM logger [i|Fetching repository #{repositoryURI repo} failed: #{e} |]
            let repoContext' = vContext $ repositoryURI repo
            pure $ FetchFailure repo (FailedAt now) (mError repoContext' e <> v)
        Right v' ->
            pure $ FetchSuccess repo (FetchedAt now) (snd v' <> v)


type RepoTriple = (Repository, RepositoryStatus, Validations)

partitionFailedSuccess :: [FetchResult] -> ([RepoTriple], [RepoTriple])
partitionFailedSuccess = go
    where
        go [] = ([], [])
        go (FetchSuccess r rs v : frs) = let (fs, ss) = go frs in (fs, (r, rs, v) : ss)
        go (FetchFailure r rs v : frs) = let (fs, ss) = go frs in ((r, rs, v) : fs, ss)



validateCA :: (Has AppContext env, Storage s) =>
            env -> VContext -> DB s -> TopDownContext -> CerObject -> IO ()
validateCA env vContext' database@DB {..} topDownContext@TopDownContext{..} certificate = do    
    let appContext@AppContext {..} = getter env        

    pubPoints <- readTVarIO publicationPoints

    let validateAll = do
            treeValidations <- runValidatorT vContext' $ 
                    validateTree env database certificate topDownContext
            queueVResult topDownContext $ toValidations vContext' treeValidations

    let finaliseQueue = atomically $ Q.writeTBQueue resultQueue Nothing

    -- Write validation results in a separate thread to avoid blocking on the 
    -- database with writing transactions during the validation process 
    void $ concurrently 
        (validateAll `finally` finaliseQueue)
        (writeVResults logger topDownContext resultStore)        

    -- top-down validation should have updates the set of publication points,
    -- get the updated set
    pubPoints' <- readTVarIO publicationPoints
    
    let pubDiff = changeSet pubPoints' pubPoints
    rwTx database $ \tx -> applyChangeSet tx repositoryStore pubDiff taName

    let (_, rootToPps) = repositoryHierarchy pubPoints'
        
    let newRepositories = (filter ((New ==) . repositoryStatus) $ Map.elems $ repositories pubPoints')    
    logDebugM logger [i|newRepositories = #{newRepositories}|]

    -- for all new repositories
    -- parallel (parallelism config) 
    -- mapM
    --     (fetchAndValidateWaitingList appContext rootToPps) 
    --     newRepositories
        
    pure ()
    where         

        -- fetchAndValidateWaitingList1 appContext rootToPps newRepositories = do
        --     statuses <- forM newRepositories $ \repo -> do 
        --         logDebugM logger [i|Fetching #{repo} |]
        --         fetchRepository appContext database repo
            
        --     -- update repositiory statuses and save them            
        --     let flatStatuses = flip NonEmpty.map statuses $ \case 
        --             FetchFailure r s _ -> (r, s)
        --             FetchSuccess r s _ -> (r, s)                

        --     -- TODO create change set directly from the 'flatStatuses'
        --     -- let updatedRepositories = updateStatuses existingRepositories flatStatuses
        --     -- let changeSet' = changeSetFromMap existingRepositories updatedRepositories
        --     -- rwAppTxEx database storageError $ \tx -> 
        --     --     applyChangeSet tx repositoryStore changeSet' taName

        --     pure ()    


        -- fetchAndValidateWaitingList appContext rootToPps repo = do 
        --     let repoContext' = vContext $ repositoryURI repo            
        --     (result, validations) <- runValidatorT repoContext' $ do 
        --         r <- fetchRepository appContext database repo                
        --         case r of
        --             Right validations ->
        --                 fromTry storageError $ do 
        --                     let fetchedPPs = fromMaybe Set.empty $ Map.lookup repo rootToPps
        --                     waitingListPerPP <- readTVarIO ppWaitingList

        --                     let waitingHashes  = fromMaybe Set.empty $ fold $ 
        --                             Set.map (\pp -> Map.lookup pp waitingListPerPP) fetchedPPs

        --                     -- TODO parallel here as well
        --                     forM_ waitingHashes $ \h -> do                
        --                         o <- roTx database $ \tx -> getByHash tx objectStore h
        --                         case o of 
        --                             Just (CerRO c) -> do
        --                                 let certVContext = vContext $ NonEmpty.head $ getLocations c
        --                                 validateCA appContext certVContext database topDownContext c
        --                             ro -> do 
        --                                 logErrorM (logger appContext) $ 
        --                                     [i| Something is really wrong with the hash #{h} in waiting list, got #{ro}|]
        --             Left e -> 
        --                 -- if it failed, the error will be in the 'validations'
        --                 pure ()        

        --         pure repo

        --     queueVResult topDownContext $ 
        --         validations <> either (mError repoContext') snd result
        
    

-- | Do top-down validation starting from the given certificate
validateTree :: (Has AppContext env, 
                Has VContext vc, 
                Storage s) =>
                env ->
                DB s ->
                CerObject ->
                TopDownContext ->
                ValidatorT vc IO ()
validateTree env database certificate topDownContext = do      
    let AppContext {..} = getter env    

    (ppStatus, publicationPoint) <- fromEitherM $ fmap (first ValidationE) $ 
            atomically $ do 
                pps <- readTVar $ publicationPoints topDownContext        
                case publicationPointsFromCertObject certificate of
                    Left e -> pure $ Left e
                    Right (_, pp) -> do
                        writeTVar (publicationPoints topDownContext) publicationPoints'
                        pure $ Right (ppStatus, pp)
                        where 
                            (publicationPoints', ppStatus) = updatePublicationPoints pps pp                        

    case ppStatus of
        New ->
            -- Nothing to do here, the publication point of this certificate 
            -- is not fetched yet. We have to remember this certificate and 
            -- continue from this point where the publiction point is fetched.
            certificate `addToWaitingListOf` publicationPoint

        -- TODO Re-check it again, maybe all this fiddling 
        -- doesn't make much sense

        -- It it's failed, don't continue, only complain that we can't go down, 
        -- because the pulication point is broken
        --
        -- If it's checked too long ago, do the same as with the new one, i.e.
        -- Add certificate to the waiting list
        FetchedAt at 
            | notTooLongAgo validationConfig at (now topDownContext) -> 
                validateCertAndDown config 
            | otherwise  -> 
                certificate `addToWaitingListOf` publicationPoint

        FailedAt at 
            | notTooLongAgo validationConfig at (now topDownContext) -> 
                    appError $ ValidationE $ PublicationPointIsNotAvailable $ publicationPointURI publicationPoint
            | otherwise -> certificate `addToWaitingListOf` publicationPoint

    where
        addToWaitingListOf :: CerObject -> PublicationPoint -> ValidatorT vc IO ()
        addToWaitingListOf cert publicationPoint = lift3 $ atomically $ 
            modifyTVar (ppWaitingList topDownContext) $ 
                ( <> (Map.singleton publicationPoint $ Set.singleton $ getHash cert))
             
        validateCertAndDown config = do
            vContext' :: VContext <- asks getter
            let (childrenAki, locations) = (toAKI $ getSKI certificate, getLocations certificate)        

            mft <- findMft childrenAki locations

            (_, crlHash) <- case findCrlOnMft mft of 
                []    -> vError $ NoCRLOnMFT childrenAki locations
                [crl] -> pure crl
                _     -> vError $ MoreThanOneCRLOnMFT childrenAki locations

            let objectStore' = objectStore database
            crlObject <- lift3 $ roTx objectStore' $ \tx -> getByHash tx objectStore' crlHash
            case crlObject of 
                Nothing          -> vError $ NoCRLExists childrenAki locations    
                Just (CrlRO crl) -> do      
                    validCrl <- vHoist $ do          
                        crl' <- validateCrl (now topDownContext) crl certificate
                        validateMft (now topDownContext) mft certificate crl'
                        pure crl'
                        
                    let childrenHashes = map snd $ mftEntries $ getCMSContent $ extract mft                       
                    mftProblems <- parallel (parallelism config) childrenHashes $ \h -> do            
                        ro <- roAppTx objectStore' $ \tx -> getByHash tx objectStore' h
                        case ro of 
                            Nothing  -> pure $ Just $ ManifestEntryDontExist h
                            Just ro' -> do                         
                                liftIO $ validateChild vContext' validCrl ro'
                                pure Nothing

                    -- TODO Here we should act depending on how strict we want to be,  
                    -- Interrupt the whole thing or just go with a warning            
                    case mftProblems of
                        [] -> pure ()
                        _  -> mapM_ vWarn $ catMaybes mftProblems

                Just _  -> vError $ CRLHashPointsToAnotherObject crlHash locations   

        findMft :: Has VContext env => 
                    AKI -> Locations -> ValidatorT env IO MftObject
        findMft childrenAki locations = do
            mft' <- lift3 $ roTx (objectStore database) $ \tx -> 
                findLatestMftByAKI tx (objectStore database) childrenAki
            case mft' of
                Nothing  -> vError $ NoMFT childrenAki locations
                Just mft -> pure mft

        -- TODO Is there a more reliable way to find it? Compare it with SIA?
        findCrlOnMft mft = filter (\(name, _) -> ".crl" `Text.isSuffixOf` name) $ 
            mftEntries $ getCMSContent $ extract mft

        validateChild :: VContext -> Validated CrlObject -> RpkiObject -> IO () 
        validateChild parentContext validCrl ro = 
            case ro of
                CerRO childCert -> do 
                    result <- runValidatorT childContext $ do
                            childVerifiedResources <- vHoist $ do                 
                                validateResourceCert (now topDownContext) childCert certificate validCrl                
                                validateResources (verifiedResources topDownContext) childCert certificate 
                            validateTree env database childCert 
                                topDownContext { verifiedResources = Just childVerifiedResources }
                    queueVResult topDownContext $ toValidations childContext result

                RoaRO roa -> queueVResult topDownContext $ toValidations childContext $ 
                                runPureValidator childContext $ 
                                    void $ validateRoa (now topDownContext) roa certificate validCrl
                GbrRO gbr -> queueVResult topDownContext $ toValidations childContext $ 
                                runPureValidator childContext $ 
                                    void $ validateGbr (now topDownContext) gbr certificate validCrl
                -- TODO Anything else?
                _ -> pure ()
            where
                childContext = childVContext parentContext childLocation 
                childLocation = NonEmpty.head $ getLocations ro


-- | Put validation result into a queue for writing
queueVResult :: TopDownContext -> Validations -> IO ()
queueVResult topDownContext validations =    
    case validations of
        Validations validationsMap
            | emptyValidations validations -> pure ()
            | otherwise -> void $ flip Map.traverseWithKey validationsMap $ 
                    \vc' problems -> atomically $ Q.writeTBQueue queue $ 
                                        Just $ VResult (Set.toList problems) vc'
    where
        queue = resultQueue topDownContext

-- | Get validation result from the queue and save it to the DB
writeVResults :: (Storage s) => AppLogger -> TopDownContext -> VResultStore s -> IO ()
writeVResults logger topDownContext resultStore =
    withQueue (resultQueue topDownContext) $ \vr -> do
        logDebug_ logger [i|VResult: #{vr}.|]
        rwTx resultStore $ \tx -> putVResult tx resultStore vr


-- TODO Optimise so that it reads the queue in big chunks
withQueue :: TBQueue (Maybe a) -> (a -> IO ()) -> IO ()
withQueue queue f = do
    z <- atomically $ Q.readTBQueue queue    
    case z of
        Nothing -> pure ()
        Just s  -> f s >> withQueue queue f

toValidations :: VContext -> (Either AppError a, Validations) -> Validations
toValidations vc (Left e, vs) = vs <> mError vc e
toValidations _ (Right _, vs) = vs


-- | Fetch TA certificate based on TAL location(s)
fetchTACertificate :: Has VContext vc => 
                    AppContext -> TAL -> ValidatorT vc IO (URI, RpkiObject)
fetchTACertificate appContext tal = 
    go $ NonEmpty.toList $ certLocations tal
    where
        go []         = throwError $ TAL_E $ TALError "No certificate location could be fetched."
        go (u : uris) = ((u,) <$> rsyncFile appContext u) `catchError` goToNext 
            where 
                goToNext e = do            
                    let message = [i| Failed to fetch #{u}: #{e}|]
                    logErrorM (logger appContext) message
                    validatorWarning $ VWarning e
                    go uris



-- Utilities to have storage transaction in ValidatorT monad.
roAppTx :: (Storage s, WithStorage s ws) => 
            ws -> (Tx s 'RO -> ValidatorT env IO a) -> ValidatorT env IO a 
roAppTx s f = appTx s f roTx    

rwAppTx :: (Storage s, WithStorage s ws) => 
            ws -> (forall mode . Tx s mode -> ValidatorT env IO a) -> ValidatorT env IO a
rwAppTx s f = appTx s f rwTx


appTx :: (Storage s, WithStorage s ws) => 
        ws -> (Tx s mode -> ValidatorT env IO a) -> 
        (ws -> (Tx s mode -> IO (Either AppError a, Validations))
            -> IO (Either AppError a, Validations)) -> 
        ValidatorT env IO a
appTx s f txF = do
    env <- ask
    validatorT $ txF s $ runValidatorT env . f


roAppTxEx :: (Storage s, WithStorage s ws, Exception exc) => 
            ws -> 
            (exc -> AppError) -> 
            (Tx s 'RO -> ValidatorT env IO a) -> 
            ValidatorT env IO a 
roAppTxEx ws err f = appTxEx ws err f roTx    

rwAppTxEx :: (Storage s, WithStorage s ws, Exception exc) => 
            ws -> (exc -> AppError) -> 
            (Tx s 'RW -> ValidatorT env IO a) -> ValidatorT env IO a
rwAppTxEx s err f = appTxEx s err f rwTx

appTxEx :: (Storage s, WithStorage s ws, Exception exc) => 
            ws -> (exc -> AppError) -> 
            (Tx s mode -> ValidatorT env IO a) -> 
            (s -> (Tx s mode -> IO (Either AppError a, Validations))
               -> IO (Either AppError a, Validations)) -> 
            ValidatorT env IO a
appTxEx ws err f txF = do
    env <- ask
    -- TODO Make it less ugly and complicated
    t <- lift3 $ try $ txF (storage ws) $ runValidatorT env . f
    validatorT $ pure $ either ((, mempty) . Left . err) id t

