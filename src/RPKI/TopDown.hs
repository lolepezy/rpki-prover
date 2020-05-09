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

import Control.Lens

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
import           RPKI.Parallel
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
    verifiedResources           :: Maybe (VerifiedRS PrefixesAndAsns),
    resultQueue                 :: TBQueue (Maybe VResult),    
    publicationPoints           :: TVar PublicationPoints,
    discoveredPublicationPoints :: TVar PublicationPoints,
    ppWaitingList               :: TVar (Map PublicationPoint (Set Hash)),
    taName                      :: TaName, 
    now                         :: Now,
    parallelismDegree           :: Parallelism
}

emptyTopDownContext :: MonadIO m => AppContext -> TaName -> PublicationPoints -> m TopDownContext
emptyTopDownContext AppContext {..} taName publicationPoints = liftIO $ do 
    now <- thisMoment
    atomically $ TopDownContext Nothing <$> 
        newTBQueue 100 <*>
        newTVar publicationPoints <*>
        newTVar emptyPublicationPoints <*>
        newTVar Map.empty <*>
        pure taName <*> 
        pure now <*>
        dynamicPara (parallelism config)


-- | Initial bootstrap of the TA: do everything needed to start up the validator, 
-- | * download and parse TA certificate
-- | * fetch the repositories
bootstrapTA :: (Has AppContext env, Storage s) => 
                env -> TAL -> DB s -> IO (Either AppError (), Validations)
bootstrapTA env tal database = 
    runValidatorT taContext $ do         
        next <- validateTAFromTAL env tal database 
        case next of
            SameTACert taCert repos     -> fetchAndValidate taCert repos
            UpdatedTACert newCert repos -> fetchAndValidate newCert repos
        
    where
        fetchAndValidate taCert repos = do            
            parallelism' <- liftIO $ dynamicParaIO $ parallelism config
            statuses <- parallel parallelism' repos $ \repo -> do 
                logDebugM logger [i|Bootstrap, fetching #{repo} |]
                fetchRepository appContext database repo                
                    
            case partitionFailedSuccess (NonEmpty.toList statuses) of 
                ([], _) -> do
                    storedPubPoints <- roAppTxEx database storageError $ \tx -> 
                            getTaPublicationPoints tx (repositoryStore database) taName'                    

                    logDebugM logger [i| storedPubPoints = #{storedPubPoints} |]

                    let flattenedStatuses = flip NonEmpty.map statuses $ \case 
                            FetchFailure r s _ -> (r, s)
                            FetchSuccess r s _ -> (r, s)            

                    -- use publication points taken from the DB and updated with the 
                    -- the statuses of the fetch that we just performed
                    let fetchUpdatedPPs = updateStatuses storedPubPoints flattenedStatuses
                    topDownContext <- emptyTopDownContext appContext taName' fetchUpdatedPPs

                    fromTry (UnspecifiedE . fmtEx) $             
                        validateCA env taCertURI database topDownContext taCert                    

                    -- get publication points from the topDownContext and save it to the database
                    pubPointAfterTopDown <- liftIO $ readTVarIO (publicationPoints topDownContext)
                    let changeSet' = changeSet fetchUpdatedPPs pubPointAfterTopDown

                    logDebugM logger [i| Saving pubPointAfterTopDown = #{pubPointAfterTopDown}, 
                                         changeSet' = #{changeSet'} |]

                    rwAppTxEx database storageError $ \tx -> 
                        applyChangeSet tx (repositoryStore database) changeSet' taName'                    

                (broken, _) -> do
                    let brokenUrls = map (repositoryURI . (^. _1)) broken
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

    let validateAll = do
            treeValidations <- runValidatorT vContext' $ 
                    validateTree env database certificate topDownContext
            queueVResult topDownContext $ toValidations vContext' treeValidations    

    -- Write validation results in a separate thread to avoid blocking on the 
    -- database with writing transactions during the validation process 
    void $ concurrently 
        (validateAll `finally` atomically (Q.writeTBQueue resultQueue Nothing))
        (writeVResults logger topDownContext resultStore)        


    --------------------------------------------------------------------------------
    -- TODO Use PPs returned by validateTree instead of re-reading the global ones
    --------------------------------------------------------------------------------

    -- top-down validation should have updates the set of publication points,
    -- get the updated set
    pubPointsAfter <- readTVarIO publicationPoints
    
    let (_, rootToPps) = repositoryHierarchy pubPointsAfter
        
    let newRepositories = (filter ((New ==) . repositoryStatus) $ Map.elems $ repositories pubPointsAfter)    
    logDebugM logger [i|newRepositories = #{newRepositories}, urls = #{map repositoryURI newRepositories}|]

    -- for all new repositories, drill down recursively
    void $ parallel (fixedPara (parallelism config)) newRepositories $ \repo -> do 
        validations <- fetchAndValidateWaitingList appContext rootToPps repo
        queueVResult topDownContext validations
        
    where

        fetchAndValidateWaitingList appContext@AppContext {..} rootToPps repo = do
            logDebugM logger [i|Fetching #{repo} |]
            result <- fetchRepository appContext database repo                                
            let statusUpdate = case result of
                    FetchFailure r s _ -> (r, s)
                    FetchSuccess r s _ -> (r, s)                

            atomically $ modifyTVar' publicationPoints $ \pubPoints -> 
                updateStatuses pubPoints [statusUpdate]

            case result of
                FetchFailure _ _ v -> pure v 
                FetchSuccess _ _ v -> do
                    let fetchedPPs = fromMaybe Set.empty $ Map.lookup repo rootToPps
                    waitingListPerPP <- readTVarIO ppWaitingList

                    let waitingHashesForThesePPs  = fromMaybe Set.empty $ fold $ 
                            Set.map (`Map.lookup` waitingListPerPP) fetchedPPs

                    parallel parallelismDegree (Set.toList waitingHashesForThesePPs) $ \hash -> do
                            o <- roTx database $ \tx -> getByHash tx objectStore hash
                            case o of 
                                Just (CerRO c) -> do
                                    let certVContext = vContext $ NonEmpty.head $ getLocations c
                                    validateCA appContext certVContext database topDownContext c
                                ro ->
                                    logErrorM logger
                                        [i| Something is really wrong with the hash #{hash} in waiting list, got #{ro}|]
                    pure v
        
    

-- | Do top-down validation starting from the given certificate
-- Returns the discovered publication points that are not registered 
-- in the top-down context yet.
validateTree :: (Has AppContext env, 
                Has VContext vc, 
                Storage s) =>
                env ->
                DB s ->
                CerObject ->
                TopDownContext ->
                ValidatorT vc IO PublicationPoints
validateTree env database certificate topDownContext = do      
    let AppContext {..} = getter env    

    globalPPs <- liftIO $ readTVarIO $ publicationPoints topDownContext
    case publicationPointsFromCertObject certificate of
        Left e                      -> appError $ ValidationE e
        Right (u, publicationPoint) ->
            case findPublicationPointStatus u globalPPs of 
                Nothing -> do 
                    -- It is a completely new piublication point, it is not fetched yet 
                    -- and it hasn't been seen. Nothing to do here, just add this certificate 
                    -- to the waitiung list for this PP.
                    certificate `addToWaitingListOf` publicationPoint
                    -- return the PP that has just been discovered
                    pure $ updatePublicationPoints emptyPublicationPoints publicationPoint

                Just New -> stopHere publicationPoint

                Just (FetchedAt time) 
                    | notTooLongAgo validationConfig time (now topDownContext) -> do
                        validateThisCertAndGoDown config                         
                    | otherwise  -> stopHere publicationPoint

                Just (FailedAt time)
                    | notTooLongAgo validationConfig time (now topDownContext) -> 
                            appError $ ValidationE $ PublicationPointIsNotAvailable $ publicationPointURI publicationPoint
                    | otherwise -> stopHere publicationPoint

    where
        stopHere publicationPoint = do 
            certificate `addToWaitingListOf` publicationPoint
            pure emptyPublicationPoints                

        addToWaitingListOf :: CerObject -> PublicationPoint -> ValidatorT vc IO ()
        addToWaitingListOf cert publicationPoint = liftIO $ atomically $ 
            modifyTVar (ppWaitingList topDownContext)
                (<> Map.singleton publicationPoint (Set.singleton $ getHash cert))
             
        validateThisCertAndGoDown config = do
            vContext' :: VContext <- asks getter
            let (childrenAki, locations) = (toAKI $ getSKI certificate, getLocations certificate)        

            mft <- findMft childrenAki locations

            (_, crlHash) <- case findCrlOnMft mft of 
                []    -> vError $ NoCRLOnMFT childrenAki locations
                [crl] -> pure crl
                _     -> vError $ MoreThanOneCRLOnMFT childrenAki locations

            let objectStore' = objectStore database
            crlObject <- liftIO $ roTx objectStore' $ \tx -> getByHash tx objectStore' crlHash
            case crlObject of 
                Nothing          -> vError $ NoCRLExists childrenAki locations    
                Just (CrlRO crl) -> do      
                    validCrl <- vHoist $ do          
                        crl' <- validateCrl (now topDownContext) crl certificate
                        validateMft (now topDownContext) mft certificate crl'
                        pure crl'
                        
                    let childrenHashes = map snd $ mftEntries $ getCMSContent $ extract mft                       
                    mftProblems <- parallel (parallelismDegree topDownContext) childrenHashes $ \h -> do            
                        ro <- roAppTx objectStore' $ \tx -> getByHash tx objectStore' h
                        case ro of 
                            Nothing  -> pure $ Left $ ManifestEntryDontExist h
                            Just ro' -> Right <$> liftIO (validateChild vContext' validCrl ro')

                    -- TODO Here we should act depending on how strict we want to be,  
                    -- Interrupt the whole thing or just go with a warning            
                    case mftProblems of
                        [] -> pure emptyPublicationPoints
                        _  -> do 
                            let (broken, pps) = partitionEithers mftProblems
                            mapM_ vWarn broken
                            pure $ mconcat pps 

                Just _  -> vError $ CRLHashPointsToAnotherObject crlHash locations   

        findMft :: Has VContext env => 
                    AKI -> Locations -> ValidatorT env IO MftObject
        findMft childrenAki locations = do
            mft' <- liftIO $ roTx (objectStore database) $ \tx -> 
                findLatestMftByAKI tx (objectStore database) childrenAki
            case mft' of
                Nothing  -> vError $ NoMFT childrenAki locations
                Just mft -> pure mft

        -- TODO Is there a more reliable way to find it? Compare it with SIA?
        findCrlOnMft mft = filter (\(name, _) -> ".crl" `Text.isSuffixOf` name) $ 
            mftEntries $ getCMSContent $ extract mft

        validateChild :: VContext -> Validated CrlObject -> RpkiObject -> IO PublicationPoints
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
                    case result of
                        (Left _, _)  -> pure emptyPublicationPoints
                        (Right pps, _) -> pure pps

                RoaRO roa -> withEmptyPPs $ 
                                queueVResult topDownContext $ toValidations childContext $ 
                                    runPureValidator childContext $ 
                                        void $ validateRoa (now topDownContext) roa certificate validCrl
                GbrRO gbr -> withEmptyPPs $ 
                                queueVResult topDownContext $ toValidations childContext $ 
                                    runPureValidator childContext $ 
                                        void $ validateGbr (now topDownContext) gbr certificate validCrl
                -- TODO Anything else?
                _ -> withEmptyPPs $ pure ()
            where
                childContext = childVContext parentContext childLocation 
                childLocation = NonEmpty.head $ getLocations ro

                withEmptyPPs f = f >> pure emptyPublicationPoints


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

