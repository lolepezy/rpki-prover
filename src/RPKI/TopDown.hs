{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

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
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (catMaybes, fromMaybe)
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


-- | Initial bootstrap of the TA: do everything needed to start up the validator, 
-- | * download and parse TA certificate
-- | * fetch the repositories
bootstrapTA :: (Has AppContext env, Storage s) => 
                env -> TAL -> DB s -> ValidatorT vc IO ()
bootstrapTA env tal database = do
    next <- validateTAFromTAL env tal database 
    case next of
        SameTACert taCert repos     -> fetchAndValidate taCert repos
        UpdatedTACert newCert repos -> fetchAndValidate newCert repos
    where
        fetchAndValidate taCert repos = do 
            -- TODO do it in parallel of course
            forM_ repos $ fetchRepository appContext database
            -- do all the recursive fetching and validaiton
            fromTry (UnspecifiedE . fmtEx) $ 
                validateCA env taCertURI database taName' taCert

        taCertURI = vContext $ NonEmpty.head $ certLocations tal
        taName' = getTaName tal
        appContext@AppContext {..} = getter env


-- | Valiidate TA starting from the TAL.
validateTAFromTAL :: (Has AppContext env, Has VContext vc, Storage s) => 
                        env -> TAL -> DB s -> ValidatorT vc IO TACertValidationResult
validateTAFromTAL env tal DB {..} = do    
    (uri', ro) <- fetchTACertificate appContext tal
    newCert    <- vHoist $ validateTACert tal uri' ro      
    rwAppTxEx 
        taStore  
        (StorageE . StorageError . fmtEx)
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
                Left e             -> appError $ ValidationE e
                Right repositories -> do 
                    putTA tx taStore (STA tal newCert repositories)
                    pure $ UpdatedTACert newCert repositories

        taName' = getTaName tal
        appContext@AppContext {..} = getter env
     

-- | Download repository and update it's status
fetchRepository :: (Storage s, Has VContext vc) => 
                    AppContext -> DB s -> Repository -> ValidatorT vc IO (Repository, Validations)
fetchRepository appContext@AppContext {..} DB {..} r = do
    Now now <- lift3 thisMoment
    vc  <- asks getter
    case r of
        RsyncR repo -> do
                    (repo', validations) <- updateObjectForRsyncRepository appContext repo objectStore
                    pure (RsyncR (repo' { status = FetchedAt now }), validations)
            `catchError` 
                \e -> 
                    pure (RsyncR (repo { status = FailedAt now }), mError vc e)
                            
        RrdpR repo -> do
                (repo', validations) <- updateObjectForRrdpRepository appContext repo objectStore
                pure (RrdpR repo' { status = FetchedAt now }, validations)
            `catchError` 
                \e -> 
                    pure (RrdpR repo { status = FailedAt now }, mError vc e)


            


-- Auxiliarry structure used in top-down validation
data TopDownContext = TopDownContext {    
    verifiedResources :: Maybe (VerifiedRS PrefixesAndAsns),
    resultQueue       :: TBQueue (Maybe VResult),    
    publicationPoints :: TVar PublicationPoints,
    ppWaitingList     :: TVar (Map PublicationPoint (Set Hash)),
    taName            :: TaName, 
    now               :: Now
}

emptyTopDownContext :: TaName -> PublicationPoints -> IO TopDownContext
emptyTopDownContext taName publicationPoints = do 
    now <- thisMoment
    atomically $ TopDownContext Nothing <$> 
        (newTBQueue 100) <*>
        (newTVar publicationPoints) <*>
        (newTVar Map.empty) <*>
        pure taName <*> 
        pure now   


validateCA :: (Has AppContext env, Storage s) =>
            env -> VContext -> DB s -> TaName -> CerObject -> IO ()
validateCA env vContext' database@DB {..} taName certificate = do    
    let appContext@AppContext {..} = getter env    

    pubPoints <- roTx database $ \tx -> getRepositoriesForTA tx repositoryStore taName
    topDownContext <- emptyTopDownContext taName pubPoints

    let validateAll = do
            treeValidations <- runValidatorT vContext' $ 
                    validateTree env database certificate topDownContext
            queueVResult topDownContext $ toValidations vContext' treeValidations

    let finaliseQueue = atomically $ Q.writeTBQueue (resultQueue topDownContext) Nothing

    -- Write validation results in a separate thread to avoid blocking on the 
    -- database with writing transactions during the validation process 
    void $ concurrently 
        (validateAll `finally` finaliseQueue)
        (writeVResults logger topDownContext resultStore)        

    -- top-down validation should have updates the set of publication points
    pubPoints' <- atomically $ readTVar $ publicationPoints topDownContext

    -- calculate diff that will be stored in the database
    let pubDiff = changeSet pubPoints' pubPoints
    rwTx database $ \tx -> applyChangeSet tx repositoryStore pubDiff taName

    let (_, rootToPps) = repositoryHierarchy pubPoints'
    let newRepositories = filter ((New ==) . repositoryStatus) $ repositories pubPoints'

    -- 1) Fetch all the new ones.
    -- 2) After successful fetch, find all the PPs under this root.
    -- 3) Resume CA validation starting from the certificates on the waiting list.
    forM newRepositories $ \r -> do 
        let repoContext' = vContext $ repositoryURI r
        (result, validations) <- runValidatorT repoContext' $ do 
            (repo, vs) <- fetchRepository appContext database r 
            -- TODO Save new repository status to the database
            case repositoryStatus repo of 
                FetchedAt _ -> do 
                    let qq = fromMaybe Set.empty $ Map.lookup repo rootToPps
                    pure ()
                _ -> pure ()
            pure (repo, vs)
        let q = validations <> either (mError repoContext') snd result
        pure ()
    
        -- queueVResult topDownContext vContext' (result', vs)


    {- 
        forM mergedRepositories $ \(r, caCert) -> do              
            r' <- case status r of    
                    NEW -> fetchRepository r
                    _   -> pure r            
            let vContext' = vContext $ repositoryURI r'
            validateCA env vContext' database taName caCert  
    -}


    pure ()
        
    

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
                    let doInParallel as f = parallel (parallelism config) f as 
                    mftProblems <- lift3 $ doInParallel childrenHashes $ \h -> do            
                        ro <- roTx objectStore' $ \tx -> getByHash tx objectStore' h
                        case ro of 
                            Nothing  -> pure $ Just $ ManifestEntryDontExist h
                            Just ro' -> do                         
                                validateChild vContext' validCrl ro'
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
            | otherwise -> void $ (flip Map.traverseWithKey) validationsMap $ 
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


-- TODO Optimise so that it reads the whole queue at once 
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
roAppTx :: Storage s => 
            s -> (Tx s 'RO -> ValidatorT env IO a) -> ValidatorT env IO a 
roAppTx s f = appTx s f roTx    

rwAppTx :: Storage s => 
            s -> (forall mode . Tx s mode -> ValidatorT env IO a) -> ValidatorT env IO a
rwAppTx s f = appTx s f rwTx


appTx :: Storage s => 
            s -> (Tx s mode -> ValidatorT env IO a) -> 
            (s -> (Tx s mode -> IO (Either AppError a, Validations))
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
roAppTxEx s err f = appTxEx s err f roTx    

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
appTxEx s err f txF = do
    env <- ask
    -- TODO Make it less ugly and complicated
    t <- lift3 $ try $ txF (storage s) $ runValidatorT env . f
    validatorT $ pure $ either ((, mempty) . Left . err) id t

