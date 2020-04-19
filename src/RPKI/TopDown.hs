{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module RPKI.TopDown where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader

import           GHC.Generics

import qualified Control.Concurrent.STM.TBQueue   as Q
import           Data.Has
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NonEmpty
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (catMaybes)
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
import           RPKI.TAL
import           RPKI.Util                        (fmtEx)
import           RPKI.Validation.ObjectValidation


data TACertValidationResult = 
    SameTACert !CerObject !(NonEmpty Repository) |
    UpdatedTACert !CerObject !(NonEmpty Repository)
    deriving (Show, Eq, Generic)

-- bootstrapTA = do
--     next <- validateTA
--     case next of
--         SameTACert newCert repositories -> theSameActually newCert repositories
--         UpdatedTACert taCert repositories -> theSameActually newCert repositories
--     where
--         theSameActually newCert repositories = do 
--             -- do it in parallel of course
--             x :: [(Repository, Validations)] <- forM repositories fetchRepository 
--             -- do all the recursive fetching and validaiton
--             validateCA env taCertURI database taName taCert





-- | Valiidate TA starting from the TAL.
-- | TODO Do something consistent with Validations
validateTA :: (Has AppContext env, Has VContext vc, Storage s) => 
            env -> TAL -> DB s -> ValidatorT vc IO ()
validateTA env tal database@DB {..} = do
    let appContext@AppContext {..} = getter env
    let taName = getTaName tal
    let taContext = vContext $ getTaURI tal
    (uri', ro) <- fetchTACertificate appContext tal
    newCert <- vHoist $ validateTACert tal uri' ro  
    join $ 
        fromTryEither (StorageE . StorageError . fmtEx) $
            rwTx taStore $ \tx -> do

                let fetchAndValidate repository = do 
                        let vContext' = vContext $ repositoryURI repository
                        (repo, validations) <- fetchRepository appContext database repository
                        lift3 $ do 
                            rwTx repositoryStore $ \txNew ->    
                                updateRepositoryStatus txNew repositoryStore repo FETCHED
                            validateCA env vContext' database taName newCert  

                getTA tx taStore taName >>= \case
                    Nothing -> do
                        -- it's a new TA, store it and trigger all the other actions
                        let c = cwsX509certificate $ getCertWithSignature $ newCert
                        logInfo_ logger [i| Storing new #{getTaName tal}, 
                            getRrdpNotifyUri newCert = #{getRrdpNotifyUri c}, 
                            getRepositoryUri newCert = #{getRepositoryUri c}|]
                                                                
                        case createRepositoriesFromTAL tal newCert of
                            Left e -> pure $ Left $ ValidationE e
                            Right repositories -> do                                
                                forM_ repositories $ 
                                    \r -> putRepository tx repositoryStore (newRepository r) taName
                                pure $ Right $ do 
                                    putTA tx taStore (STA tal newCert repositories)
                                    forM_ repositories fetchAndValidate
                                    -- pure $ Right $ UpdatedTACert newCert repositories

                    Just (sta@STA {..}) -> do
                        when (getSerial taCert /= getSerial newCert) $ do            
                            logInfo_ logger [i| Updating TA certificate for #{getTaName tal} |]
                            -- pure $ Right $ do 
                                    -- lift3 $ rwTx taStore $ \txNew ->    
                                    --          putTA txNew taStore $ sta { taCert = newCert }
                            --         forM_ initialRepositories fetchAndValidate
                            -- pure $ Right $ pure ()
                                -- pure $ Right $ UpdatedTACert repositories newCert
                        pure $ Right $ pure ()
                        -- pure $ Right $ SameTACert newCert repositories


-- | Valiidate TA starting from the TAL.
-- | TODO Do something consistent with Validations
validateTAFromTAL :: (Has AppContext env, Has VContext vc, Storage s) => 
                        env -> TAL -> DB s -> ValidatorT vc IO TACertValidationResult
validateTAFromTAL env tal DB {..} = do
    let appContext@AppContext {..} = getter env
    let taName = getTaName tal
    (uri', ro) <- fetchTACertificate appContext tal
    newCert    <- vHoist $ validateTACert tal uri' ro      
    rwAppTxEx 
        taStore  
        (StorageE . StorageError . fmtEx)
        $ \tx -> do
            r <- getTA tx taStore taName
            case r of
                Nothing -> do
                    -- it's a new TA, store it and trigger all the other actions
                    let c = cwsX509certificate $ getCertWithSignature $ newCert
                    logInfoM logger [i| Storing new #{getTaName tal}, 
                        getRrdpNotifyUri newCert = #{getRrdpNotifyUri c}, 
                        getRepositoryUri newCert = #{getRepositoryUri c}|]
                                                            
                    case createRepositoriesFromTAL tal newCert of
                        Left e             -> appError $ ValidationE e
                        Right repositories -> pure $ UpdatedTACert newCert repositories

                Just (STA {..}) ->
                    if (getSerial taCert /= getSerial newCert) 
                        then do            
                            logInfoM logger [i| Updating TA certificate for #{getTaName tal} |]
                            case createRepositoriesFromTAL tal newCert of
                                Left e             -> appError $ ValidationE e
                                Right repositories -> pure $ UpdatedTACert newCert repositories                             
                        else 
                            pure $ SameTACert taCert initialRepositories
     

-- | Download repository and 
fetchRepository :: Storage s => 
            AppContext -> DB s -> Repository -> ValidatorT env IO (Repository, Validations)
fetchRepository appContext@AppContext {..} database r = 
    case r of
        RsyncRepo repo -> do
            (repo', validations) <- updateObjectForRsyncRepository appContext repo (objectStore database)
            pure (RsyncRepo repo', validations)
        RrdpRepo repo -> do
            (repo', validations) <- updateObjectForRrdpRepository appContext repo (objectStore database)
            pure (RrdpRepo repo', validations)


-- Auxiliarry structure used in top-down validation
data TopDownContext = TopDownContext {    
    verifiedResources :: Maybe (VerifiedRS PrefixesAndAsns),
    resultQueue :: TBQueue (Maybe VResult),
    repositoryMap :: TVar (Map URI Repository),
    newRepositories :: TVar (Map URI CerObject),
    taName :: TaName, 
    now :: Now
}

emptyTopDownContext :: TaName -> IO TopDownContext
emptyTopDownContext taName = do 
    now <- thisMoment
    atomically $ TopDownContext Nothing <$> 
            (newTBQueue 100) <*>
            (newTVar Map.empty) <*>
            (newTVar Map.empty) <*>
            pure taName <*> 
            pure now   


validateCA :: (Has AppContext env, Storage s) =>
            env -> VContext -> DB s -> TaName -> CerObject -> IO ()
validateCA env vContext' database@DB {..} taName certificate = do    
    let AppContext {..} = getter env
    topDownContext <- emptyTopDownContext taName    

    currentRepositories <- roTx database $ \tx -> getRepositoriesForTA tx repositoryStore taName

    let validateAll = do
            result <- runValidatorT vContext' $ validateTree env database certificate topDownContext
            queueVResult topDownContext vContext' result

    let finaliseQueue = atomically $ Q.writeTBQueue (resultQueue topDownContext) Nothing

    let topDown = Concurrently $ validateAll `finally` finaliseQueue
    let saveVResults = Concurrently $ writeVResults logger topDownContext resultStore

    {- Write validation results and repositories in separate threads to avoid 
        blocking on the database with writing transactions during the validation process 
    -}
    void $ runConcurrently $ (,) <$> topDown <*> saveVResults

    newRepos <- atomically $ readTVar $ newRepositories topDownContext


    -- merge newRepositories with existing ones, i.e.
    --  - join rsync repos under the same root
    --  - take only unique RRDP repos

    -- save new ones after filtering everything with NEW status

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
-- TODO Do something reasonable when MFT is not found because repository is not yet fetched
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
    vContext' :: VContext <- asks getter

    let (childrenAki, locations) = (toAKI $ getSKI certificate, getLocations certificate)        

    publicationUri <- registerPublicationPoint certificate

    mft <- findMft childrenAki locations publicationUri

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
    where                        
        findMft :: Has VContext env => 
                    AKI -> Locations -> Maybe URI -> ValidatorT env IO MftObject
        findMft childrenAki locations publicationUri = do
            mft' <- lift3 $ roTx (objectStore database) $ \tx -> 
                findLatestMftByAKI tx (objectStore database) childrenAki
            case mft' of
                Nothing  -> case publicationUri of
                                Nothing -> vError $ NoMFTNoRepository childrenAki locations
                                Just _  -> vError $ NoMFT childrenAki locations
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
                            -- logDebugM (getter childContext :: AppLogger) 
                            --     [i|Validated: #{getter childContext :: VContext}, resources: #{childVerifiedResources}.|]                
                            validateTree env database childCert 
                                topDownContext { verifiedResources = Just childVerifiedResources }
                    queueVResult topDownContext childContext result

                RoaRO roa -> queueVResult topDownContext childContext $ 
                                runPureValidator childContext $ 
                                    void $ validateRoa (now topDownContext) roa certificate validCrl
                GbrRO gbr -> queueVResult topDownContext childContext $ 
                                runPureValidator childContext $ 
                                    void $ validateGbr (now topDownContext) gbr certificate validCrl
                -- TODO Anything else?
                _ -> pure ()
            where
                childContext = childVContext parentContext childLocation 
                childLocation = NonEmpty.head $ getLocations ro


        -- | Register URI of the repository for the given certificate if it's not there yet
        registerPublicationPoint cerObject@(cwsX509certificate . getCertWithSignature -> cert) = 
            case repositoryObject of
                Nothing          -> pure Nothing
                Just (uri, repo) -> lift3 $ atomically $ do            
                    let tRepoMap = repositoryMap topDownContext
                    let tNewRepositories = newRepositories topDownContext
                    repoMap <- readTVar tRepoMap                        
                    newRepositories <- readTVar tNewRepositories                        
                    case Map.lookup uri repoMap of
                        Nothing -> do
                            writeTVar tRepoMap $ Map.insert uri repo repoMap     
                            writeTVar tNewRepositories $ Map.insert uri cerObject newRepositories
                            pure $ Just uri
                        Just _ -> pure $ Just uri
            where
                repositoryObject = 
                    case getRrdpNotifyUri cert of
                        Just rrdpNotify -> 
                            Just (rrdpNotify, RrdpRepo $ RrdpRepository rrdpNotify Nothing)
                        Nothing  -> flip fmap (getRepositoryUri cert) $ \repositoryUri ->
                            (repositoryUri, RsyncRepo $ RsyncRepository repositoryUri)


-- | Put validation result into a queue for writing
queueVResult :: TopDownContext -> VContext -> (Either AppError (), Validations) -> IO ()
queueVResult topDownContext vc result = do    
    let validations = case result of            
            (Left e, vs)  -> vs <> mError vc e
            (Right _, vs) -> vs
    let queue = resultQueue topDownContext
    case validations of
        Validations validationsMap
            | emptyValidations validations -> pure ()
            | otherwise -> void $ (flip Map.traverseWithKey) validationsMap $ 
                    \vc' problems -> atomically $ Q.writeTBQueue queue $ 
                                        Just $ VResult (Set.toList problems) vc'

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
            ws -> (exc -> AppError) -> (Tx s 'RO -> ValidatorT env IO a) -> ValidatorT env IO a 
roAppTxEx s err f = appTxEx s err f roTx    

rwAppTxEx :: (Storage s, WithStorage s ws, Exception exc) => 
            ws -> (exc -> AppError) -> 
            (forall mode . Tx s mode -> ValidatorT env IO a) -> ValidatorT env IO a
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

