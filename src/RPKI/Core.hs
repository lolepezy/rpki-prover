{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

module RPKI.Core where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader

import qualified Control.Concurrent.STM.TBQueue   as Q

import           Data.Has
import qualified Data.List.NonEmpty               as NE
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes)
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.String.Interpolate
import qualified Data.Text                        as T


import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Logging
import           RPKI.Parallel                    (parallel)
import           RPKI.Parse.Parse
import           RPKI.Resources.Types
import           RPKI.Rsync
import           RPKI.RRDP.Update
import           RPKI.Repository
import           RPKI.Store.Base.Storage
import           RPKI.Store.Data
import           RPKI.Store.Stores
import           RPKI.TAL
import           RPKI.Util                        (fmtEx)
import           RPKI.Validation.ObjectValidation 



roAppTx :: WithStorage s s => s -> env -> (Tx s 'RO -> ValidatorT env IO a) -> ValidatorT env IO a
roAppTx s e f = validatorT $ roTx s $ \tx -> runValidatorT e (f tx)

rwAppTx :: WithStorage s s => s -> env -> (forall tm . Tx s tm -> ValidatorT env IO a) -> ValidatorT env IO a
rwAppTx s e f = validatorT $ rwTx s $ \tx -> runValidatorT e (f tx)


-- | Valiidate TA starting from the TAL.
-- | TODO Do something consistent with Validations
validateTA :: (Has AppContext env, Has VContext vc, Storage s) => 
            env -> TAL -> DB s -> ValidatorT vc IO ()
validateTA env tal database = do
    let appContext@AppContext {..} = getter env
    let tas = taStore database
    let taName = getTaName tal
    let taContext = vContext $ getTaURI tal
    (uri', ro) <- fetchTACertificate appContext tal
    newCert <- vHoist $ validateTACert tal uri' ro  
    join $ 
        fromTryEither (StorageE . StorageError . fmtEx) $
            rwTx tas $ \tx -> do

                let fetchAndValidate repository = do 
                    let vContext' = vContext $ repositoryURI repository
                    (repo, validations) <- fetchRepository appContext database repository
                    lift3 $ updateRepositoryStatus tx (repositoryStore database) repo FETCHED
                    lift3 $ validateCA env vContext' database taName newCert  

                getTA tx tas taName >>= \case
                    Nothing -> do
                        -- it's a new TA, store it and trigger all the other actions
                        let c = cwsX509certificate $ getCertWithSignature $ newCert
                        logInfo_ logger [i| Storing new #{getTaName tal}, 
                            getRrdpNotifyUri newCert = #{getRrdpNotifyUri c }, getRepositoryUri newCert = #{getRepositoryUri c}|]
                    
                        putTA tx tas (STA tal newCert)
                        
                        case createRepositoryFromTAL tal newCert of
                            Left e -> pure $ Left $ ValidationE e
                            Right repository -> do                                
                                putRepository tx (repositoryStore database) (newRepository repository) taName
                                pure $ Right $ fetchAndValidate repository 

                    Just (STA _ oldCert) -> do
                        when (getSerial oldCert /= getSerial newCert) $ do            
                            logInfo_ logger [i| Updating TA certificate for #{getTaName tal} |]
                            putTA tx tas (STA tal newCert)            
                        pure $ Right $ pure ()
    

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
validateCA env vContext database taName certificate = do    
    let AppContext {..} = getter env
    topDownContext <- emptyTopDownContext taName    
    let validateAll = do
            result <- runValidatorT vContext $ validateTree env database certificate topDownContext
            queueVResult topDownContext vContext result

    let finaliseQueue = atomically $ Q.writeTBQueue (resultQueue topDownContext) Nothing

    let topDown = Concurrently $ validateAll `finally` finaliseQueue
    let saveVResults = Concurrently $ writeVResults logger topDownContext (resultStore database)

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
            r' <- fetchRepository r 
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
                ps -> mapM_ vWarn $ catMaybes mftProblems

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
        findCrlOnMft mft = filter (\(name, _) -> ".crl" `T.isSuffixOf` name) $ 
            mftEntries $ getCMSContent $ extract mft

        validateChild :: VContext -> Validated CrlObject -> RpkiObject -> IO () 
        validateChild parentContext validCrl ro = 
            case ro of
                CerRO childCert -> do 
                    result <- runValidatorT childContext $ do
                            childVerifiedResources <- vHoist $ do                 
                                validateResourceCert (now topDownContext) childCert certificate validCrl                
                                validateResources (verifiedResources topDownContext) childCert certificate 
                            -- lift3 $ logDebug_ (getter childContext :: AppLogger) 
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
                childLocation = NE.head $ getLocations ro


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
queueVResult :: TopDownContext -> VContext -> (Either AppError (), [VWarning]) -> IO ()
queueVResult topDownContext vc result = do    
    let problems = case result of            
            (Left e, ws)  -> map VWarn ws <> [VErr e]
            (Right _, ws) -> map VWarn ws
    let queue = resultQueue topDownContext
    case problems of
        [] -> pure ()
        ps -> atomically $ Q.writeTBQueue queue $ Just $ VResult ps vc

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
    go $ NE.toList $ certLocations tal
    where
        go []         = throwError $ TAL_E $ TALError "No certificate location could be fetched."
        go (u : uris) = ((u,) <$> rsyncFile appContext u) `catchError` goToNext 
            where 
                goToNext e = do            
                    let message = [i| Failed to fetch #{u}: #{e}|]
                    lift3 $ logError_ (logger appContext) message
                    validatorWarning $ VWarning e
                    go uris
