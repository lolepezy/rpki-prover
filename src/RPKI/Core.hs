{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

module RPKI.Core where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader

import qualified Control.Concurrent.STM.TBQueue   as Q

import           Data.Has
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NE
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes)
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.String.Interpolate
import qualified Data.Text                        as T

import           Data.Tuple.Ops

import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Logging
import           RPKI.Parallel                    (parallel)
import           RPKI.Parse.Parse
import           RPKI.Resources.Resources
import           RPKI.Resources.Types
import           RPKI.Rsync
import           RPKI.Store.Base.Storage
import           RPKI.Store.Data
import           RPKI.Store.Stores
import           RPKI.TAL
import           RPKI.Env
import           RPKI.Util                        (fmtEx)
import           RPKI.Validation.ObjectValidation 



roAppTx :: WithStorage s s => s -> env -> (Tx s 'RO -> ValidatorT env IO a) -> ValidatorT env IO a
roAppTx s e f = validatorT $ roTx s $ \tx -> runValidatorT e (f tx)

rwAppTx :: WithStorage s s => s -> env -> (forall tm . Tx s tm -> ValidatorT env IO a) -> ValidatorT env IO a
rwAppTx s e f = validatorT $ rwTx s $ \tx -> runValidatorT e (f tx)


-- | Valiidate TA starting from the TAL.
-- | 
validateTA :: (Has AppContext env, Has VContext vc, Storage s) => 
            env -> TAL -> DB s -> ValidatorT vc IO ()
validateTA env tal database = do
    let appContext@AppContext {..} = getter env
    let tas = taStore database
    let taName = getTaName tal
    let taContext = vContext $ getTaURI tal
    (uri', ro) <- fetchTACertificate appContext tal
    newCert <- pureToValidatorT $ validateTACert tal uri' ro  
    join $ 
        fromTryEither (StorageE . StorageError . fmtEx) $
            rwTx tas $ \tx -> do
                getTA tx tas taName >>= \case
                    Nothing -> do
                        -- it's a new TA, store it
                        let c = cwsX509certificate $ getCertWithSignature $ newCert
                        logInfo_ logger [i| Storing new #{getTaName tal}, 
                            getRrdpNotifyUri newCert = #{getRrdpNotifyUri c }, getRepositoryUri newCert = #{getRepositoryUri c}|]
                    
                        putTA tx tas (STA tal newCert)
                        
                        case createRepository tal newCert of
                            Left e -> pure $ Left $ ValidationE e
                            Right repository -> do                                
                                putRepository tx (repositoryStore database) (newRepository repository) taName                                      
                                pure $ Right $ do 
                                    let vContext' = vContext $ repositoryURI repository
                                    prefetch repository                                    
                                    void $ lift3 $ validateCA env vContext' database taName newCert                                    

                    Just (STA _ oldCert) -> do
                        when (getSerial oldCert /= getSerial newCert) $ do            
                            logInfo_ logger [i| Updating TA certificate for #{getTaName tal} |]
                            putTA tx tas (STA tal newCert)            
                        pure $ Right $ pure ()
    

-- TODO Implement
prefetch :: Repository -> ValidatorT env IO ()
prefetch _ = pure ()


data TopDownContext = TopDownContext {    
    verifiedResources :: Maybe (VerifiedRS PrefixesAndAsns),
    resultQueue :: TBQueue (Maybe VResult),
    repositoryQueue :: TBQueue (Maybe Repository),
    repositoryMap :: TVar (Map URI Repository),
    newRepositories :: TVar (Set URI),
    taName :: TaName, 
    now :: Now
}

emptyTopDownContext :: TaName -> IO TopDownContext
emptyTopDownContext taName = do 
    now <- thisMoment
    atomically $ TopDownContext Nothing <$> 
            (newTBQueue 100) <*>
            (newTBQueue 100) <*>
            (newTVar Map.empty) <*>
            (newTVar Set.empty) <*>
            pure taName <*> 
            pure now   


validateCA :: (Has AppContext env, Storage s) =>
            env -> VContext -> DB s -> TaName -> CerObject -> IO [URI]
validateCA env vContext database taName certificate = do    
    let appContext@AppContext {..} = getter env
    topDownContext <- emptyTopDownContext taName    
    let validateAll = do
            result <- runValidatorT vContext $ validateTree env database certificate topDownContext
            queueVResult topDownContext vContext result

    let finaliseQueues = atomically $ do
            Q.writeTBQueue (repositoryQueue topDownContext) Nothing
            Q.writeTBQueue (resultQueue topDownContext) Nothing

    let topDown = Concurrently $ validateAll `finally` finaliseQueues
    let saveVResults = Concurrently $ writeVResults logger topDownContext (resultStore database)
    let saveRepositories = Concurrently $ writeRepositories logger taName topDownContext (repositoryStore database)

    {- Write validation results and repositories in separate threads to avoid 
        blocking on the database with writing transactions during the validation process 
    -}
    void $ runConcurrently $ (,,) <$> 
                topDown <*> saveVResults <*> saveRepositories

    Set.toList <$> (atomically $ readTVar $ newRepositories topDownContext)
        
    

-- | Do top-down validation starting from the given certificate
-- TODO Resolve URL VContext to an actual URL instead of the rsync path.
validateTree :: (Has AppContext env, 
                Has VContext vc, 
                Storage s) =>
            env ->
            DB s ->
            CerObject ->
            TopDownContext ->
            ValidatorT vc IO ()
validateTree env database certificate topDownContext = do      
    let appContext@AppContext {..} = getter env
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
            validCrl <- pureToValidatorT $ do          
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
                            childVerifiedResources <- pureToValidatorT $ do                 
                                validateResourceCert childCert certificate validCrl                
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
        registerPublicationPoint (cwsX509certificate . getCertWithSignature -> cert) = 
            case repositoryObject of
                Nothing          -> pure Nothing
                Just (uri, repo) -> lift3 $ atomically $ do            
                    let tRepoMap = repositoryMap topDownContext
                    let tNewRepositories = newRepositories topDownContext
                    repoMap <- readTVar tRepoMap                        
                    newRepositories <- readTVar tNewRepositories                        
                    case Map.lookup uri repoMap of
                        Nothing -> do
                            let queue = repositoryQueue topDownContext                                
                            writeTVar tRepoMap $ Map.insert uri repo repoMap     
                            writeTVar tNewRepositories $ Set.insert uri newRepositories
                            Q.writeTBQueue queue $ Just repo
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
queueVResult :: TopDownContext -> VContext -> (Either SomeError (), [VWarning]) -> IO ()
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


-- | Get new repositories from the queue and save it to the DB
writeRepositories :: (Storage s) => AppLogger -> TaName -> TopDownContext -> RepositoryStore s -> IO ()
writeRepositories logger taName topDownContext repositoryStore =
    withQueue (repositoryQueue topDownContext) $ \r -> do
        logDebug_ logger [i|Repository: #{r}.|]
        rwTx repositoryStore $ \tx ->            
            putRepository tx repositoryStore (newRepository r) taName

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
        go (u : uris) = ((u,) <$> rsyncFile appContext u) `catchError` \e -> do            
            let message = [i| Failed to fetch #{u}: #{e}|]
            lift3 $ logError_ (logger appContext) message
            validatorWarning $ VWarning e
            go uris


-- | Create repository based on URIs in TAL and in TA certificate,
-- | use some reasonable heuristics, but don't try to be very smart.
-- | URI of the repository is supposed to be a "real" one, i.e. where
-- | repository can actually be downloaded from.
createRepository :: TAL -> CerObject -> Either ValidationError Repository
createRepository tal (cwsX509certificate . getCertWithSignature -> cert) = 
    case tal of 
        PropertiesTAL {..} -> case prefetchUris of
                                []      -> fromCert
                                uri : _ -> fromURI uri
        RFC_TAL {..}       -> fromCert
    where
        isRsyncURI (URI u) = "rsync://" `T.isPrefixOf` u
        isRrdpURI (URI u) = "http://" `T.isPrefixOf` u || "https://" `T.isPrefixOf` u
        fromURI u = 
            if isRsyncURI u 
                then Right $ RsyncRepo $ RsyncRepository u
                else if isRrdpURI u
                    then Right $ RrdpRepo $ RrdpRepository u Nothing
                    else Left $ UnknownUriType u

        fromCert = case getRrdpNotifyUri cert of
                Just rrdpNotify | isRrdpURI rrdpNotify -> 
                                        Right $ RrdpRepo $ RrdpRepository rrdpNotify Nothing
                                | otherwise -> Left $ UnknownUriType rrdpNotify
                Nothing -> 
                    case getRepositoryUri cert of 
                        Just repositoryUri | isRsyncURI repositoryUri -> 
                                        Right $ RsyncRepo $ RsyncRepository repositoryUri
                                            | otherwise -> Left $ UnknownUriType repositoryUri
                        Nothing -> Left CertificateDoesn'tHaveSIA 
