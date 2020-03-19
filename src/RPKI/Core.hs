{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

module RPKI.Core where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Exception
import           Control.Monad.Reader

import qualified Control.Concurrent.STM.TBQueue as Q

import           Data.Has
import qualified Data.List.NonEmpty             as NE
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Maybe                     (catMaybes)
import           Data.String.Interpolate
import qualified Data.Text                      as T

import           Data.Tuple.Ops

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Config
import           RPKI.Logging
import           RPKI.Parse.Parse
import           RPKI.Resources.Resources
import           RPKI.Resources.Types
import           RPKI.Rsync
import           RPKI.Store.Base.Storage
import           RPKI.Store.Data
import           RPKI.Store.Stores
import           RPKI.TAL
import           RPKI.Util                        (fmtEx)
import           RPKI.Parallel                    (parallel)
import           RPKI.Validation.ObjectValidation



-- | Valiidate TAL
validateTA :: (Has RsyncConf env, Has AppLogger env, Storage s) =>
                TAL -> 
                TAStore s ->
                ValidatorT env IO ()
validateTA tal taStore = do
    logger :: AppLogger <- asks getter
    (uri', ro) <- fetchTACertificate tal
    newCert <- pureToValidatorT $ validateTACert tal uri' ro  
    fromTry (StorageE . StorageError . fmtEx) $ 
        rwTx taStore $ \tx -> do      
            getTA tx taStore (getTaName tal) >>= \case
                Nothing -> do
                    -- it's a new TA, store it
                    logInfo_ logger [i| Storing new #{getTaName tal} |]
                    putTA tx taStore (STA tal newCert)
                    -- TODO Trigger tree validation
                Just (STA _ oldCert) -> 
                    when (getSerial oldCert /= getSerial newCert) $ do            
                        logInfo_ logger [i| Updating TA certificate for #{getTaName tal} |]
                        putTA tx taStore (STA tal newCert)            
                        -- TODO Trigger tree validation          


data TopDownContext = TopDownContext {    
    verifiedResources :: Maybe (VerifiedRS PrefixesAndAsns),
    resultQueue :: TBQueue (Maybe VResult),
    repositoryQueue :: TBQueue (Maybe Repository),
    repositoryMap :: TVar (Map URI Repository)
}


validateCA :: (Has AppLogger env, 
            Has Now env, 
            Has VContext env,
            Has Config env,
            Has TaName env,
            Storage s) =>
            env ->                
            (RpkiObjectStore s, VResultStore s, RepositoryStore s) -> 
            CerObject ->                
            IO ()
validateCA env stores@(objectStore, resultStore, repositoryStore) certificate = do
    let vContext = getter env :: VContext
    let logger = getter env :: AppLogger
    topDownContext <- TopDownContext Nothing <$> 
                        (atomically $ newTBQueue 100) <*>
                        (atomically $ newTBQueue 100) <*>
                        (atomically $ newTVar Map.empty)

    let validateAll = do
            result <- runValidatorT env $ validateTree stores certificate topDownContext
            queueVResult (topDownContext, vContext) result

    let finalizeQueues = atomically $ do
            Q.writeTBQueue (repositoryQueue topDownContext) Nothing
            Q.writeTBQueue (resultQueue topDownContext) Nothing

    let taName = getter env :: TaName
    let topDown = Concurrently $ validateAll `finally` finalizeQueues
    let saveVResults = Concurrently $ writeVResults logger topDownContext resultStore
    let saveRepositories = Concurrently $ writeRepositories logger taName topDownContext repositoryStore

    {- Write validation results and repositories in separate threads to avoid 
        blocking on the database with writing transactions during the validation process 
    -}
    void $ runConcurrently $ (,,) <$> 
            topDown <*> saveVResults <*> saveRepositories
        
    

-- | Do top-down validation starting from the given certificate
-- TODO Resolve URL VContext to an actual URL instead of the rsync path.
validateTree :: (Has AppLogger env, 
                Has Now env, 
                Has VContext env,
                Has Config env,
                Storage s) =>
                (RpkiObjectStore s, VResultStore s, RepositoryStore s) ->
                CerObject ->
                TopDownContext ->
                ValidatorT env IO ()
validateTree stores@(objectStore, resultStore, repositoryStore) certificate topDownContext = do  
    (logger :: AppLogger, now :: Now, vContext :: VContext, config :: Config) <- 
        (,,,) <$> asks getter <*> asks getter <*> asks getter <*> asks getter   

    let (childrenAki, locations) = (toAKI $ getSKI certificate, getLocations certificate)
        
    publicationUri <- registerPublicationPoint certificate

    mft <- findMft childrenAki locations publicationUri

    (_, crlHash) <- case findCrlOnMft mft of 
        []    -> vError $ NoCRLOnMFT childrenAki locations
        [crl] -> pure crl
        _     -> vError $ MoreThanOneCRLOnMFT childrenAki locations

    crlObject <- lift3 $ roTx objectStore $ \tx -> getByHash tx objectStore crlHash
    case crlObject of 
        Nothing          -> vError $ NoCRLExists childrenAki locations    
        Just (CrlRO crl) -> do      
            validatedCrl <- pureToValidatorT $ do          
                crl' <- validateCrl crl certificate
                validateMft mft certificate crl'
                pure crl'
                
            let childrenHashes = map snd $ mftEntries $ getCMSContent $ extract mft    
            let doInParallel as f = parallel (parallelism config) f as 
            -- let doInParallel as f = mapM f as 

            mftProblems <- lift3 $ doInParallel childrenHashes $ \h -> do            
                ro <- roTx objectStore $ \tx -> getByHash tx objectStore h
                case ro of 
                    Nothing  -> pure $ Just $ ManifestEntryDontExist h
                    Just ro' -> do 
                        validateChild (logger, now, topDownContext, vContext, config) validatedCrl ro'
                        pure Nothing

            case mftProblems of
                [] -> pure ()
                ps -> mapM_ vWarn $ catMaybes mftProblems

        Just _  -> vError $ CRLHashPointsToAnotherObject crlHash locations        
    where                        
        findMft :: Has VContext env => 
                    AKI -> Locations -> Maybe URI -> ValidatorT env IO MftObject
        findMft childrenAki locations publicationUri = do
            mft' <- lift3 $ roTx objectStore $ \tx -> 
                findLatestMftByAKI tx objectStore childrenAki
            case mft' of
                Nothing  -> case publicationUri of
                                Nothing -> vError $ NoMFTNoRepository childrenAki locations
                                Just _  -> vError $ NoMFT childrenAki locations
                Just mft -> pure mft

        -- TODO Is there a more reliable way to find it? Compare it with SIA?
        findCrlOnMft mft = filter (\(name, _) -> ".crl" `T.isSuffixOf` name) $ 
            mftEntries $ getCMSContent $ extract mft

        validateChild :: (AppLogger, Now, TopDownContext, VContext, Config) -> Validated CrlObject -> RpkiObject -> IO () 
        validateChild parentContext validatedCrl ro = 
            case ro of
                CerRO childCert -> do 
                    result <- runValidatorT childContext $ do
                            childVerifiedResources <- pureToValidatorT $ do                 
                                validateResourceCert childCert certificate validatedCrl                
                                validateResources (verifiedResources topDownContext) childCert certificate 
                            -- lift3 $ logDebug_ (getter childContext :: AppLogger) 
                            --     [i|Validated: #{getter childContext :: VContext}, resources: #{childVerifiedResources}.|]                
                            validateTree stores childCert (topDownContext { verifiedResources = Just childVerifiedResources })
                    queueVResult childContext result

                RoaRO roa -> queueVResult childContext $ 
                                runPureValidator childContext $ 
                                    void $ validateRoa roa certificate validatedCrl
                GbrRO gbr -> queueVResult childContext $ 
                                runPureValidator childContext $ 
                                    void $ validateGbr gbr certificate validatedCrl
                -- TODO Anything else?
                _ -> pure ()
            where
                childContext = mapT (\pv -> childVContext pv childLocation) parentContext                
                childLocation = NE.head $ getLocations ro


        -- | Register URI of the repository for the given certificate if it's not there yet
        registerPublicationPoint (cwsX509certificate . getCertWithSignature -> cert) = 
            case repositoryObject of
                Nothing          -> pure Nothing
                Just (uri, repo) -> lift3 $ atomically $ do            
                    let tRepoMap = repositoryMap topDownContext
                    repoMap <- readTVar tRepoMap                        
                    case Map.lookup uri repoMap of
                        Nothing -> do
                            let queue = repositoryQueue topDownContext                                
                            writeTVar tRepoMap $ Map.insert uri repo repoMap                            
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
queueVResult :: (Has TopDownContext env, Has VContext env) => 
                env -> (Either SomeError (), [VWarning]) -> IO ()
queueVResult env r = do
    let topDownContext :: TopDownContext = getter env
    let context :: VContext = getter env
    let problems = case r of            
            (Left e, ws)  -> map VWarn ws <> [VErr e]
            (Right _, ws) -> map VWarn ws
    let queue = resultQueue topDownContext
    case problems of
        [] -> pure ()
        ps -> atomically $ Q.writeTBQueue queue $ Just $ VResult ps context

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


-- | Fetch TA certificate based on TAL location(s 
fetchTACertificate :: (Has RsyncConf env, Has AppLogger env) => 
                        TAL -> ValidatorT env IO (URI, RpkiObject)
fetchTACertificate tal = 
    go $ NE.toList $ certLocations tal
    where
        go []         = throwError $ TAL_E $ TALError "No certificate location could be fetched."
        go (u : uris) = ((u,) <$> rsyncFile u) `catchError` \e -> do          
            logger :: AppLogger <- asks getter
            let message = [i| Failed to fetch #{u}: #{e}|]
            lift3 $ logError_ logger message
            validatorWarning $ VWarning e
            go uris

