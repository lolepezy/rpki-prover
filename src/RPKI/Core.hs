{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

module RPKI.Core where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Monad.Reader

import qualified Control.Concurrent.STM.TBQueue   as Q

import           Data.String.Interpolate
import           Data.Has
import qualified Data.List.NonEmpty               as NE
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import qualified Data.Text                        as T

import           Data.Tuple.Ops

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Logging
import           RPKI.Parse.Parse
import           RPKI.Resources.Resources
import           RPKI.Resources.Types
import           RPKI.Rsync
import           RPKI.Store.Base.Storage
import           RPKI.Store.Stores
import           RPKI.TAL
import           RPKI.Util                        (fmtEx)
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
                    putTA tx taStore (StoredTA tal newCert)
                    -- TODO Trigger tree validation
                Just (StoredTA _ oldCert) -> 
                    when (getSerial oldCert /= getSerial newCert) $ do            
                        logInfo_ logger [i| Updating TA certificate for #{getTaName tal} |]
                        putTA tx taStore (StoredTA tal newCert)            
                        -- TODO Trigger tree validation          


data TopDownContext = TopDownContext {
    verifiedResources :: Maybe (VerifiedRS PrefixesAndAsns),
    resultQueue :: TBQueue VResult,
    repositoryQueue :: TBQueue Repository,
    repositoryMap :: TVar (Map URI Repository)
}


validateCA :: Storage s => 
            (AppLogger, VContext, Now) ->                
            (RpkiObjectStore s, VResultStore s, RepositoryStore s) -> 
            CerObject ->                
            IO ()
validateCA env stores@(objectStore, resultStore, repositoryStore) certificate = do
    let vContext = getter env :: VContext
    topDownContext <- TopDownContext Nothing <$> 
                        (atomically $ newTBQueue 100) <*>
                        (atomically $ newTBQueue 100) <*>
                        (atomically $ newTVar Map.empty)

    let topDown = Concurrently $ do
        result <- runValidatorT env $ validateTree stores certificate topDownContext
        queueVResult (topDownContext, vContext) result

    let writeVResults = Concurrently $ forever $ 
            writeVResult topDownContext resultStore
    let writeRepositories = Concurrently $ forever $ 
            writeRepository topDownContext repositoryStore

    {- Write validation results and repositories in separate threads to avoid 
        blocking on the database with writing transactions during the validation process 
    -}
    void $ runConcurrently $ (,,) <$> 
            topDown <*> writeVResults <*> writeRepositories
    

-- | Do top-down validation starting from the given certificate
-- TODO Resolve URL VContext to an actual URL instead of the rsync path.
validateTree :: (Has AppLogger env, 
                Has Now env, 
                Has VContext env,
                Storage s) =>
                (RpkiObjectStore s, VResultStore s, RepositoryStore s) ->
                CerObject ->
                TopDownContext ->
                ValidatorT env IO ()
validateTree stores@(objectStore, resultStore, repositoryStore) certificate topDownContext = do  
    (logger :: AppLogger, now :: Now, vContext :: VContext) <- 
        (,,) <$> asks getter <*> asks getter <*> asks getter    

    let (childrenAki, locations) = (toAKI $ getSKI certificate, getLocations certificate)
        
    registerPublicationPoint certificate

    mft <- findMft childrenAki locations  

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
            forM_ childrenHashes $ \h -> do
                ro <- lift3 $ roTx objectStore $ \tx -> getByHash tx objectStore h
                case ro of 
                    Nothing  -> vWarn $ ManifestEntryDontExist h
                    Just ro' -> lift3 $ validateChild (logger, now, topDownContext, vContext) validatedCrl ro'

        Just _  -> vError $ CRLHashPointsToAnotherObject crlHash locations        
    where
        findMft :: Has VContext env => 
                    AKI -> Locations -> ValidatorT env IO MftObject
        findMft childrenAki locations = do
            vContext :: VContext <- asks getter 
            validatorT $ roTx objectStore $ \tx -> 
                runValidatorT vContext $
                    lift3 (findLatestMftByAKI tx objectStore childrenAki) >>= \case
                        Nothing  -> vError $ NoMFT childrenAki locations
                        Just mft -> pure mft

        -- TODO Is there a more reliable way to find it? Compare it with SIA?
        findCrlOnMft mft = filter (\(name, _) -> ".crl" `T.isSuffixOf` name) $ 
            mftEntries $ getCMSContent $ extract mft

        validateChild parentContext validatedCrl ro = 
            case ro of
                CerRO childCert -> do 
                    result <- runValidatorT childContext $ do
                            childVerifiedResources <- pureToValidatorT $ do                 
                                validateResourceCert childCert certificate validatedCrl                
                                validateResources (verifiedResources topDownContext) childCert certificate 
                            lift3 $ logDebug_ (getter childContext :: AppLogger) 
                                [i|Validated: #{getter childContext :: VContext}, resources: #{childVerifiedResources}.|]                
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
            lift3 $ atomically $ case getRrdpNotifyUri cert of
                Just rrdpNotify -> 
                    add rrdpNotify $ RrdpRepo $ RrdpRepository rrdpNotify Nothing
                Nothing         -> 
                    case getRepositoryUri cert of
                        Nothing -> pure ()
                        Just repositoryUri -> 
                            add repositoryUri $ RsyncRepo $ RsyncRepository repositoryUri
                where 
                    add uri repo = do
                        let tRepoMap = repositoryMap topDownContext
                        repoMap <- readTVar tRepoMap                        
                        case pure $ Map.lookup uri repoMap of
                            Nothing -> do
                                let queue = repositoryQueue topDownContext                                
                                writeTVar tRepoMap $ Map.insert uri repo repoMap                            
                                Q.writeTBQueue queue repo
                            Just _ -> pure ()            

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
    atomically $ Q.writeTBQueue queue $ VResult problems context

-- | Get validation result from the queue and save it to the DB
writeVResult topDownContext resultStore = do
    vrs <- atomically $ many $ Q.readTBQueue $ resultQueue topDownContext
    rwTx resultStore $ \tx -> 
        forM_ vrs $ putVResult tx resultStore

-- | Get new repositories from the queue and save it to the DB
writeRepository topDownContext repositoryStore = do
    vrs <- atomically $ many $ Q.readTBQueue $ repositoryQueue topDownContext
    rwTx repositoryStore $ \tx -> 
        forM_ vrs $ \r -> putRepository tx repositoryStore $ newRepository r



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

