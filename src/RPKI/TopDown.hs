{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

module RPKI.TopDown where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader

import           Control.Lens
import           Data.Generics.Product.Typed

import           GHC.Generics

import qualified Control.Concurrent.STM.TBQueue   as Q
import           Data.Bifunctor
import           Data.Either                      (partitionEithers)
import           Data.Foldable
import           Data.Has
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NonEmpty
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromMaybe)
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
import           RPKI.Resources.Resources
import           RPKI.Resources.Types
import           RPKI.RRDP.Update
import           RPKI.Rsync
import           RPKI.Store.Base.Storage
import           RPKI.Store.Data
import           RPKI.Store.Repository
import           RPKI.Store.Stores
import           RPKI.TAL
import           RPKI.Util                        (fmtEx)
import           RPKI.Validation.ObjectValidation



data TACertValidationResult = 
        SameTACert !CerObject !(NonEmpty Repository) |
        UpdatedTACert !CerObject !(NonEmpty Repository)
    deriving (Show, Eq, Generic)

storageError :: SomeException -> AppError
storageError = StorageE . StorageError . fmtEx


data Stats = Stats {
    validCount :: Int
}

-- Auxiliarry structure used in top-down validation
data TopDownContext = TopDownContext {    
    verifiedResources           :: Maybe (VerifiedRS PrefixesAndAsns),
    resultQueue                 :: TBQueue (Maybe VResult),    
    publicationPoints           :: TVar PublicationPoints,
    ppWaitingList               :: TVar (Map URI (Set Hash)),
    takenCareOf                 :: TVar (Set URI),
    taName                      :: TaName, 
    now                         :: Now,
    parallelismDegree           :: Parallelism,
    objectStats                 :: TVar Stats
}

createVerifiedResources :: CerObject -> VerifiedRS PrefixesAndAsns
createVerifiedResources (getRC -> ResourceCertificate certificate) = 
    VerifiedRS $ toPrefixesAndAsns $ withRFC certificate resources

emptyTopDownContext :: MonadIO m => AppContext -> TaName -> PublicationPoints -> CerObject -> m TopDownContext
emptyTopDownContext AppContext {..} taName publicationPoints certificate = liftIO $ do             
    now <- thisMoment
    atomically $ TopDownContext (Just $ createVerifiedResources certificate) <$> 
        newTBQueue 100 <*>
        newTVar publicationPoints <*>
        newTVar Map.empty <*>
        newTVar Set.empty <*>
        pure taName <*> 
        pure now <*>
        dynamicPara (parallelism config) <*>
        newTVar (Stats 0)

oneMoreValid :: MonadIO m => TopDownContext -> m ()
oneMoreValid TopDownContext {..} = liftIO $ atomically $ 
    modifyTVar' objectStats $ \s -> s { validCount = validCount s + 1 }


-- | Initial bootstrap of the TA: do everything needed to start up the validator, 
-- | * download and parse TA certificate
-- | * fetch the repositories
bootstrapTA :: (Has AppContext env, Storage s) => 
                env -> TAL -> DB s -> IO (Either AppError (), Validations)
bootstrapTA env tal database = 
    runValidatorT taContext $ do         
        nextStep <- validateTACertificateFromTAL env tal database 
        case nextStep of
            SameTACert taCert repos     -> fetchAndValidate taCert repos
            UpdatedTACert newCert repos -> fetchAndValidate newCert repos        
    where

        fetchAndValidate taCert repos = do            
            parallelism' <- liftIO $ dynamicParaIO $ parallelism config
            fetchStatuses <- parallel parallelism' repos $ \repo -> do 
                logDebugM logger [i|Bootstrap, fetching #{repo} |]
                fetchRepository appContext database repo                

            logDebugM logger [i| fetchStatuses = #{fetchStatuses} |]                

            case partitionFailedSuccess (NonEmpty.toList fetchStatuses) of 
                ([], _) -> do
                    storedPubPoints <- roAppTxEx database storageError $ \tx -> 
                            getTaPublicationPoints tx (repositoryStore database) taName'

                    logDebugM logger [i| storedPubPoints = #{storedPubPoints} |]

                    let flattenedStatuses = flip NonEmpty.map fetchStatuses $ \case 
                            FetchFailure r s _ -> (r, s)
                            FetchSuccess r s _ -> (r, s)            

                    -- use publication points taken from the DB and updated with the 
                    -- the fetchStatuses of the fetch that we just performed
                    let fetchUpdatedPPs = updateStatuses storedPubPoints flattenedStatuses
                    topDownContext <- emptyTopDownContext appContext taName' fetchUpdatedPPs taCert
                    -- this is for TA cert
                    oneMoreValid topDownContext

                    logDebugM logger [i| fetchUpdatedPPs = #{fetchUpdatedPPs} |]

                    fromTry (UnspecifiedE . fmtEx) $
                        validateCA env taCertURI database topDownContext taCert                    

                    -- get publication points from the topDownContext and save it to the database
                    pubPointAfterTopDown <- liftIO $ readTVarIO (publicationPoints topDownContext)
                    let changeSet' = changeSet fetchUpdatedPPs pubPointAfterTopDown
                
                    Stats {..} <- liftIO $ readTVarIO (objectStats topDownContext)
                    logDebugM logger [i| TA: #{taName'} validCount = #{validCount} |]
                    
                    vResults <- roAppTx database $ \tx -> allVResults tx (resultStore database)
                    logDebugM logger [i| Validation results TA: #{taName'} = #{vResults} |]

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
validateTACertificateFromTAL :: (Has AppContext env, Has VContext vc, Storage s) => 
                                env -> TAL -> DB s -> ValidatorT vc IO TACertValidationResult
validateTACertificateFromTAL env tal DB {..} = do    
    (uri', ro) <- fetchTACertificate appContext tal
    newCert    <- vHoist $ validateTACert tal uri' ro      
    rwAppTxEx taStore storageError
        $ \tx -> do
            r <- getTA tx taStore taName'
            case r of
                Nothing -> do
                    -- it's a new TA, store it and trigger all the other actions
                    let c = cwsX509certificate $ getCertWithSignature newCert
                    logInfoM logger [i| Storing new #{taName'}, 
                        getRrdpNotifyUri newCert = #{getRrdpNotifyUri c}, 
                        getRepositoryUri newCert = #{getRepositoryUri c}|]
                                                            
                    storeTaCert tx newCert

                Just STA {..} ->
                    if getSerial taCert /= getSerial newCert
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
        Right (resultRepo, vs) ->
            pure $ FetchSuccess resultRepo (FetchedAt now) (vs <> v)


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

    logDebugM logger [i| Starting to validate #{NonEmpty.head $ getLocations certificate}|]    

    let validateAll = do
            treeValidations <- runValidatorT vContext' $ 
                    validateTree env database certificate topDownContext
            queueVResult appContext topDownContext $ snd treeValidations
            pure treeValidations

    -- Write validation results in a separate thread to avoid blocking on the 
    -- database with writing transactions during the validation process 
    z <- fst . fst <$> concurrently 
            (validateAll `finally` atomically (Q.writeTBQueue resultQueue Nothing))
            (writeVResults logger topDownContext resultStore)                

    -- logDebugM logger [i|validateCA, z = #{z} |]

    case z of 
        Left _ -> 
            -- Already reported the problem using `writeVResults` by now
            pure ()
        Right discoveredPPs -> do             
            ppsToFetch <- atomically $ do 
                    globalPPs           <- readTVar publicationPoints                    
                    alreadyTakenCareOf  <- readTVar takenCareOf

                    let newGlobalPPs     = globalPPs <> discoveredPPs
                    let discoveredURIs   = allURIs discoveredPPs
                    let urisToTakeCareOf = Set.difference discoveredURIs alreadyTakenCareOf

                    writeTVar publicationPoints newGlobalPPs                                        
                    modifyTVar' takenCareOf (<> discoveredURIs) 
                    
                    pure $ newGlobalPPs `shrinkTo` urisToTakeCareOf
            
            let (_, rootToPps) = repositoryHierarchy discoveredPPs
                
            let newRepositories = filter ((New ==) . repositoryStatus) $ Map.elems $ repositories ppsToFetch

            -- logDebugM logger [i|discoveredPPs = #{discoveredPPs}, 
            --                     ppsToFetch = #{ppsToFetch} 
            --                     repositories ppsToFetch = #{repositories ppsToFetch} 
            --                     |]

            forM_ newRepositories $ \r -> 
                logDebugM logger [i|new url = #{repositoryURI r}|]

            -- for all new repositories, drill down recursively
            void $ parallel parallelismDegree newRepositories $ \repo -> do 
                validations <- fetchAndValidateWaitingList appContext rootToPps repo
                queueVResult appContext topDownContext validations

            logDebugM logger [i| Finished validating #{NonEmpty.head $ getLocations certificate}|]   
        
    where

        fetchAndValidateWaitingList appContext@AppContext {..} rootToPps repo = do
            logDebugM logger [i|Fetching #{repo} |]
            result <- fetchRepository appContext database repo                                
            let statusUpdate = case result of
                    FetchFailure r s _ -> (r, s)
                    FetchSuccess r s _ -> (r, s)                

            -- logDebugM logger [i|statusUpdate = #{statusUpdate} |]

            atomically $ modifyTVar' publicationPoints $ \pubPoints -> 
                updateStatuses pubPoints [statusUpdate]            

            -- ppsAfterFetch <- readTVarIO publicationPoints                

            -- logDebugM logger [i|ppsAfterFetch = #{ppsAfterFetch} |]            

            case result of
                FetchFailure _ _ v -> pure v 
                FetchSuccess _ _ v -> do
                    let fetchedPPs = fromMaybe Set.empty $ Map.lookup repo rootToPps
                    waitingListPerPP <- readTVarIO ppWaitingList
                    -- logDebugM logger [i| waitingListPerPP = #{waitingListPerPP}|]

                    let waitingHashesForThesePPs  = fromMaybe Set.empty $ fold $ 
                            Set.map (\pp -> publicationPointURI pp `Map.lookup` waitingListPerPP) fetchedPPs

                    void $ parallel parallelismDegree (Set.toList waitingHashesForThesePPs) $ \hash -> do
                            o <- roTx database $ \tx -> getByHash tx objectStore hash
                            case o of 
                                Just (CerRO c) -> do
                                    -- logDebugM logger [i| #{getLocations c} was waiting for #{fetchedPPs}|]
                                    let certVContext = vContext $ NonEmpty.head $ getLocations c
                                    let childTopDownContext = topDownContext { 
                                            verifiedResources = Just $ createVerifiedResources certificate 
                                        }
                                    -- TODO void . try is weird, do something more meaningful here
                                    void (try $ validateCA appContext certVContext database childTopDownContext c :: IO (Either SomeException ()))
                                ro ->
                                    logErrorM logger
                                        [i| Something is really wrong with the hash #{hash} in waiting list, got #{ro}|]
                    pure v
        
    

-- | Do top-down validation starting from the given certificate
-- Returns the discovered publication points that are not registered 
-- in the top-down context yet.
validateTree :: (Has AppContext env, Storage s) =>
                env ->
                DB s ->
                CerObject ->
                TopDownContext ->
                ValidatorT VContext IO PublicationPoints
validateTree env database certificate topDownContext = do      
    let appContext@AppContext {..} = getter env    

    globalPPs <- liftIO $ readTVarIO $ publicationPoints topDownContext

    -- logDebugM logger [i| validateTree, globalPPs = #{globalPPs} |]                

    let validationConfig = appContext ^. typed @Config . typed @ValidationConfig

    case publicationPointsFromCertObject certificate of
        Left e                  -> appError $ ValidationE e
        Right (u, discoveredPP) -> 
            do            
            logDebugM logger [i| pp =  #{unURI u}, cert = #{getLocations certificate}
                publicationPoint = #{discoveredPP} 
                findPublicationPointStatus = #{findPublicationPointStatus u (discoveredPP `mergePP` globalPPs)} |]                

            case findPublicationPointStatus u (discoveredPP `mergePP` globalPPs) of 
                -- Both Nothing and Just New mean that it is a completely new piublication point, 
                -- it is not fetched yet or it hasn't been seen. Nothing to do here, just add  
                -- current certificate to the waiting list for this PP.                
                Nothing  -> stopHere discoveredPP
                Just New -> stopHere discoveredPP

                Just (FetchedAt time) 
                    | notTooLongAgo validationConfig time (now topDownContext) -> 
                        validateThisCertAndGoDown appContext                          
                    | otherwise  -> stopHere discoveredPP

                Just (FailedAt time)
                    | notTooLongAgo validationConfig time (now topDownContext) -> 
                            appError $ ValidationE $ PublicationPointIsNotAvailable $ publicationPointURI discoveredPP
                    | otherwise -> stopHere discoveredPP

    where
        stopHere discoveredPP = do 
            certificate `addToWaitingListOf` discoveredPP
            pure $ mergePP discoveredPP emptyPublicationPoints                

        addToWaitingListOf :: CerObject -> PublicationPoint -> ValidatorT vc IO ()
        addToWaitingListOf cert pp = liftIO $ atomically $           
            modifyTVar (ppWaitingList topDownContext) $ \m -> 
                Map.unionWith (<>) m (Map.singleton (publicationPointURI pp) (Set.singleton $ getHash cert))
             
        validateThisCertAndGoDown appContext@AppContext {..} = do
            vContext' :: VContext <- asks getter
            let (childrenAki, locations) = (toAKI $ getSKI certificate, getLocations certificate)        

            -- this for the certificate
            oneMoreValid topDownContext

            mft <- findMft childrenAki locations
            -- this for the manifest
            oneMoreValid topDownContext                                

            forChild (NonEmpty.head $ getLocations mft) $ do
                logDebugM logger [i|mft=#{getLocations mft}|]

                (_, crlHash) <- case findCrlOnMft mft of 
                    []    -> vError $ NoCRLOnMFT childrenAki locations
                    [crl] -> pure crl
                    crls  -> vError $ MoreThanOneCRLOnMFT childrenAki locations crls

                let objectStore' = objectStore database
                crlObject <- liftIO $ roTx objectStore' $ \tx -> getByHash tx objectStore' crlHash
                case crlObject of 
                    Nothing          -> vError $ NoCRLExists childrenAki locations    
                    Just (CrlRO crl) -> do      

                        validCrl <- forChild (NonEmpty.head $ getLocations crl) $ do    
                            logDebugM logger [i|crl=#{getLocations crl}|]
                            vHoist $ do          
                                crl' <- validateCrl (now topDownContext) crl certificate
                                void $ validateMft (now topDownContext) mft certificate crl'
                                pure crl'                                        

                        -- this for the CRL
                        oneMoreValid topDownContext                        
                            
                        -- TODO Check locations and give warnings if it's wrong
                        let childrenHashes = filter ( /= getHash crl) $ -- filter out CRL itself
                                                map snd $ mftEntries $ getCMSContent $ extract mft

                        logDebugM logger [i|childrenHashes size=#{length childrenHashes}|]

                        mftProblems <- parallel (parallelismDegree topDownContext) childrenHashes $ \h -> do            
                            ro <- roAppTx objectStore' $ \tx -> getByHash tx objectStore' h
                            case ro of 
                                Nothing  -> pure $ Left $ ManifestEntryDontExist h
                                Just ro' -> Right <$> liftIO (validateChild vContext' appContext validCrl ro')

                        -- TODO Here we should act depending on how strict we want to be,  
                        -- Interrupt the whole thing or just continue with a warning            
                        case mftProblems of
                            [] -> pure emptyPublicationPoints
                            _  -> do 
                                let (broken, pps) = partitionEithers mftProblems
                                mapM_ vWarn broken
                                pure $! mconcat pps 

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

        validateChild :: VContext -> AppContext -> Validated CrlObject -> RpkiObject -> IO PublicationPoints
        validateChild parentContext appContext@AppContext {..} validCrl ro = 
            case ro of
                CerRO childCert -> do 
                    (r, validations) <- runValidatorT childContext $ do
                            childVerifiedResources <- vHoist $ do                 
                                void $ validateResourceCert (now topDownContext) childCert certificate validCrl                
                                validateResources (verifiedResources topDownContext) childCert certificate 
                            validateTree env database childCert 
                                topDownContext { verifiedResources = Just childVerifiedResources }
                    queueVResult appContext topDownContext validations
                    pure $ case r of
                        Left _    -> emptyPublicationPoints
                        Right pps -> pps

                RoaRO roa -> withEmptyPPs $ do 
                                let (r, validations) = runPureValidator childContext $                                     
                                            void $ validateRoa (now topDownContext) roa certificate validCrl
                                queueVResult appContext topDownContext validations
                                case r of 
                                    Left _  -> pure ()
                                    Right _ -> do                                
                                        oneMoreValid topDownContext
                                        let vrps = getCMSContent (extract roa :: CMS [Roa])
                                        forM_ vrps $ \vrp ->
                                            logDebugM logger [i|vrp=#{vrp}, roa=#{NonEmpty.head $ getLocations roa} |]
                GbrRO gbr -> withEmptyPPs $ do
                                z <- queueVResult appContext topDownContext $ snd $ 
                                    runPureValidator childContext $ 
                                        void $ validateGbr (now topDownContext) gbr certificate validCrl
                                oneMoreValid topDownContext
                                pure z
                -- TODO Anything else?
                _ -> withEmptyPPs $ pure ()
            where
                childContext = childVContext parentContext childLocation 
                childLocation = NonEmpty.head $ getLocations ro

                withEmptyPPs f = f >> pure emptyPublicationPoints


-- | Put validation result into a queue for writing
queueVResult :: AppContext -> TopDownContext -> Validations -> IO ()
queueVResult AppContext {..} topDownContext validations = do
    logDebug_ logger [i|queueVResult=#{validations}|]
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
        -- TODO Fix it, saving it results in MDB_BAD_VALSIZE
        -- it most probably means the key size is wrong
        -- rwTx resultStore $ \tx -> putVResult tx resultStore vr


-- TODO Optimise so that it reads the queue in big chunks
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
    t <- liftIO $ try $ txF (storage ws) $ runValidatorT env . f
    validatorT $ pure $ either ((, mempty) . Left . err) id t

