{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE DerivingVia                #-}

module RPKI.TopDown where

import           Control.Concurrent.STM
import           Control.Concurrent.Async (forConcurrently)
import           Control.Exception.Lifted
import           Control.Monad.Except
import           Control.Monad.Reader

import           Control.Lens
import           Data.Generics.Product.Typed
import           Data.Generics.Product.Fields
import           GHC.Generics (Generic)


import           Data.Either                      (fromRight, partitionEithers)
import           Data.Foldable
import qualified Data.Set.NonEmpty                as NESet
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Monoid.Generic
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text
import           Data.Tuple.Strict
import           Data.Proxy

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Fetch
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.Resources.Resources
import           RPKI.Resources.Types
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import           RPKI.Store.Repository
import           RPKI.Store.Types
import           RPKI.TAL
import           RPKI.Time
import           RPKI.Util                        (fmtEx, ifJust)
import           RPKI.Validation.ObjectValidation
import           RPKI.AppState
import           RPKI.Metrics

-- Auxiliarry structure used in top-down validation. It has a lot of global variables 
-- but it's lifetime is limited to one top-down validation run.
data TopDownContext s = TopDownContext {    
        verifiedResources    :: Maybe (VerifiedRS PrefixesAndAsns),    
        taName               :: TaName,         
        now                  :: Now,
        worldVersion         :: WorldVersion,
        validManifests       :: TVar (Map AKI Hash),
        visitedHashes        :: TVar (Set Hash),
        repositoryProcessing :: RepositoryProcessing
    }
    deriving stock (Generic)


data TopDownResult = TopDownResult {
        vrps               :: Vrps,
        topDownValidations :: ValidationState
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving Semigroup via GenericSemigroup TopDownResult   
    deriving Monoid    via GenericMonoid TopDownResult


fromValidations :: ValidationState -> TopDownResult
fromValidations = TopDownResult mempty


newTopDownContext :: MonadIO m => 
                    WorldVersion 
                    -> TaName                     
                    -> Now 
                    -> CerObject 
                    -> RepositoryProcessing
                    -> m (TopDownContext s)
newTopDownContext worldVersion taName now certificate repositoryProcessing = 
    liftIO $ atomically $ do    
        let verifiedResources = Just $ createVerifiedResources certificate        
        visitedHashes     <- newTVar mempty
        validManifests    <- newTVar mempty        
        pure $ TopDownContext {..}

newRepositoryContext :: PublicationPoints -> RepositoryContext
newRepositoryContext publicationPoints = let 
    takenCareOf = Set.empty 
    in RepositoryContext {..}

createVerifiedResources :: CerObject -> VerifiedRS PrefixesAndAsns
createVerifiedResources (getRC -> ResourceCertificate certificate) = 
    VerifiedRS $ toPrefixesAndAsns $ withRFC certificate resources


-- | It is the main entry point for the top-down validation. 
-- Validates a bunch of TA starting from their TALs.  
validateMutlipleTAs :: Storage s => 
                    AppContext s 
                    -> WorldVersion 
                    -> [TAL]
                    -> IO [TopDownResult]
validateMutlipleTAs appContext@AppContext {..} worldVersion tals = do                    
    database' <- readTVarIO database 

    repositoryProcessing <- newRepositoryProcessingIO 

    validateThem database' repositoryProcessing 
        `finally` 
        cancelFetchTasks repositoryProcessing

  where

    validateThem database' repositoryProcessing = do 
        -- set initial publication point state
        mapException (AppException . storageError) $ 
            roTx database' $ \tx -> do 
                pps <- getPublicationPoints tx (repositoryStore database')    
                atomically $ writeTVar (repositoryProcessing ^. #publicationPoints) pps
        
        rs <- forConcurrently tals $ \tal -> do           
            (r@TopDownResult{..}, elapsed) <- timedMS $ validateTA appContext tal worldVersion repositoryProcessing
            logInfo_ logger [i|Validated #{getTaName tal}, got #{vrpCount vrps} VRPs, took #{elapsed}ms|]
            pure r    

        -- save publication points state    
        mapException (AppException . storageError) $
            rwTx database' $ \tx -> do                             
                pps <- readTVarIO $ repositoryProcessing ^. #publicationPoints    
                savePublicationPoints tx (repositoryStore database') pps
        
        -- Get validations for all the fetches that happened during this top-down traversal
        fetchValidation <- validationStateOfFetches repositoryProcessing
        pure $ fromValidations fetchValidation : rs


--
validateTA :: Storage s => 
            AppContext s 
            -> TAL 
            -> WorldVersion             
            -> RepositoryProcessing 
            -> IO TopDownResult
validateTA appContext tal worldVersion repositoryProcessing = do    
    r <- runValidatorT taContext $
            timedMetric (Proxy :: Proxy ValidationMetric) $ do 
                ((taCert, repos, _), elapsed) <- timedMS $ validateTACertificateFromTAL appContext tal worldVersion
                -- this will be used as the "now" in all subsequent time and period validations 
                let now = Now $ versionToMoment worldVersion
                topDownContext <- newTopDownContext worldVersion 
                                    taName
                                    now  
                                    (taCert ^. #payload)  
                                    repositoryProcessing
                vrps <- validateFromTACert appContext topDownContext repos taCert
                setVrpNumber $ Count $ fromIntegral $ Set.size vrps                
                pure vrps

    case r of 
        (Left e, vs) -> 
            pure $ TopDownResult mempty vs
        (Right vrps, vs) ->            
            pure $ TopDownResult (newVrps taName vrps) vs
  where
    taName = getTaName tal
    taContext = newValidatorPath $ unTaName taName


data TACertStatus = Existing | Updated

-- | Fetch and validated TA certificate starting from the TAL.
-- | 
-- | This function doesn't throw exceptions.
validateTACertificateFromTAL :: Storage s => 
                                AppContext s 
                                -> TAL 
                                -> WorldVersion 
                                -> ValidatorT IO (Located CerObject, PublicationPointAccess, TACertStatus)
validateTACertificateFromTAL appContext@AppContext {..} tal worldVersion = do
    let now = Now $ versionToMoment worldVersion
    let validationConfig = config ^. typed @ValidationConfig

    taStore  <- taStore <$> liftIO (readTVarIO database)
    taByName <- roAppTxEx taStore storageError $ \tx -> getTA tx taStore (getTaName tal)
    case taByName of
        Nothing -> fetchValidateAndStore taStore now
        Just StorableTA { taCert, initialRepositories, fetchStatus }
            | needsFetching (getTaCertURL tal) fetchStatus validationConfig now ->
                fetchValidateAndStore taStore now
            | otherwise -> do
                logInfoM logger [i|Not re-fetching TA certificate #{getTaCertURL tal}, it's up-to-date.|]
                pure (locatedTaCert (getTaCertURL tal) taCert, initialRepositories, Existing)
    where
        fetchValidateAndStore taStore (Now moment) = do 
            (uri', ro) <- fetchTACertificate appContext tal
            cert       <- vHoist $ validateTACert tal uri' ro            
            case publicationPointsFromTAL tal cert of
                Left e      -> appError $ ValidationE e
                Right ppAccess -> 
                    rwAppTxEx taStore storageError $ \tx -> do 
                        putTA tx taStore (StorableTA tal cert (FetchedAt moment) ppAccess)
                        pure (locatedTaCert uri' cert, ppAccess, Updated)
             
        locatedTaCert url cert = Located (toLocations url) cert


-- | Do the validation starting from the TA certificate.
-- | 
-- | This function doesn't throw exceptions.
validateFromTACert :: Storage s =>
                    AppContext s -> 
                    TopDownContext s ->                                        
                    PublicationPointAccess -> 
                    Located CerObject ->                     
                    ValidatorT IO (Set Vrp)
validateFromTACert 
    appContext@AppContext {..} 
    topDownContext@TopDownContext { .. } 
    initialRepos@(PublicationPointAccess initialAccess) 
    taCert 
    = do  
    
    let taURIContext = newValidatorPath $ locationsToText $ taCert ^. #locations
    
    unless (appContext ^. typed @Config . typed @ValidationConfig . #dontFetch) $ do
        -- Merge the main repositories in first, all these PPs will be stored 
        -- in `#publicationPoints` with the status 'Pending'
        liftIO $ atomically $ modifyTVar' 
                    (repositoryProcessing ^. #publicationPoints)
                    (\pubPoints -> foldr mergePP pubPoints initialAccess) 
        
        -- ignore return result here, because all the fetching statuses will be
        -- handled afterwards by getting them from `repositoryProcessing` 
        void $ fetchPPWithFallback appContext repositoryProcessing now initialRepos    
        
    -- Do the tree descend, gather validation results and VRPs            
    T2 vrps validationState <- fromTry 
                (\e -> UnspecifiedE (unTaName taName) (fmtEx e)) 
                (validateCA appContext taURIContext topDownContext taCert)

    embedState validationState    
    pure vrps
         

-- | Validate CA starting from its certificate.
-- 
validateCA :: Storage s =>
            AppContext s 
            -> ValidatorPath 
            -> TopDownContext s 
            -> Located CerObject 
            -> IO (T2 (Set Vrp) ValidationState)
validateCA appContext validatorPath topDownContext certificate =
    validateCARecursively 
        `finally`  
        markValidatedObjects appContext topDownContext       
  where
    validateCARecursively = do             
        (r, validations) <- runValidatorT validatorPath $
                validateCaCertificate appContext topDownContext certificate
        pure $ case r of
            Left _     -> T2 mempty validations
            Right vrps -> T2 vrps validations         

    

validateCaCertificate :: Storage s =>
                        AppContext s ->
                        TopDownContext s ->
                        Located CerObject ->                
                        ValidatorT IO (Set Vrp)
validateCaCertificate 
    appContext@AppContext {..} 
    topDownContext@TopDownContext {..} 
    certificate = do          
    
    if appContext ^. typed @Config . typed @ValidationConfig . #dontFetch 
        -- Don't add anything with the pending repositories
        -- Just expect all the objects to be already cached        
        then validateThisCertAndGoDown 
        else 
            case getPublicationPointsFromCertObject (certificate ^. #payload) of            
                Left e         -> appError $ ValidationE e
                Right ppAccess -> do                    
                    fetches <- fetchPPWithFallback appContext repositoryProcessing now ppAccess                                   
                    if anySuccess fetches                    
                        then validateThisCertAndGoDown                            
                        else do                             
                            fetchEverSucceeded repositoryProcessing ppAccess >>= \case                        
                                Never       -> pure mempty
                                AtLeastOnce -> validateThisCertAndGoDown
  where    
    -- Here we do the following
    -- 
    --  1) get the latest manifest (latest by the validatory period)
    --  2) find CRL on it
    --  3) make sure they both are valid
    --  4) go through the manifest children and either 
    --     + validate them as signed objects
    --     + or valdiate them recursively as CA certificates
    -- 
    -- If anything falled, try to fetch previously latest cached 
    -- valid manifest and repeat (2) - (4) for it.

    -- Everything else is either extra checks or metrics.
    -- 
    validateThisCertAndGoDown = do            
        let childrenAki    = toAKI $ getSKI certificate
        let certLocations = getLocations certificate        
        
        validateObjectLocations certificate

        oneMoreCert            
        visitObject appContext topDownContext (CerRO $ certificate ^. #payload)                                                    

        -- first try to use the latest manifest 
        -- https://tools.ietf.org/html/draft-ietf-sidrops-6486bis-03#section-6.2                                     
        findLatestMft childrenAki >>= \case                        
            Nothing -> 
                -- Use awkward vError + catchError to force the error to 
                -- get into the ValidationResult in the state.
                vError (NoMFT childrenAki certLocations)
                    `catchError`
                    (\e -> do                         
                        tryLatestValidCachedManifest Nothing childrenAki certLocations e)
                
            Just mft -> 
                tryManifest mft childrenAki certLocations
                    `catchError` 
                    tryLatestValidCachedManifest (Just mft) childrenAki certLocations

      where                       

        tryManifest mft childrenAki certLocations = do             
            validateManifestAndItsChildren mft childrenAki certLocations
                `finallyError`
                -- manifest should be marked as visited regardless of its validitity
                visitObject appContext topDownContext mft               

        tryLatestValidCachedManifest latestMft childrenAki certLocations e =
            -- this "fetch" has failed so we are falling back to a latest valid 
            -- cached manifest for this CA               
            -- https://tools.ietf.org/html/draft-ietf-sidrops-6486bis-03#section-6.7
            --
            findCachedLatestValidMft childrenAki >>= \case
                Nothing             -> throwError e
                Just latestValidMft ->                         
                    case latestMft of 
                        Nothing -> do 
                            appWarn e      
                            logDebugM logger [i|Failed to process manifest: #{e}, will try previous valid version.|]
                            tryManifest latestValidMft childrenAki certLocations                                
                        Just latestMft'
                            | getHash latestMft' == getHash latestValidMft 
                                -- it doesn't make sense to try the same manifest again
                                -- just re-trow the error
                                -> throwError e
                            | otherwise -> do 
                                appWarn e                                    
                                logWarnM logger [i|Failed to process latest valid manifest: #{e}, fetch is invalid.|]
                                tryManifest latestValidMft childrenAki certLocations


        validateManifestAndItsChildren locatedMft childrenAki certLocations = do                         
            let mft = locatedMft ^. #payload            

            visitedObjects <- liftIO $ readTVarIO visitedHashes            
            when (getHash mft `Set.member` visitedObjects) $                 
                -- We have already visited this manifest before, so 
                -- there're some circular references in the objects.
                -- 
                -- NOTE: We are limiting cycle detection only to manfests
                -- to minimise the false positives where the same object
                -- is referenced from multiple manifests and we are treating 
                -- it as a cycle.
                vError $ CircularReference (getHash mft) (locatedMft ^. #locations)

            -- General location validation
            validateObjectLocations locatedMft

            -- Manifest-specific location validation
            validateMftLocation locatedMft certificate

            manifestResult <- inSubVPath (locationsToText $ locatedMft ^. #locations) $ do                

                -- vPath :: ValidatorPath <- asks (^. typed)
                -- logDebugM logger [i|Manifest = #{vPath}.|]

                T2 _ crlHash <- 
                    case findCrlOnMft mft of 
                        []    -> vError $ NoCRLOnMFT childrenAki certLocations
                        [crl] -> pure crl
                        crls  -> vError $ MoreThanOneCRLOnMFT childrenAki certLocations crls

                objectStore' <- (^. #objectStore) <$> liftIO (readTVarIO database)
                crlObject <- liftIO $ roTx objectStore' $ \tx -> getByHash tx objectStore' crlHash
                case crlObject of 
                    Nothing -> 
                        vError $ NoCRLExists childrenAki certLocations    

                    Just foundCrl@(Located crlLocations (CrlRO crl)) -> do      
                        visitObject appContext topDownContext foundCrl                        
                        validateObjectLocations foundCrl
                        validCrl <- inSubVPath (locationsToText crlLocations) $ 
                                        vHoist $ do        
                                            let mftEECert = getEECert $ unCMS $ cmsPayload mft
                                            checkCrlLocation foundCrl mftEECert
                                            validateCrl now crl certificate
                        oneMoreCrl

                        -- MFT can be revoked by the CRL that is on this MFT -- detect 
                        -- revocation as well                               
                        void $ vHoist $ validateMft now mft 
                                            certificate validCrl verifiedResources
                                            
                        -- Validate entry list and filter out CRL itself
                        nonCrlChildren <- validateMftEntries mft (getHash crl)

                        -- Mark all manifest entries as visited to avoid the situation
                        -- when some of the children are deleted from the cache and some
                        -- are still there. Do it both in case of successful validation
                        -- or a validation error.
                        let markAllEntriesAsVisited = 
                                visitObjects topDownContext $ map (\(T2 _ h) -> h) nonCrlChildren                                        
                        
                        vp <- askEnv
                        let processChildren = do 
                                -- this indicates the difeerence between RFC6486-bis 
                                -- version 02 (strict) and version 03 and later (more loose).                                                                                            
                                let gatherMftEntryValidations = 
                                        case config ^. #validationConfig . #manifestProcessing of
                                            {- 
                                            The latest version so far of the 
                                            https://datatracker.ietf.org/doc/draft-ietf-sidrops-6486bis/06/                                            
                                            item 6.4 says
                                                "If there are files listed in the manifest that cannot be retrieved 
                                                from the publication point, the fetch has failed.." 

                                            For that case validity of every object on the manifest is completely 
                                            separate from each other and don't influence the manifest validity.
                                            -}
                                            RFC6486 -> independentMftChildrenResults

                                            {- 
                                            https://datatracker.ietf.org/doc/draft-ietf-sidrops-6486bis/02/
                                            item 6.4 says
                                                "If there are files listed in the manifest that cannot be retrieved 
                                                from the publication point, or if they fail the validity tests 
                                                specified in [RFC6488], the fetch has failed...". 

                                            For that case invalidity of some of the objects (all except certificates) 
                                            on the manifest make the whole manifest invalid.
                                            -}
                                            RFC6486_Strict -> allOrNothingMftChildrenResults

                                useMftEntryResults =<< gatherMftEntryValidations nonCrlChildren validCrl                                                                       

                        mconcat <$> processChildren `finallyError` markAllEntriesAsVisited                                                

                    Just _ -> 
                        vError $ CRLHashPointsToAnotherObject crlHash certLocations   

            oneMoreMft
            addValidMft topDownContext childrenAki mft
            pure manifestResult            

    allOrNothingMftChildrenResults nonCrlChildren validCrl = do
        vp <- askEnv
        liftIO $ inParallel
            (cpuBottleneck appBottlenecks <> ioBottleneck appBottlenecks)
            nonCrlChildren
            $ \(T2 filename hash') -> runValidatorT vp $ do 
                    ro <- findManifestEntryObject filename hash' 
                    -- if failed this one interrupts the whole MFT valdiation
                    validateMftObject ro hash' filename validCrl                

    independentMftChildrenResults nonCrlChildren validCrl = do
        vp <- askEnv
        liftIO $ inParallel
            (cpuBottleneck appBottlenecks <> ioBottleneck appBottlenecks)
            nonCrlChildren
            $ \(T2 filename hash') -> do 
                (r, vs) <- runValidatorT vp $ findManifestEntryObject filename hash' 
                case r of 
                    Left e   -> pure (Left e, vs)
                    Right ro -> do 
                        -- We are cheating here a little by faking the empty VRP set.
                        -- 
                        -- if failed this one will result in the empty VRP set
                        -- while keeping errors and warning are in the `vs'` value.
                        (z, vs') <- runValidatorT vp $ validateMftObject ro hash' filename validCrl
                        pure $ case z of                             
                            Left _ -> (Right mempty, vs')
                            _      -> (z, vs')     

    useMftEntryResults mftEntryResults = do                 
        -- gather all the validation states from every MFT entry
        mapM_ (embedState . snd) mftEntryResults                

        case partitionEithers $ map fst mftEntryResults of
            ([], vrps) -> pure vrps
            (e : _, _) -> appError e

    -- Check manifest entries as a whole, without doing anything 
    -- with the objects they are pointing to.    
    validateMftEntries mft crlHash = do         
        let children = mftEntries $ getCMSContent $ cmsPayload mft
        let nonCrlChildren = filter (\(T2 _ hash') -> crlHash /= hash') children
                    
        -- Make sure all the entries are unique
        let entryMap = Map.fromListWith (<>) $ map (\(T2 f h) -> (h, [f])) nonCrlChildren
        let nonUniqueEntries = Map.filter longerThanOne entryMap

        -- Don't crash here, it's just a warning, at the moment RFC doesn't say anything 
        -- about uniqueness of manifest entries.
        unless (Map.null nonUniqueEntries) $ 
            vWarn $ NonUniqueManifestEntries $ Map.toList nonUniqueEntries

        pure nonCrlChildren
        where
            longerThanOne [_] = False
            longerThanOne []  = False            
            longerThanOne _   = True


    validateMftObject ro hash' filename validCrl = do
        -- warn about names on the manifest mismatching names in the object URLs
        let objectLocations = getLocations ro
        let nameMatches = NESet.filter ((filename `Text.isSuffixOf`) . toText) $ unLocations objectLocations
        when (null nameMatches) $ 
            vWarn $ ManifestLocationMismatch filename objectLocations

        -- Validate the MFT entry, i.e. validate a ROA/GBR/etc.
        -- or recursively validate CA if the child is a certificate.                           
        validateChild validCrl ro

    
    findManifestEntryObject filename hash' = do                    
        validateMftFileName filename                         
        ro <- liftIO $ do 
            objectStore' <- (^. #objectStore) <$> readTVarIO database
            roTx objectStore' $ \tx -> getByHash tx objectStore' hash'
        case ro of 
            Nothing  -> vError $ ManifestEntryDoesn'tExist hash' filename
            Just ro' -> pure ro'


    allowedMftFileNameCharacters = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "-_"
    validateMftFileName filename =                
        case Text.splitOn "." filename of 
            [ mainName, extension ] -> do                    
                unless (isSupportedExtension $ Text.toLower extension) $ 
                    vError $ BadFileNameOnMFT filename 
                                ("Unsupported filename extension " <> extension)

                unless (Text.all (`elem` allowedMftFileNameCharacters) mainName) $ do 
                    let badChars = Text.filter (`notElem` allowedMftFileNameCharacters) mainName
                    vError $ BadFileNameOnMFT filename 
                                ("Unsupported characters in filename: '" <> badChars <> "'")

            somethingElse -> 
                vError $ BadFileNameOnMFT filename 
                            "Filename doesn't have exactly one DOT"            

    
    validateChild validCrl child@(Located locations ro) = do
        -- At the moment of writing RFC 6486-bis 
        -- (https://tools.ietf.org/html/draft-ietf-sidrops-6486bis-03#page-12) 
        -- prescribes to consider the manifest invalid if any of the objects 
        -- referred by the manifest is invalid. 
        -- 
        -- That's why _only_ recursive validation of the child CA happens in the separate   
        -- runValidatorT (...) call, but all the other objects are validated within the 
        -- same context of ValidatorT, i.e. have short-circuit logic implemented by ExceptT.        
        --
        parentContext <- ask        
        case ro of
            CerRO childCert -> do 
                let TopDownContext{..} = topDownContext
                (r, validationState) <- liftIO $ runValidatorT parentContext $                     
                        inSubVPath (toText $ pickLocation locations) $ do                                
                            childVerifiedResources <- vHoist $ do                 
                                    Validated validCert <- validateResourceCert 
                                            now childCert (certificate ^. #payload) validCrl
                                    validateResources verifiedResources childCert validCert
                            let childTopDownContext = topDownContext { 
                                    verifiedResources = Just childVerifiedResources 
                                }                            
                            validateCaCertificate appContext childTopDownContext (Located locations childCert)                            

                embedState validationState
                pure $ fromRight mempty r                

            RoaRO roa -> do 
                    validateObjectLocations child
                    inSubVPath (locationsToText locations) $ 
                        allowRevoked $ do
                            void $ vHoist $ validateRoa now roa certificate validCrl verifiedResources
                            oneMoreRoa
                            
                            let vrps = getCMSContent $ cmsPayload roa                            
                            -- logDebugM logger [i|Roa #{vPath}, vrps = #{length vrps}.|]
                            pure $ Set.fromList vrps

            GbrRO gbr -> do                
                    validateObjectLocations child
                    inSubVPath (locationsToText locations) $ 
                        allowRevoked $ do
                            void $ vHoist $ validateGbr now gbr certificate validCrl verifiedResources
                            oneMoreGbr
                            pure mempty

            -- TODO Anything else?
            _ -> pure mempty

        where                
            -- In case of RevokedResourceCertificate error, the whole manifest is not be considered 
            -- invalid, only the object with the revoked certificate is considered invalid.
            -- This is a slightly ad-hoc code, but works fine.
            allowRevoked f =                
                catchAndEraseError f isRevokedCertError $ do 
                    vWarn RevokedResourceCertificate
                    pure mempty
                where                 
                    isRevokedCertError (ValidationE RevokedResourceCertificate) = True
                    isRevokedCertError _ = False


    findLatestMft childrenAki = liftIO $ do 
        objectStore' <- (^. #objectStore) <$> readTVarIO database
        roTx objectStore' $ \tx -> 
            findLatestMftByAKI tx objectStore' childrenAki

    findCachedLatestValidMft childrenAki = liftIO $ do 
        objectStore' <- (^. #objectStore) <$> readTVarIO database
        roTx objectStore' $ \tx -> 
            getLatestValidMftByAKI tx objectStore' childrenAki


    -- TODO Is there a more reliable way to find it?
    findCrlOnMft mft = filter (\(T2 name _) -> ".crl" `Text.isSuffixOf` name) $
        mftEntries $ getCMSContent $ cmsPayload mft


    -- | Check that manifest URL in the certificate is the same as the one 
    -- the manifest was actually fetched from.
    validateMftLocation mft certficate = 
        case getManifestUri $ cwsX509certificate $ getCertWithSignature certficate of
            Nothing     -> vError $ NoMFTSIA $ getLocations certficate
            Just mftSIA -> do 
                let mftLocations = getLocations mft
                when (Set.null $ NESet.filter ((mftSIA ==) . getURL) $ unLocations mftLocations) $ 
                    vWarn $ MFTOnDifferentLocation mftSIA mftLocations                    

    -- | Validate that the object has only one location: if not, 
    -- it's generally is a warning, not really an error.
    validateObjectLocations (getLocations -> locs@(Locations locSet)) =
        inSubVPath (locationsToText locs) $ 
            when (NESet.size locSet > 1) $ 
                vWarn $ ObjectHasMultipleLocations $ neSetToList locSet

    -- | Check that CRL URL in the certificate is the same as the one 
    -- the CRL was actually fetched from. 
    -- 
    checkCrlLocation crl eeCert = 
        ifJust (getCrlDistributionPoint $ cwsX509certificate eeCert) $ \crlDP -> do
            let crlLocations = getLocations crl
            when (Set.null $ NESet.filter ((crlDP ==) . getURL) $ unLocations crlLocations) $ 
                vError $ CRLOnDifferentLocation crlDP crlLocations


-- Mark validated objects in the database, i.e.
-- 
-- - save all the visited hashes together with the current world version
-- - save all the valid manifests for each CA/AKI
-- 
markValidatedObjects :: (MonadIO m, Storage s) => 
                        AppContext s -> TopDownContext s -> m ()
markValidatedObjects AppContext { .. } TopDownContext {..} = liftIO $ do
    ((visitedSize, validMftsSize), elapsed) <- timedMS $ do 
            (vhs, vmfts, objectStore') <- atomically $ (,,) <$> 
                                readTVar visitedHashes <*> 
                                readTVar validManifests <*>
                                ((^. #objectStore) <$> readTVar database)

            rwTx objectStore' $ \tx -> do 
                for_ vhs $ \h -> 
                    markValidated tx objectStore' h worldVersion 
                for_ (Map.toList vmfts) $ \(aki, h) -> 
                    markLatestValidMft tx objectStore' aki h

            pure (Set.size vhs, Map.size vmfts)

    logInfo_ logger 
        [i|Marked #{visitedSize} objects as used, #{validMftsSize} manifests as valid, took #{elapsed}ms.|]



-- Do whatever is required to notify other subsystems that the object was touched 
-- during top-down validation. It doesn't mean that the object is valid, just that 
-- we read it from the database and looked at it. It will be used to decide when 
-- to GC this object from the cache -- if it's not visited for too long, it is 
-- removed.
visitObject :: (MonadIO m, WithHash ro, Storage s) => 
                AppContext s -> TopDownContext s -> ro -> m ()
visitObject _ topDownContext ro = 
    visitObjects topDownContext [getHash ro]    

visitObjects :: MonadIO m => TopDownContext s -> [Hash] -> m ()
visitObjects TopDownContext {..} hashes =
    liftIO $ atomically $ modifyTVar' visitedHashes (<> Set.fromList hashes)


-- Add manifest to the map of the valid ones
addValidMft :: (MonadIO m, Storage s) => 
                TopDownContext s -> AKI -> MftObject -> m ()
addValidMft TopDownContext {..} aki mft = 
    liftIO $ atomically $ modifyTVar' 
                validManifests (<> Map.singleton aki (getHash mft))    

oneMoreCert, oneMoreRoa, oneMoreMft, oneMoreCrl, oneMoreGbr :: Monad m => ValidatorT m ()
oneMoreCert = updateMetric @ValidationMetric @_ (& #validCertNumber %~ (+1))
oneMoreRoa  = updateMetric @ValidationMetric @_ (& #validRoaNumber %~ (+1))
oneMoreMft  = updateMetric @ValidationMetric @_ (& #validMftNumber %~ (+1))
oneMoreCrl  = updateMetric @ValidationMetric @_ (& #validCrlNumber %~ (+1))
oneMoreGbr  = updateMetric @ValidationMetric @_ (& #validGbrNumber %~ (+1))

setVrpNumber :: Monad m => Count -> ValidatorT m ()
setVrpNumber n = updateMetric @ValidationMetric @_ (& #vrpNumber .~ n)


-- Sum up all the validation metrics from all TA to create 
-- the "alltrustanchors" validation metric
addTotalValidationMetric :: (HasType ValidationState s, HasField' "vrps" s Vrps) => s -> s
addTotalValidationMetric totalValidationResult = 
    totalValidationResult 
        & vmLens %~ Map.insert (newPath allTAsMetricsName) totalValidationMetric
  where    
    totalValidationMetric = mconcat (Map.elems $ totalValidationResult ^. vmLens) 
                            & #vrpNumber .~ Count 
                                (fromIntegral $ vrpCount $ totalValidationResult ^. #vrps)

    vmLens = typed @ValidationState . 
            typed @AppMetric . 
            #validationMetrics . 
            #unMetricMap . 
            #getMonoidalMap    
    