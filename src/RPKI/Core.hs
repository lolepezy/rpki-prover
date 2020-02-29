{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

module RPKI.Core where

import           Control.Concurrent.Async
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Concurrent.STM

import qualified Control.Concurrent.STM.TBQueue as Q

import           Data.String.Interpolate

import           Data.Has
import qualified Data.List.NonEmpty               as NE
import qualified Data.Text                        as T

import           Data.Tuple.Ops

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Logging
import           RPKI.Resources.Resources
import           RPKI.Parse.Parse
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
    repositoryQueue :: TBQueue URI
}

-- | Do top-down validation starting from the given object
-- TODO Resolve URL VContext to an actual URL instead of the rsync path.
validateTree :: (Has AppLogger env, 
                Has Now env, 
                Has VContext env,
                Storage s) =>
                RpkiObjectStore s -> 
                VResultStore s -> 
                CerObject ->
                TopDownContext ->
                ValidatorT env IO ()
validateTree objectStore resultStore certificate topDownContext = do  
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
                    queueVResult childContext $ 
                        runValidatorT childContext $ do
                        childVerifiedResources <- pureToValidatorT $ do                 
                            validateResourceCert childCert certificate validatedCrl                
                            validateResources (verifiedResources topDownContext) childCert certificate 
                        lift3 $ logDebug_ (sel childContext :: AppLogger) 
                            [i|Validated: #{sel childContext :: VContext}, resources: #{childVerifiedResources}.|]                
                        validateTree objectStore resultStore childCert 
                            (topDownContext { verifiedResources = Just childVerifiedResources })              

                RoaRO roa -> queueVResult childContext $ 
                                pure $ runPureValidator childContext $ 
                                    void $ validateRoa roa certificate validatedCrl
                GbrRO gbr -> queueVResult childContext $ 
                                pure $ runPureValidator childContext $ 
                                    void $ validateGbr gbr certificate validatedCrl
                -- TODO Anything else?
                _ -> pure ()
            where
                childContext = mapT (\pv -> childVContext pv childLocation) parentContext                
                childLocation = NE.head $ getLocations ro


        -- | Register URI of the repository for the given certificate if it's not there yet
        -- TODO Implement
        registerPublicationPoint (cwsX509certificate . getCertWithSignature -> cert) = 
            case getRrdpNotifyUri cert of
                Just rrdpNotify -> pure ()
                Nothing         -> 
                    case getRepositoryUri cert of
                        Nothing -> pure ()
                        Just repositoryUri -> pure ()

        queueVResult context f = do
            r <- f
            let vContext = sel context :: VContext
            let problems = case r of            
                    (Left e, ws)  -> map VWarn ws <> [VErr e]
                    (Right _, ws) -> map VWarn ws
            let q = resultQueue topDownContext
            atomically $ Q.writeTBQueue q $ VResult problems vContext

        writeVResult = do
            vr <- atomically $ Q.readTBQueue $ resultQueue topDownContext
            rwTx resultStore $ \tx -> putVResult tx resultStore vr



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

