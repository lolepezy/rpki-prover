{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

module RPKI.Core where

import           Control.Concurrent.Async
import           Control.Monad.Except
import           Control.Monad.Reader

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
  (u, ro) <- fetchTACertificate tal
  newCert <- pureToValidatorT $ validateTACert tal u ro  
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
  verifiedResources :: Maybe (VerifiedRS PrefixesAndAsns)
}

-- | Do top-down validation starting from the given object
-- TODO Resolve URL VContext to an actual URL instead of the rsync path.
validateTree :: (Has AppLogger env, 
                Has Now env, 
                Has VContext env,
                Storage s) =>
                RpkiObjectStore s -> 
                CerObject ->
                TopDownContext ->
                ValidatorT env IO ()
validateTree objectStore certificate topDownContext = do  

    logger :: AppLogger  <- asks getter
    now :: Now           <- asks getter
    vContext :: VContext <- asks getter    

    let childrenAki = toAKI $ getSKI certificate
    let locations = getLocations certificate  

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
              Just ro' -> lift3 $ validateChild (logger, now, topDownContext) validatedCrl ro'            

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
          saveResult childContext $ 
            runValidatorT childContext $ do
              childVerifiedResources <- pureToValidatorT $ do                 
                validateResourceCert childCert certificate validatedCrl                
                validateResources (verifiedResources topDownContext) childCert certificate 
              lift3 $ logDebug_ (sel childContext :: AppLogger) 
                  [i|Validated: #{sel childContext :: VContext}, resources: #{childVerifiedResources}.|]                
              validateTree objectStore childCert 
                  (topDownContext { verifiedResources = Just childVerifiedResources })              

        RoaRO roa -> saveResult childContext $ 
                        pure $ runPureValidator childContext $ 
                            void $ validateRoa roa certificate validatedCrl
        GbrRO gbr -> saveResult childContext $ 
                        pure $ runPureValidator childContext $ 
                            void $ validateGbr gbr certificate validatedCrl
        -- TODO Anything else?
        _ -> pure ()
      where
        childContext = childLocation `snocT` parentContext
        childLocation = vContext $ NE.head $ getLocations ro

    saveResult context f = 
        f >>= \case
          (Left e, w) -> do
              logDebug_ (sel context :: AppLogger) [i|Validated: #{sel context :: VContext}, result: #{e}.|]
          (Right _, w) -> pure ()
      



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

