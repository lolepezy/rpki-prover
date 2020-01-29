{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

module RPKI.Core where

import           Control.Concurrent.Async
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.String.Interpolate

import qualified Data.ByteString                  as B
import           Data.Has
import qualified Data.List.NonEmpty               as NE
import qualified Data.Text                        as T

import           Data.Tuple.Ops

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Logging
import           RPKI.Resources
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
validateTree :: (Has AppLogger env, 
                 Has Now env, 
                 Has ValidationContext env,
                 Storage s) =>
                RpkiObjectStore s -> 
                CerObject ->
                TopDownContext ->
                ValidatorT env IO ()
validateTree objectStore certificate topDownContext = do  

  logger :: AppLogger  <- asks getter
  now :: Now           <- asks getter
  validationContext :: ValidationContext <- asks getter    

  let childrenAki = toAKI $ getSKI certificate
  let locations = getLocations certificate  

  mft <- validatorT $ roTx objectStore $ \tx -> 
    runValidatorT validationContext $
      lift3 (findLatestMftByAKI tx objectStore childrenAki) >>= \case
        Nothing  -> validatorError $ NoMFT childrenAki locations
        Just mft -> pure mft

  let crls = filter (\(name, _) -> ".crl" `T.isSuffixOf` name) $ 
        mftEntries $ getCMSContent $ extract mft

  (_, crlHash) <- case crls of 
    []    -> validatorError $ NoCRLOnMFT childrenAki locations
    [crl] -> pure crl
    _     -> validatorError $ MoreThanOneCRLOnMFT childrenAki locations

  crlObject <- lift3 $ roTx objectStore $ \tx -> getByHash tx objectStore crlHash
  case crlObject of 
    Nothing          -> validatorError $ NoCRLExists childrenAki locations    
    Just (CrlRO crl) -> do      
        validatedCrl <- pureToValidatorT $ do          
          vCrl <- validateCrl crl certificate
          validateMft mft certificate vCrl
          pure vCrl        
  
        let manifestChildren = map snd $ mftEntries $ getCMSContent $ extract mft    
        lift3 $ forM_ manifestChildren $ \h -> do
          ro <- roTx objectStore $ \tx -> getByHash tx objectStore h
          case ro of 
            Nothing  -> runValidatorT validationContext $ vWarn $ ManifestEntryDontExist h
            Just ro' -> validateChild (logger, now, topDownContext) validatedCrl ro'            
    Just _  -> validatorError $ CRLHashPointsToAnotherObject crlHash locations        
    where              
        validateChild parentContext validatedCrl ro = 
          case ro of 
            CerRO childCert -> runValidatorT childContext $ do
              childVerifiedResources <- pureToValidatorT $ do                 
                validateResourceCert childCert certificate validatedCrl
                validateResources (verifiedResources topDownContext) childCert certificate 
              validateTree objectStore childCert 
                (topDownContext { verifiedResources = Just childVerifiedResources })

            RoaRO roa -> pure $ runPureValidator childContext $ 
              void $ validateRoa roa certificate validatedCrl
            GbrRO gbr -> pure $ runPureValidator childContext $ 
              void $ validateGbr gbr certificate validatedCrl
            -- TODO Anything else?
            _ -> valid
            where
              childContext = childLocation `snocT` parentContext
              childLocation = vContext $ NE.head $ getLocations ro
  


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
      validatorWarning $ ValidationWarning e
      go uris
 
