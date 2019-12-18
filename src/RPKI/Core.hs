{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

module RPKI.Core where

import           Control.Concurrent.Async
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.String.Interpolate

import qualified Data.ByteString            as B
import qualified Data.Text                  as T
import           Data.Has
import qualified Data.List.NonEmpty         as NE

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Logging
import           RPKI.Rsync
import           RPKI.Store.Base.Storage
import           RPKI.Store.Stores
import           RPKI.TAL
import           RPKI.Util                  (fmtEx)
import           RPKI.Validation.Objects


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


-- | Do top-down validation starting from the given object
validateTree :: (Has AppLogger env, 
                 Has Now env, 
                 Has ValidationContext env,
                 Storage s) =>
                RpkiObjectStore s -> 
                RpkiObject ->                
                ValidatorT env IO ()
validateTree objectStore (CerRO rc) = do

  logger :: AppLogger  <- asks getter
  now :: Now           <- asks getter
  validationContext :: ValidationContext <- asks getter

  let childrenAki = toAKI $ getSKI rc
  let RpkiMeta {..} = getMeta rc

  mft <- validatorT $ roTx objectStore $ \tx -> 
    runValidatorT validationContext $
      lift3 (findMftsByAKI tx objectStore childrenAki) >>= \case
        []        -> validatorError $ NoMFT childrenAki locations
        (mft : _) -> pure mft

  let crls = filter (\(name, _) -> ".crl" `T.isSuffixOf` name) $ 
        mftEntries $ getCMSContent $ snd mft

  (_, crlHash) <- case crls of 
    []    -> validatorError $ NoCRLOnMFT childrenAki locations
    [crl] -> pure crl
    _     -> validatorError $ MoreThanOneCRLOnMFT childrenAki locations

  crlObject <- lift3 $ roTx objectStore $ \tx -> getByHash tx objectStore crlHash
  case crlObject of 
    Nothing          -> validatorError $ NoCRLExists childrenAki locations    
    Just (CrlRO crl) -> do      
        validatedCrl <- pureToValidatorT $ do
          vCrl <- validateCrl crl (snd rc)
          validateMft mft (snd rc) vCrl
          pure vCrl
  
        let manifestChildren = map snd $ mftEntries $ getCMSContent $ snd mft
  
        {- TODO narrow down children traversal to the ones that  
          1) Are on the manifest
          or
          2) Are (or include as EE) the latest 
          certificates with the unique (non-overlaping) set of reseources        
        -}      
        let childContext = (logger, now, )
        lift3 $ forM_ manifestChildren $ \h -> do
          ro <- roTx objectStore $ \tx -> getByHash tx objectStore h
          case ro of 
            Nothing  -> runValidatorT validationContext $ vWarn $ ManifestEntryDontExist h
            Just ro' -> validateChild childContext validatedCrl ro'
          pure ()
    Just _  -> validatorError $ CRLHashPointsToAnotherObject crlHash locations        
    where              
        validateChild childContext validatedCrl ro = case ro of 
          cer@(CerRO _) -> 
            runValidatorT childValidationContext $ validateTree objectStore cer
          RoaRO roa -> pure $ runPureValidator childValidationContext $ 
            void $ validateRoa roa (snd rc) validatedCrl
          GbrRO gbr -> pure $ runPureValidator childValidationContext $ 
            void $ validateGbr gbr (snd rc) validatedCrl
          -- TODO Anything else?
          _ -> valid
          where
            childValidationContext = childContext $ vContext $ NE.head $ getLocations ro
  


  -- | Fetch TA certificate based on TAL location(s 
fetchTACertificate :: (Has RsyncConf env, Has AppLogger env) => 
                      TAL -> ValidatorT env IO (URI, RpkiObject)
fetchTACertificate tal = 
  go $ NE.toList $ certLocations tal
  where
    go []         = throwError $ TAL_E $ TALError "No certificate location could be fetched."
    go (u : uris) = ((u,) <$> rsyncFile u validateSize) `catchError` \e -> do          
      logger :: AppLogger <- asks getter
      let message = [i| Failed to fetch #{u}: #{e}|]
      lift3 $ logError_ logger message
      validatorWarning $ ValidationWarning e
      go uris


validateSize :: B.ByteString -> PureValidator c B.ByteString
validateSize bs = 
  case () of _
              | len < 10         -> pureError $ TACertificateIsTooSmall len
              | len > 10_000_000 -> pureError $ TACertificateIsTooBig len
              | otherwise        -> pure bs
  where len = B.length bs

