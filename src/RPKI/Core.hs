{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

module RPKI.Core where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Except

import           Data.String.Interpolate

import           Data.Foldable

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
validateTA :: (Has RsyncConf conf, Has AppLogger conf, Storage s) =>
              TAL -> 
              TAStore s ->
              ValidatorT conf IO ()
validateTA tal taStore = do
  logger :: AppLogger <- asks getter
  (u, ro) <- fetchTACertificate tal
  WithMeta newMeta cert <- pureToValidatorT $ validateTACert tal u ro  
  fromTry (StorageE . StorageError . fmtEx) $ 
    rwTx taStore $ \tx -> do      
      getTA tx taStore (getTaName tal) >>= \case
        Nothing -> do
          -- it's a new TA, store it
          logInfo_ logger [i| Storing new #{getTaName tal} |]
          putTA tx taStore (StoredTA tal cert newMeta)
          -- TODO Trigger tree validation
        Just (StoredTA _ _ oldMeta) -> 
          when (serial oldMeta /= serial newMeta) $ do            
            logInfo_ logger [i| Updating TA certificate for #{getTaName tal} |]
            putTA tx taStore (StoredTA tal cert newMeta)            
            -- TODO Trigger tree validation          


-- | Do top-down validation starting from the given object
validateTree :: (Has AppLogger conf, Storage s) =>
                RpkiObject ->
                RpkiObjectStore s -> 
                ValidatorT conf IO ()
validateTree ro@(RpkiObject (WithMeta meta (CerRO cert@(ResourceCert rc)))) objectStore = do

  logger :: AppLogger <- asks getter
  let aki = toAKI $ ski meta

  mftCms <- fromIOEitherSt $ roTx objectStore $ \tx -> 
    runValidatorT () $
      lift3 (findMftsByAKI tx objectStore aki) >>= \case
        []        -> validatorError $ NoMFT aki (getLocations ro)
        (mft : _) -> pure mft

  let crls = filter (\(name, _) -> ".crl" `T.isSuffixOf` name) $ 
        mftEntries $ getCMSContent mftCms

  (_, crlHash) <- case crls of 
    []    -> validatorError $ NoCRLOnMFT aki (getLocations ro)    
    [crl] -> pure crl
    _     -> validatorError $ MoreThanOneCRLOnMFT aki (getLocations ro)    

  crlObject <- lift3 $ roTx objectStore $ \tx -> getByHash tx objectStore crlHash
  case crlObject of 
    Nothing              -> validatorError $ NoCRLExists aki (getLocations ro)        
    Just (RpkiObject _ ) -> validatorError $ CRLHashPointsToAnotherObject crlHash (getLocations ro)
    Just crl@(RpkiCrl _) -> do
      pure ()

  
  
  


  -- | Fetch TA certificate based on TAL location(s 
fetchTACertificate :: (Has RsyncConf conf, Has AppLogger conf) => 
                      TAL -> ValidatorT conf IO (URI, RpkiObject)
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


validateSize :: B.ByteString -> PureValidator B.ByteString
validateSize bs = 
  case () of _
              | len < 10         -> pureError $ TACertificateIsTooSmall len
              | len > 10_000_000 -> pureError $ TACertificateIsTooBig len
              | otherwise        -> pure bs
  where len = B.length bs

