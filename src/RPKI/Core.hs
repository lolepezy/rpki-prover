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
import           RPKI.Validation.Cert


-- | Valiidate TAL
validateTA :: (Has RsyncConf conf, Has AppLogger conf, Storage s) =>
              TAL -> 
              TAStore s ->
              ValidatorT conf IO ()
validateTA tal taStore = do
  logger :: AppLogger <- asks getter
  (u, ro) <- fetchTACertificate tal
  cert <- pureToValidatorT $ validateTACert tal u ro  
  fromTryEither (StorageE . StorageError . fmtEx) $ 
    rwTx taStore $ \tx -> do
      let newMeta = getMeta ro
      -- It has to be IO a, so return IO (Either SomeError ()) here
      getTA tx taStore (getTaName tal) >>= \case
        Nothing -> Right <$> do
          logInfo_ logger [i| Storing new #{getTaName tal} |]
          putTA tx taStore (StoredTA tal cert newMeta)
          -- TODO Trigger tree validation
        Just (StoredTA _ _ oldMeta) -> 
          if serial oldMeta < serial newMeta 
          then Right <$> do            
            logInfo_ logger [i| Updating TA certificate for #{getTaName tal} |]
            putTA tx taStore (StoredTA tal cert newMeta)            
            -- TODO Trigger tree validation
          else 
            pure $ Left $ ValidationE $ TACertificateLocalIsNewer (serial oldMeta) (serial newMeta)


-- | Do top-down validation starting from the given object
validateTree :: (Has RsyncConf conf, Has AppLogger conf, Storage s) =>
                RpkiObject ->
                RpkiObjectStore s -> 
                ValidatorT conf IO ()
validateTree (RpkiObject meta (CerRO cert@(CerObject (ResourceCert rc)))) objectStore = do
  -- find children
  -- findByAKI objectStore $ toAKI $ ski meta
  -- find MFT
  -- find CRL
  -- 
  pure ()


  -- | Fetch TA certificate based on TAL location(s)
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

