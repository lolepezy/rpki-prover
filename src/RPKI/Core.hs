{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module RPKI.Core where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Except


import qualified Data.ByteString            as B
import           Data.Has
import qualified Data.List.NonEmpty         as NE

import           Data.X509
import           RPKI.AppMonad
import           RPKI.Errors
import           RPKI.Domain
import           RPKI.Logging
import           RPKI.Rsync
import           RPKI.TAL
import           RPKI.Store.Base.Storage
import           RPKI.Store.Stores
import           RPKI.Validation.Cert
import           RPKI.Util                  (convert, fmtEx)


validateTA :: (Has RsyncConf conf, Has AppLogger conf, Storage s) =>
              TAL -> 
              RpkiObjectStore s ->
              ValidatorT conf IO ()
validateTA tal objectStore = do
  (u, ro) <- fetchTACertificate tal
  pureToValidatorT $ validateTACert tal u ro
  fromTry (StorageE . StorageError . fmtEx) $ 
      rwTx (storage objectStore) $ \tx -> 
          putObject tx objectStore (getHash ro) (storableValue ro)
  pure ()


  -- | Fetch TA certificate based on TAL location(s)
fetchTACertificate :: (Has RsyncConf conf, Has AppLogger conf) => 
                      TAL -> ValidatorT conf IO (URI, RpkiObject)
fetchTACertificate tal = do
  logger :: AppLogger    <- asks getter
  rsyncConf :: RsyncConf <- asks getter
  lift $ go logger rsyncConf $ NE.toList $ certLocations tal
  where
    go _ _ []                      = throwE $ TAL_E $ TALError "No certificate location could be fetched."
    go logger rsyncConf (u : uris) = catchE ((u,) <$> rsync) $ \e -> do          
        lift2 $ logError_ logger $ convert $ "Failed to fetch " <> show u <> ": " <> show e
        go logger rsyncConf uris
      where 
        rsync = runReaderT (rsyncFile u validateSize) (logger, rsyncConf)        


validateSize :: B.ByteString -> PureValidator B.ByteString
validateSize bs = 
  case () of _
              | len < 10             -> pureError $ TACertificateIsTooSmall len
              | len > 10 * 1000*1000 -> pureError $ TACertificateIsTooBig len
              | otherwise            -> pure bs
  where len = B.length bs

