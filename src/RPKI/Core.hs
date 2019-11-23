{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module RPKI.Core where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict


import           Data.Has
import qualified Data.List                  as L
import qualified Data.Text                  as T
import qualified Data.ByteString            as B

import           Data.X509
import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Logging
import           RPKI.Parse.Parse
import           RPKI.Rsync
import           RPKI.Store.Base.Storage
import           RPKI.TAL
import           RPKI.Util                  (convert)


validateTAL :: (Has RsyncConf conf, Has AppLogger conf) => 
              TAL -> 
              ValidatorT conf IO TA
validateTAL tal = do  
  logger :: AppLogger    <- asks getter
  rsyncConf :: RsyncConf <- asks getter
  fetchTACertificate logger rsyncConf >>= 
    \case
      (u, RpkiObject RpkiMeta {..}  (CerRO (CerObject rc@(ResourceCert taCert)))) -> do
        let spki = subjectPublicKeyInfo $ signedObject $ getSigned $ withRFC taCert certX509
        if publicKeyInfo tal == spki 
          then pure $ TA {
                taName = TaName $ case caName tal of
                            Nothing -> unURI u
                            Just ca -> ca,
                taCertificate = rc,
                taUri = u,
                taSpki = SPKI spki
              }                
          else 
            validatorError $ SPKIMismatch (publicKeyInfo tal) spki
      _ -> 
        validatorError UnknownObjectAsTACert
  where        
    fetchTACertificate logger rsyncConf = lift $ go $ certLocations tal
      where
        go []         = throwE $ TAL_E $ TALError "No certificate location could be fetched."
        go (u : uris) = catchE ((u,) <$> rsync) $ \e -> do          
            lift2 $ logError_ logger $ convert $ "Failed to fetch " <> show u <> ": " <> show e
            go uris
            where
              rsync = runReaderT (rsyncFile u checkForWeirdSizes) (logger, rsyncConf)

      
    checkForWeirdSizes :: B.ByteString -> Maybe ValidationError
    checkForWeirdSizes bs = 
      case () of _
                  | len < 10        -> Just $ TACertificateIsTooSmall len
                  | len > 1000*1000 -> Just $ TACertificateIsTooBig len
                  | otherwise       -> Nothing
      where len = B.length bs

