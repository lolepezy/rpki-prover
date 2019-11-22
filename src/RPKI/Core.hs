{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
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
import           RPKI.Store.Stores
import           RPKI.TAL
import           RPKI.Util                  (convert, fmtEx)


validateTA :: (Has RsyncConf conf, 
               Has AppLogger conf,
               Storage s) => 
              TAL -> 
              RpkiObjectStore s ->
              ValidatorT conf IO ()
validateTA tal objectStore = do  
  logger :: AppLogger    <- asks getter
  rsyncConf :: RsyncConf <- asks getter
  fetchTACertificate logger rsyncConf >>= 
    \case
      ro@(RpkiObject RpkiMeta {..}  (CerRO (CerObject (ResourceCert taCert)))) -> do
        let spki = subjectPublicKeyInfo $ signedObject $ getSigned $ withRFC taCert certX509
        if publicKeyInfo tal == spki 
          then do
            -- TODO Check serial number and compare it with the local one
            -- If the serial is bigger than the local one -- update local 
            -- certificate and proceed with validating the tree
            fromTry (StorageE . StorageError . fmtEx) $ 
              rwTx (storage objectStore) $ \tx -> 
                putObject tx objectStore (getHash ro) (storableValue ro)
          else 
            lift $ throwE $ ValidationE $ SPKIMismatch (publicKeyInfo tal) spki
      _ -> 
        lift $ throwE $ ValidationE UnknownObjectAsTACert

  where        
    fetchTACertificate logger rsyncConf = lift $ go $ certLocations tal
      where
        go []         = throwE $ TAL_E $ TALError "No certificate location could be fetched."
        go (u : uris) = catchE (runReaderT (rsyncFile u checkForWeirdSizes) (logger, rsyncConf)) (\e -> do          
            lift2 $ logError_ logger $ convert $ "Failed to fetch " <> show u <> ": " <> show e
            go uris)
      
    checkForWeirdSizes :: B.ByteString -> Maybe ValidationError
    checkForWeirdSizes bs = 
      case () of _
                  | len < 10        -> Just $ TACertificateIsTooSmall len
                  | len > 1000*1000 -> Just $ TACertificateIsTooBig len
                  | otherwise       -> Nothing
      where len = B.length bs

