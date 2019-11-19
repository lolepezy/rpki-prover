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
{-# LANGUAGE OverloadedStrings #-}

module RPKI.Core where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict


import           Data.Has
import qualified Data.List                  as L
import qualified Data.Text                  as T

import           Data.X509
import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Logging
import           RPKI.Parse.Parse
import           RPKI.Rsync
import           RPKI.Store.Stores
import           RPKI.TAL
import           RPKI.Util                  (convert)


validateTA :: (Has RsyncConf conf, Has AppLogger conf) => 
              TAL -> 
              ValidatorT conf IO ()
validateTA tal = do  
  logger :: AppLogger    <- asks getter
  rsyncConf :: RsyncConf <- asks getter 
  object <- lift $ withExceptT TAL_E $ ExceptT $ fetchTACertificate logger rsyncConf
  
  case object of    
    RpkiObject _ (CerRO (CerObject (ResourceCert taCert))) -> do
      let spki = subjectPublicKeyInfo $ signedObject $ getSigned $ withRFC taCert certX509
      if publicKeyInfo tal == spki 
        then do
          pure ()
        else 
          lift $ throwE $ ValidationE $ SPKIMismatch (publicKeyInfo tal) spki
    _ -> 
      lift $ throwE $ ValidationE UnknownObjectAsTACert

  where        
    fetchTACertificate logger rsyncConf = go $ certLocations tal
      where
        go [] = pure $ Left $ TALError "No certificate location could be fetched."
        go (u : uris) = toEither (logger, rsyncConf) (rsyncFile u) >>= \case 
          Left e -> do 
            lift $ logError_ logger $ convert $ "Failed to fetch " <> show u <> ": " <> show e
            go uris
          Right o -> pure $ Right o
