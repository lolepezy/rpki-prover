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

module RPKI.Core where

import           Control.Monad
import           Control.Monad.Reader

import qualified Data.List                         as L
import qualified Data.Text                         as T
import Data.Has

import           Data.X509
import           RPKI.Domain
import           RPKI.Parse.Parse
import           RPKI.TAL
import           RPKI.Store.Stores
import           RPKI.Rsync
import           RPKI.Logging
import           RPKI.Util (convert)


validateTA :: (Has RsyncConf conf, Has AppLogger conf) => 
              TAL -> 
              ReaderT conf IO (Either SomeError ())
validateTA tal = do  
  logger :: AppLogger <- asks getter 
  fetchTACertificate logger >>= \case
    Left e             -> pure $ Left $ TAL_E e
    Right (RpkiObject _ (CerRO (CerObject (ResourceCert taCert)))) -> do
      let spki = subjectPublicKeyInfo $ signedObject $ getSigned $ withRFC taCert certX509
      if publicKeyInfo tal == spki 
        then do
          pure $ Right ()
        else 
          pure $ Left $ ValidationE $ SPKIMismatch (publicKeyInfo tal) spki
    _ -> 
      pure $ Left $ ValidationE UnknownObjectAsTACert

  where
    fetchTACertificate logger = go $ certLocations tal
      where
        go [] = pure $ Left $ TALError "No certificate location could be fetched."
        go (u : uris) = rsyncFile u >>= \case 
          Left e -> do 
            lift $ logError_ logger $ convert $ "Failed to fetch " <> show u <> ": " <> show e
            go uris
          Right o -> pure $ Right o
