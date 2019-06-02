{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


module RPKI.RRDP.Update where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.ST
import           Data.STRef

import           Control.Lens               ((^.))

import           Control.Monad.Primitive    (PrimMonad (..), stToPrim)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except

import qualified Data.ByteString            as B
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Lazy       as BL
import           Data.Char                  (chr, isSpace)
import qualified Data.List                  as L
import qualified Data.Map                   as M
import           Data.String.Utils          (strip)
import qualified Data.Text                  as T
import qualified Network.Wreq               as WR

import           Data.Hex                   (unhex)
import           Data.Word

import           Data.IORef
import           Text.Read                  (readMaybe)

import           Xeno.SAX                   as X

import           RPKI.Domain
import           RPKI.RRDP.Types
import           RPKI.RRDP.Parse
import           RPKI.Util                  (convert)


-- Download the notification
getNotification :: URI -> IO (Either RrdpError Notification)
getNotification (URI uri) = do
    try (WR.get $ T.unpack uri) >>= \case 
        Left (SomeException e) -> pure $ Left RrdpGeneralProblem
        Right r -> pure $ parseNotification $ r ^. WR.responseBody

-- TODO Replace RrdpGeneralProblem with something more concrete
-- TODO Add logging
-- TODO Add actual saving to the storage
updateLocalState :: Repository 'Rrdp -> IO (Either RrdpError (Repository 'Rrdp))
updateLocalState repo@(RrdpRepo uri sid serial) = do
    getNotification uri >>= \case
        Left e -> pure $ Left RrdpGeneralProblem -- complain
        Right notification ->
            if (sessionId notification /= sid) then
                try (processSnapshot notification)  >>= \case 
                    Left (SomeException e)  -> pure $ Left RrdpGeneralProblem -- complain
                    Right _ -> pure $ Right repo
            else 
                try (processDeltas notification) >>= \case
                    Left (SomeException e) -> do
                        -- complain
                        try (processSnapshot notification) >>= \case 
                            Left (SomeException e)  -> pure $ Left RrdpGeneralProblem -- complain
                            Right _ -> pure $ Right repo
                    Right _ -> pure $ Right repo
    where 
        processSnapshot notification = pure repo
        processDeltas notification = pure repo
