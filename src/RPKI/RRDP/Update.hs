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
    r <- WR.get $ T.unpack uri
    pure $ parseNotification $ r ^. WR.responseBody

-- updateLocalState :: Repository 'Rrdp -> IO ()
-- updateLocalState (RrdpRepo uri sessionId serial) = do
--     getNotification uri >>= \case
--         Left e -> pure () -- complain
--         Right notification -> do            
--             if (sessionId notification <> sessionId) then
--                 processSnapshot notification
--             else do
--                 try (processDeltas notification) 
--                     `catch` 
--                     (\e -> processSnapshot notification) -- complain

--             pure ()
