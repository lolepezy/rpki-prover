{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}

module RPKI.Cache where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Exception
  
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Maybe (maybe)

import Data.Hashable
import qualified StmContainers.Map as SM

import RPKI.Domain

data Store = Store

load :: Store -> SKI -> IO [RpkiObject]
load store ski = pure []

-- load :: (Eq k, Hashable k) =>
--         Cache k v -> k -> (k -> IO (Maybe v)) -> IO (Maybe v)
-- load (Cache cache) k loadIO = do
--     atomically zzz >>= \case 
--       DoIO io -> io
--       DoSTM s -> pure s
--     where
--       zzz = do
--         v <- SM.lookup k cache 
--         case v of        
--           Just Loading   -> retry
--           Just (Value v) -> pure $ DoSTM $ Just v
--           Nothing -> do
--             SM.insert Loading k cache
--             pure $ DoIO $ try (loadIO k) >>= \case 
--               Left e         -> throw e
--               Right Nothing  -> pure Nothing
--               Right (Just v) -> do 
--                 atomically (SM.insert (Value v) k cache)
--                 pure $ Just v      
          


