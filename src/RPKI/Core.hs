{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module RPKI.Core where

import qualified Data.DList                        as DL
import qualified Data.List                         as L
import qualified Data.Map                          as M
import           Data.Maybe                        (maybe)
import qualified Data.Text                         as T

import           Data.Data                         (Typeable)
import           Data.Kind                         (Type)

import           Control.Monad.Trans.Writer.Strict

import qualified StmContainers.Map                 as SM

import           Control.Concurrent.STM.TQueue

import           RPKI.Domain
import           RPKI.TAL


validateTA :: TAL -> IO ()
validateTA tal = pure ()