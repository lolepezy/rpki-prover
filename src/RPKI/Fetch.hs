{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}

{-# LANGUAGE DerivingStrategies #-}
module RPKI.Fetch where
    
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM

import           Control.Lens                     ((%~), (&), (+=), (^.))
import           Data.Generics.Product.Fields
import           Data.Generics.Product.Typed

import           Data.Bifunctor

import           Control.Concurrent.STM
import           Control.Exception.Lifted

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class

import qualified Data.ByteString                  as BS
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map

import GHC.Generics (Generic)

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import           RPKI.Time
import qualified RPKI.Util                        as U
import           RPKI.Validation.ObjectValidation
import           RPKI.AppState


data FetchResult = 
    FetchSuccess Repository Instant ValidationState | 
    FetchFailure Repository Instant ValidationState
    deriving stock (Show, Eq, Generic)

data FetchTasks = FetchTasks {
    tasks :: Map RpkiURL (Async FetchResult)
}


-- fetch :: FetchTasks -> [RpkiURL] -> 