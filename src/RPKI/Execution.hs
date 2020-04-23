{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module RPKI.Execution where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Except

import           Control.Concurrent.STM

import           Data.String.Interpolate

import qualified Data.ByteString            as BS
import           Data.Has
import qualified Data.List.NonEmpty         as NonEmpty

import           RPKI.AppMonad
import           RPKI.Errors
import           RPKI.Domain
import           RPKI.Repository
import           RPKI.Logging
import           RPKI.Rsync
import           RPKI.TAL
import           RPKI.Store.Base.Storage
import           RPKI.Store.Stores
import           RPKI.Util                  (convert, fmtEx)


data Task = CheckTACertificate (TAL -> Task) |
            FetchRepository (Repository -> Task) |
            FetchAndValidate (Repository -> Task) |
            TaskSequence [Task] |
            Noop



init :: TAL -> [Task]
init tal = [
        CheckTACertificate (\_ -> Noop)
    ]


{-
  - TA certificate valdiation triggers the whole tree re-validation

  - Adding a *new* publication point from a certificate triggers repository validation

  - Repository validation means
    * download the point (or lookup for already prefetched)
    * re-validate something -- figure out what (in RIPE NCC 
        validator it's just re-validate all relevant TAs)
        
  - Adding an object should trigger
    * Validating the object against it's parent
    * Validating the whole tree down from the object

  - 

-} 