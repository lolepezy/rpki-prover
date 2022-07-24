{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE DerivingVia                #-}

module RPKI.Validation.BottomUp where

import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad.Except
import           Control.Monad.Reader

import           Control.Lens
import           Data.Generics.Product.Typed
import           Data.Generics.Product.Fields
import           GHC.Generics (Generic)

import           Data.Either                      (fromRight, partitionEithers)
import           Data.Foldable
import qualified Data.Set.NonEmpty                as NESet
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import qualified Data.Map.Monoidal.Strict         as MonoidalMap
import           Data.Monoid.Generic
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text
import           Data.Tuple.Strict
import           Data.Proxy

import           UnliftIO.Async

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Fetch
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.Resources.Resources
import           RPKI.Resources.Types
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import           RPKI.Store.Types
import           RPKI.TAL
import           RPKI.Time
import           RPKI.Util (fmtEx, fmtLocations)
import           RPKI.Validation.ObjectValidation
import           RPKI.Validation.Common
import           RPKI.AppState


validateBottomUp :: Storage s => 
                AppContext s 
                -> RpkiObject
                -> ValidatorT IO ()
validateBottomUp 
    appContext@AppContext{..}
    object = do 
    db@DB {..} <- liftIO $ readTVarIO database
    case getAKI object of 
        Nothing  -> appError $ ValidationE NoAKI
        Just (AKI ki) -> do 
            parentCert <- roAppTx db $ \tx -> getBySKI tx db (SKI ki)
            case parentCert of 
                Nothing -> appError $ ValidationE ParentCertificateNotFound
                Just c  -> do
                    validateManifest c                                        
                    pure ()
            pure ()
  where
    validateManifest certificate = do
        let childrenAki   = toAKI $ getSKI certificate
        let certLocations = getLocations certificate        
        -- first try to use the latest manifest 
        -- https://tools.ietf.org/html/draft-ietf-sidrops-6486bis-03#section-6.2                                     
        maybeMft <- findLatestMft database childrenAki           
        case maybeMft of 
            Nothing -> 
                -- Use awkward vError + catchError to force the error to 
                -- get into the Validations in the state.
                vError (NoMFT childrenAki certLocations)
                    `catchError`
                    tryLatestValidCachedManifest appContext useManifest maybeMft childrenAki certLocations
                
            Just mft -> 
                useManifest mft childrenAki certLocations
                    `catchError` 
                    tryLatestValidCachedManifest appContext useManifest maybeMft childrenAki certLocations

        pure ()

    useManifest mft childrenAki certLocations = do 
        pure ()


