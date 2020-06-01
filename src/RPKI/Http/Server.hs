{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module RPKI.Http.Server where

import           Control.Monad.IO.Class

import Data.Proxy
import Data.List (maximumBy)

import           Data.Aeson
import           Servant
import           Servant.API
import           Servant.API.Generic
import           Servant.Server.Generic
import           Servant.CSV.Cassava

import qualified Servant.Types.SourceT as S

import           RPKI.Http.Api
import           RPKI.Domain
import           RPKI.Execution
import           RPKI.Version
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import           RPKI.Resources.Types

validatorServer :: Storage s => AppContext s -> Server API
validatorServer AppContext { database = DB {..}} = 
    liftIO getVRPs
    :<|> liftIO getVRPs
    -- :<|> pure (S.source [])
    where
        getVRPs = 
            roTx versionStore $ \tx -> do 
                versions <- allVersions tx versionStore
                case versions of
                    [] -> pure []
                    vs -> do 
                        let lastVersion = maximum [ v | (v, FinishedVersion) <- vs ]
                        map toVRPs <$> allVRPs tx vrpStore lastVersion
        
        toVRPs (Roa a p len) = VRP a p len


httpApi :: Storage s => AppContext s -> Application
httpApi = serve api . validatorServer