{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module RPKI.Http.Server where

import Data.Proxy

import           Data.Aeson
import           Servant
import           Servant.API
import           Servant.API.Generic
import           Servant.Server.Generic
import           Servant.CSV.Cassava

import qualified Servant.Types.SourceT as S

import           RPKI.Http.Api
import           RPKI.Domain
import           RPKI.Resources.Types

validatorServer :: Server API
validatorServer = 
    pure allVRPs
    :<|> pure allVRPs
    -- :<|> pure (S.source [])
    where
        allVRPs = replicate 200000 (VRP (ASN 123) prefix 18)
        prefix = Ipv4P $ Ipv4Prefix (read "10.0.0.0/16")


httpApi :: Application
httpApi = serve api validatorServer