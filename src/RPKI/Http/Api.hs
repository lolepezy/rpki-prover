{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeOperators         #-}

module RPKI.Http.Api where


import           Data.Text                   (Text)

import qualified Data.Csv                    as Csv

import           Servant.API
import           Servant.CSV.Cassava


import           RPKI.Reporting

import           RPKI.Store.Database
import RPKI.Http.Types


data CSVOptions = CSVOptions

instance EncodeOpts CSVOptions where
    encodeOpts _ = Csv.defaultEncodeOptions { Csv.encUseCrLf = False } 

type CSVType = CSV' 'HasHeader CSVOptions

type API = "api" :> (     
                 "vrps.csv"  :> Get '[CSVType] [VrpDto]
            :<|> "vrps.json" :> Get '[JSON] [VrpDto]
            :<|> "validation-results" :> Get '[JSON] [ValidationResult]
            :<|> "app-metrics"    :> Get '[JSON] AppMetric
            :<|> "lmdb-stats" :> Get '[JSON] DBStats
            :<|> "object"     :> QueryParam "uri" Text 
                              :> QueryParam "hash" Text 
                              :> Get '[JSON] (Maybe RObject)                        
        )

type PrometheusAPI = "metrics" :> Get '[PlainText] Text 