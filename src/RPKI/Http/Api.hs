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
import           Servant.API.Generic
import           Servant.CSV.Cassava
import           Servant.HTML.Blaze (HTML)
import           Text.Blaze.Html5 (Html)

import           RPKI.Reporting
import           RPKI.Store.Types
import           RPKI.Http.Types
import           RPKI.SLURM.Types


data CSVOptions = CSVOptions

instance EncodeOpts CSVOptions where
    encodeOpts _ = Csv.defaultEncodeOptions { Csv.encUseCrLf = False } 

type CSVType = CSV' 'HasHeader CSVOptions

data API api = API {
        vrpsCsv  :: api :- "vrps.csv"  :> Get '[CSVType] [VrpDto],
        vrpsJson :: api :- "vrps.json" :> Get '[JSON] [VrpDto],

        vrpsCsvFiltered  :: api :- "vrps-filtered.csv"  :> Get '[CSVType] [VrpDto],
        vrpsJsonFiltered :: api :- "vrps-filtered.json" :> Get '[JSON] [VrpDto],

        slurm :: api :- "slurm.json" :> Get '[JSON] Slurm,
                
        validationResults :: api :- "validation-results" :> Get '[JSON] [ValidationResult],
        appMetrics        :: api :- "app-metrics"        :> Get '[JSON] AppMetric,
                
        lmdbStats :: api :- "lmdb-stats" :> Get '[JSON] TotalDBStats,

        objectView :: api :- "object" :> QueryParam "uri" Text 
                                    :> QueryParam "hash" Text 
                                    :> Get '[JSON] [RObject]
    }
    deriving (Generic)

data HttpApi route = HttpApi {
        api     :: route :- "api" :> ToServant API AsApi,
        metrics :: route :- "metrics" :> Get '[PlainText] Text,
        staticContent :: route :- "static" :> Raw,
        ui            :: route :- Get '[HTML] Html
    }
    deriving (Generic)