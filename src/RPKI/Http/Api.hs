{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}

module RPKI.Http.Api where

import           Data.Text                   (Text)

import           Servant.API
import           Servant.API.Generic
import           Servant.HTML.Blaze (HTML)
import           Text.Blaze.Html5 (Html)

import           RPKI.Store.Types
import           RPKI.Http.Types
import           RPKI.SLURM.Types


data API api = API {        
        vrpsCsv  :: api :- "vrps.csv"  :> Get '[ManualCVS] RawCVS,
        vrpsJson :: api :- "vrps.json" :> Get '[JSON] [VrpDto],

        vrpsCsvFiltered  :: api :- "vrps-filtered.csv"  :> Get '[ManualCVS] RawCVS,        
        vrpsJsonFiltered :: api :- "vrps-filtered.json" :> Get '[JSON] [VrpDto],

        slurm :: api :- "slurm.json" :> Get '[JSON] Slurm,
                
        validationResultsMinimal :: api :- "validations"      :> Get '[JSON] (ValidationsDto MinimalVDto),
        fullValidationResults    :: api :- "validations-full" :> Get '[JSON] (ValidationsDto FullVDto),

        metrics :: api :- "metrics"        :> Get '[JSON] MetricsDto,
                
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
