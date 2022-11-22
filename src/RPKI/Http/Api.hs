{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE QuasiQuotes        #-}

module RPKI.Http.Api where

import           Control.Lens
import           Data.Text                   (Text)

import           Data.String.Interpolate.IsString

import           Servant.API
import           Servant.API.Generic
import           Servant.HTML.Blaze (HTML)
import           Text.Blaze.Html5 (Html)

import           Data.Swagger hiding (prefix)
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI

import           Data.Version
import qualified Paths_rpki_prover as Autogen

import qualified Data.HashMap.Strict.InsOrd as IOMap

import           RPKI.Config
import           RPKI.Store.Types
import           RPKI.Http.Types
import           RPKI.SLURM.Types
import           RPKI.Util (convert)


data API api = API {        
        vrpsCsv  :: api :- "vrps.csv"  :> Get '[ManualCVS] RawCSV,
        vrpsJson :: api :- "vrps" :> Get '[JSON] [VrpDto],

        vrpsCsvFiltered  :: api :- "vrps-filtered.csv"  :> Get '[ManualCVS] RawCSV,        
        vrpsJsonFiltered :: api :- "vrps-filtered" :> Get '[JSON] [VrpDto],

        aspas :: api :- "aspa" :> Get '[JSON] [AspaDto],

        slurm :: api :- "slurm" :> Get '[JSON] Slurm,
                
        validationResultsMinimal :: api :- "validations"      :> Get '[JSON] (ValidationsDto MinimalVDto),
        fullValidationResults    :: api :- "validations-full" :> Get '[JSON] (ValidationsDto FullVDto),

        metrics :: api   :- "metrics" :> Get '[JSON] MetricsDto,                

        lmdbStats :: api :- "lmdb-stats" :> Get '[JSON] TotalDBStats,
        jobs :: api      :- "jobs" :> Get '[JSON] JobsDto,
        config :: api    :- "config" :> Get '[JSON] Config,

        publicationsPoints :: api :- "repositories" :> Get '[JSON] PublicationPointDto,        

        objectView :: api :- "object" :> QueryParam "uri" Text 
                                    :> QueryParam "hash" Text 
                                    :> Get '[JSON] [RObject]
    }
    deriving (Generic)

data HttpApi route = HttpApi {
        api     :: route :- "api" :> ToServant API AsApi,
        metrics :: route :- "metrics" :> Get '[PlainText] Text,
        staticContent :: route :- "static" :> Raw,
        ui            :: route :- Get '[HTML] Html,
        swagger       :: route :- SwaggerSchemaUI "swagger-ui" "swagger.json"
    }
    deriving (Generic)


-- It is located in this module to have API documentation 
-- in the same place as the definition.
-- 
swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy (ToServantApi API))
    & info.title    .~ "RPKI Prover API"
    & info.version  .~ convert (showVersion Autogen.version)
    & info.description  ?~ ("Note: at the moment this API does not generate a proper API schema, " <> 
                            "this UI is only good for documentation and examples." )
    & basePath          ?~ "/api"
    & paths .~ IOMap.fromList 
        [ 
            ("/vrps.csv", mempty & get ?~ csvOn200 "CSV-formatted list of VRPs with filtering applied"),
            ("/vrps", mempty & get ?~ jsonOn200 "JSON-formatted list of VRPs with filtering applied"),
            
            ("/vrps-filtered.csv", mempty & get ?~ csvOn200 
                "CSV-formatted list of VRPs with SLURM filtering applied to it"),
            ("/vrps-filtered.csv", mempty & get ?~ jsonOn200 
                "List of VRPs with SLURM filtering applied to it"),

            ("/aspa", mempty & get ?~ jsonOn200 "List of all ASPA objects"),

            ("/validations", mempty & get ?~ jsonOn200 
                "Returns validation result for the last validation run"),

            ("/validations-full", mempty & get ?~ jsonOn200 
                [i|Returns validation result for the last validation run, but every 
                object represented with full URL path in the RPKI tree, starting from TA.|]),

            ("/metrics", mempty & get ?~ jsonOn200 
                "Returns metrics for the latest validation run (object counts, repository statuses, etc.)"),

            ("/repositories", mempty & get ?~ jsonOn200 "Returns statuses of the repositories"),
        
            ("/slurm", mempty & get ?~ (jsonOn200 
                        "Returns SLURM (RFC 8416) that is set using --local-exceptions option"
                    & at 404 ?~ "SLURM is not set using --local-exceptions")),

            ("/lmdb-stats", mempty & get ?~ jsonOn200 "Returns LMDB cache statistics"),
            ("/jobs", mempty & get ?~ jsonOn200 "Returns list of scheduled jobs"),
            ("/config", mempty & get ?~ jsonOn200 "Returns config that is used for the process")
        ] 
  where                
    jsonOn200 txt = mempty
                    & produces ?~ MimeList ["application/json"]
                    & at 200 ?~ txt
    csvOn200 txt = mempty
                    & produces ?~ MimeList ["text/csv"]
                    & at 200 ?~ txt