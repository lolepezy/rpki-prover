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

import qualified Data.HashMap.Strict.InsOrd as IOMap

import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Store.Types
import           RPKI.Http.Types
import           RPKI.SLURM.Types
import           RPKI.Util (convert)


data API api = API {        
        vrpsCsv  :: api :- "vrps.csv"  :> QueryParam "version" Text 
                                       :> Get '[ManualCVS] RawCSV,

        vrpsJson :: api :- "vrps" :> QueryParam "version" Text 
                                  :> Get '[JSON] [VrpDto],

        vrpsCsvFiltered  :: api :- "vrps-filtered.csv" :> QueryParam "version" Text 
                                                       :> Get '[ManualCVS] RawCSV,        
        vrpsJsonFiltered :: api :- "vrps-filtered" :> QueryParam "version" Text 
                                                   :> Get '[JSON] [VrpDto],

        vrpsCsvUnique :: api :- "vrps-unique.csv" :> QueryParam "version" Text 
                                                  :> Get '[ManualCVS] RawCSV,

        vrpsJsonUnique :: api :- "vrps-unique" :> QueryParam "version" Text 
                                                :> Get '[JSON] [VrpMinimalDto],

        aspas    :: api :- "aspa" :> Get '[JSON] [AspaDto],
        bgpCerts :: api :- "bgp-certificates" :> Get '[JSON] [BgpCertDto],

        slurm :: api :- "slurm" :> Get '[JSON] Slurm,
                
        validationResultsMinimal :: api :- "validations"      :> Get '[JSON] (ValidationsDto MinimalVDto),
        fullValidationResults    :: api :- "validations-full" :> Get '[JSON] (ValidationsDto FullVDto),

        metrics :: api   :- "metrics" :> Get '[JSON] MetricsDto,                

        lmdbStats :: api :- "lmdb-stats" :> Get '[JSON] TotalDBStats,
        jobs :: api      :- "jobs" :> Get '[JSON] JobsDto,
        system :: api    :- "system" :> Get '[JSON] SystemDto,

        publicationsPoints :: api :- "repositories" :> Get '[JSON] PublicationPointDto,        

        objectView :: api :- "object" :> QueryParam "uri" Text 
                                    :> QueryParam "hash" Text 
                                    :> Get '[JSON] [RObject],

        rtrDiffs :: api :- "rtr" :> Get '[JSON] RtrDto,

        versions :: api :- "versions" :> Get '[JSON] [WorldVersion]
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
    & info.version  .~ convert getVersion
    & info.description  ?~ ("Note: at the moment this API does not generate a proper API schema, " <> 
                            "this UI is only good for documentation and examples." )
    & basePath          ?~ "/api"
    & paths .~ IOMap.fromList 
        [ 
            ("/vrps.csv", mempty & get ?~ csvOn200 
                "CSV-formatted list of VRPs from the latest validation run without SLURM filtering applied"),
            ("/vrps", mempty & get ?~ jsonOn200 
                "List of VRPs from the latest validation run without SLURM filtering applied"),
            
            ("/vrps-filtered.csv", mempty & get ?~ csvOn200 
                "CSV-formatted list of VRPs with SLURM filtering applied to it"),
            ("/vrps-filtered", mempty & get ?~ jsonOn200 
                "List of VRPs with SLURM filtering applied to it"),

            ("/vrps-unique.csv", mempty & get ?~ csvOn200 
                "CSV-formatted list of unique VRPs with SLURM filtering applied to it"),
            ("/vrps-unique", mempty & get ?~ jsonOn200 
                "List of of unique VRPs with SLURM filtering applied to it"),

            ("/aspa", mempty & get ?~ jsonOn200 "List of all valid ASPA objects found in repositories"),
            ("/bgp-certificates", mempty & get ?~ jsonOn200 "List of all valid BGPSec certificates found in repositories"),

            ("/validations", mempty & get ?~ jsonOn200 
                "Validation results for the latest validation run"),

            ("/validations-full", mempty & get ?~ jsonOn200 
                [i|Returns validation results for the last validation run, but every 
                object represented with full URL path in the RPKI tree, starting from TA.|]),

            ("/metrics", mempty & get ?~ jsonOn200 
                "Metrics for the latest validation run (object counts, repository statuses, etc.)"),

            ("/repositories", mempty & get ?~ jsonOn200 "Statuses of the repositories"),
        
            ("/slurm", mempty & get ?~ (jsonOn200 
                        "Returns SLURM (RFC 8416) that is set using --local-exceptions option"
                    & at 404 ?~ "SLURM is not set using --local-exceptions")),

            ("/lmdb-stats", mempty & get ?~ jsonOn200 "LMDB cache statistics per key-value map"),
            ("/jobs", mempty & get ?~ jsonOn200 "List of latest job runs"),
            ("/system", mempty & get ?~ jsonOn200 "State of RPKI prover instance itself, some metrics and config"),
            ("/rtr", mempty & get ?~ jsonOn200 "State of the RTR server"),
            ("/versions", mempty & get ?~ jsonOn200 "Return list of all world versions")            
        ] 
  where                
    jsonOn200 txt = mempty
                    & produces ?~ MimeList ["application/json"]
                    & at 200 ?~ txt
    csvOn200 txt = mempty
                    & produces ?~ MimeList ["text/csv"]
                    & at 200 ?~ txt