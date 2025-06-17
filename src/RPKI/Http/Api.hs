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
import           RPKI.Domain
import           RPKI.Store.Types
import           RPKI.Http.Types
import           RPKI.SLURM.Types
import           RPKI.Util (convert)
import           RPKI.Version


data API api = API {        
        vrpsCsv  :: api :- "vrps.csv"  :> QueryParam "version" Text 
                                       :> Get '[ManualCVS] RawCSV,

        vrpsCsvExt :: api :- "vrps.csvext"  :> QueryParam "version" Text 
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

        splJson :: api :- "spls" :> QueryParam "version" Text 
                                 :> Get '[JSON] [SplDto],

        aspas    :: api :- "aspa" :> QueryParam "version" Text 
                                  :> Get '[JSON] [AspaDto],

        gbrs     :: api :- "gbrs" :> QueryParam "version" Text 
                                  :> Get '[JSON] [Located GbrDto],

        bgpCerts :: api :- "bgpsec" :> QueryParam "version" Text 
                                    :> Get '[JSON] [BgpCertDto],

        bgpCertsFiltered :: api :- "bgpsec-filtered" :> QueryParam "version" Text 
                                                     :> Get '[JSON] [BgpCertDto],

        slurm :: api :- "slurm" :> Get '[JSON] Slurm,
        slurms :: api :- "slurms" :> Get '[JSON] [(WorldVersion, Slurm)],

        tals :: api :- "tals" :> Get '[JSON] [TalDto],
                
        minimalValidationResults  :: api :- "validations"      
                                    :> QueryParam "version" Text 
                                    :> Get '[JSON] (ValidationsDto (MinimalVDto ResolvedFocusDto)),

        fullValidationResults     :: api :- "validations-full" 
                                    :> QueryParam "version" Text 
                                    :> Get '[JSON] (ValidationsDto ResolvedVDto),

        originalValidationResults :: api :- "validations-original" 
                                    :> QueryParam "version" Text 
                                    :> Get '[JSON] (ValidationsDto OriginalVDto),

        metrics :: api   :- "metrics" 
                            :> QueryParam "version" Text 
                            :> Get '[JSON] MetricsDto,                

        lmdbStats :: api :- "lmdb-stats" :> Get '[JSON] TotalDBStats,
        jobs :: api      :- "jobs" :> Get '[JSON] JobsDto,
        system :: api    :- "system" :> Get '[JSON] SystemDto,

        repositories :: api :- "repositories" :> Get '[JSON] PublicationPointsDto,        

        objectView :: api :- "object" :> QueryParam "uri" Text 
                                      :> QueryParam "hash" Text 
                                      :> QueryParam "key" Text 
                                      :> Get '[JSON] [RObject],

        originals :: api :- "original" :> QueryParam "hash" Text 
                                       :> Get '[ObjectBlob] ObjectOriginal,

        manifests :: api :- "manifests" :> QueryParam "aki" Text 
                                        :> Get '[JSON] ManifestsDto,

        rtr :: api :- "rtr" :> Get '[JSON] RtrDto,

        versions :: api :- "versions" :> Get '[JSON] [WorldVersion],

        validity :: api :- "validity" :> Capture "asn" String 
                                      :> CaptureAll "prefix" String 
                                      :> Get '[JSON] ValidityResultDto,

        validityAsnPrefix :: api :- "validity" :> QueryParam "asn" String 
                                       :> QueryParam "prefix" String 
                                       :> Get '[JSON] ValidityResultDto,

        validityBulk :: api :- "validity" :> ReqBody '[JSON] [ValidityBulkInputDto] 
                                          :> Post '[JSON] ValidityBulkResultDto        
    }
    deriving (Generic)

data HttpApi route = HttpApi {
        api           :: route :- "api" :> ToServant API AsApi,
        metrics       :: route :- "metrics" :> Get '[PlainText] Text,
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
    & info.version  .~ convert rpkiProverVersion
    & info.description  ?~ ("Note: at the moment this API does not generate a proper API schema, " <> 
                            "this UI is only good for documentation and examples." )
    & basePath          ?~ "/api"
    & paths .~ IOMap.fromList 
        [ 
            ("/vrps.csv", mempty & get ?~ csvOn200 
                "CSV-formatted list of VRPs from the latest validation run(s) without SLURM filtering applied"),
            ("/vrps.csvext", mempty & get ?~ csvOn200 
                "CSV-formatted list of VRPs from the latest validation run(s) without SLURM filtering applied"),
            ("/vrps", mempty & get ?~ jsonOn200 
                "List of VRPs from the latest validation run(s) without SLURM filtering applied"),

            ("/vrps-filtered.csv", mempty & get ?~ csvOn200 
                "CSV-formatted list of VRPs with SLURM filtering applied to it"),
            ("/vrps-filtered", mempty & get ?~ jsonOn200 
                "List of VRPs with SLURM filtering applied to it"),

            ("/vrps-unique.csv", mempty & get ?~ csvOn200 
                "CSV-formatted list of unique VRPs with SLURM filtering applied to it"),
            ("/vrps-unique", mempty & get ?~ jsonOn200 
                "List of of unique VRPs with SLURM filtering applied to it"),

            ("/gbrs", mempty & get ?~ jsonOn200 "List of all valid GBR objects found in repositories"),
            ("/spls", mempty & get ?~ jsonOn200 "List of all valid Prefix List objects found in repositories"),
            ("/aspa", mempty & get ?~ jsonOn200 "List of all valid ASPA objects found in repositories"),
            ("/bgpsec", mempty & get ?~ jsonOn200 "List of all valid BGPSec certificates found in repositories"),
            ("/bgpsec-filtered", mempty & get ?~ jsonOn200 
                "List of all valid BGPSec certificates found in repositories filtered with SLURM"),

            ("/validations", mempty & get ?~ jsonOn200 
                "Validation results for the latest validation run"),

            ("/validations-full", mempty & get ?~ jsonOn200 
                [i|Returns validation results for the last validation run, but every 
                object represented with full URL path in the RPKI tree, starting from TA.|]),

            ("/validations-original", mempty & get ?~ jsonOn200 
                [i|Returns validation results for the last validation run, but every 
                object represented with full URL path in the RPKI tree, starting from TA.|]),

            ("/metrics", mempty & get ?~ jsonOn200 
                "Metrics for the latest validation run (object counts, repository statuses, etc.)"),

            ("/repositories", mempty & get ?~ jsonOn200 "Statuses of the repositories"),

            ("/object", mempty 
                & get ?~ jsonOn200 
                    [i|JSON-formatted array of objects with the given hash, url or key. 
                       One (and only one) of the 'hash', 'url' or 'key' parameters must be provided.|]
                & parameters .~ [ 
                    Inline $ mempty
                        & name .~ "hash"
                        & description ?~ "Object hash in hex"
                        & required ?~ False
                        & schema .~ ParamOther (mempty & in_ .~ ParamQuery), 
                    Inline $ mempty
                        & name .~ "url"
                        & description ?~ "Object url"
                        & required ?~ False
                        & schema .~ ParamOther (mempty & in_ .~ ParamQuery),
                    Inline $ mempty
                        & name .~ "key"
                        & description ?~ "Object internal unique key (mainly for debugging purposes)"
                        & required ?~ False
                        & schema .~ ParamOther (mempty & in_ .~ ParamQuery)
                ]            
            ),        

            ("/original", mempty 
                & get ?~ jsonOn200 
                            [i|Original ASN1 blob of the object (mainly for debugging purposes)
                               Note: only the object that failed to parse are stored in their original form.|]
                & parameters .~ [ 
                    Inline $ mempty
                        & name .~ "hash"
                        & description ?~ "Object hash in hex"
                        & required ?~ False
                        & schema .~ ParamOther (mempty & in_ .~ ParamQuery) ]            
            ),  

            ("/manifests", mempty 
                & get ?~ jsonOn200 "Returns all cached manifests and manifest shortcut for the given AKI"
                & parameters .~ [ 
                    Inline $ mempty
                        & name .~ "aki"
                        & description ?~ "AKI of the manifest in hex, i.e. SKI of the parent CA certificate"
                        & required ?~ False
                        & schema .~ ParamOther (mempty & in_ .~ ParamQuery) ]            
            ),                            
        
            ("/slurm", mempty & get ?~ (jsonOn200 
                        "Returns SLURM (RFC 8416) that is set using --local-exceptions option"
                    & at 404 ?~ "SLURM is not set using --local-exceptions")),

            ("/slurms", mempty & get ?~ jsonOn200 
                        "Returns all SLURMs (RFC 8416) for every version"),
            ("/tals", mempty & get ?~ jsonOn200 "Returns all TALs"),

            ("/lmdb-stats", mempty & get ?~ jsonOn200 "LMDB cache statistics per key-value map"),
            ("/jobs", mempty & get ?~ jsonOn200 "List of latest job runs"),
            ("/system", mempty & get ?~ jsonOn200 "State of RPKI prover instance itself, some metrics and config"),
            ("/rtr", mempty & get ?~ jsonOn200 "State of the RTR server"),
            ("/versions", mempty & get ?~ jsonOn200 "Return list of all world versions"),

            ("/validity/{asn}/{prefix}", mempty & get ?~ jsonOn200 validityDescription),

            ("/validity", mempty 
                & get ?~ jsonOn200 validityDescription                    
                & parameters .~ [ 
                    Inline $ mempty
                        & name .~ "asn"
                        & description ?~ "ASN"
                        & required ?~ True
                        & schema .~ ParamOther (mempty & in_ .~ ParamQuery), 
                    Inline $ mempty
                        & name .~ "prefix"
                        & description ?~ "Prefix"
                        & required ?~ True
                        & schema .~ ParamOther (mempty & in_ .~ ParamQuery)                    
                ]            
            ),               

            ("/validity", mempty 
                & post ?~ 
                    (jsonOn200                
                            [i|Accepts a list of asn/prefix pair encoded in JSON, e.g. 
                               [{ "asn": "AS123", "prefix": "X.X.X.X/Y" }, ...] and returns the list of 
                               validity for each pair. |]                
                    & consumes ?~ MimeList ["application/json"])
                & parameters .~ [ 
                    Inline $ mempty
                        & name .~ "input"                        
                        & required ?~ True
                        & schema .~ ParamBody (Inline mempty)
                ]
            )
        ] 
  where            
    validityDescription = [i|Returns the same as Routinator's /validity 
                             end-point (https://routinator.docs.nlnetlabs.nl/en/stable/api-endpoints.html)|]    
    jsonOn200 txt = mempty
                    & produces ?~ MimeList ["application/json"]
                    & at 200 ?~ txt
    csvOn200 txt = mempty
                    & produces ?~ MimeList ["text/csv"]
                    & at 200 ?~ txt