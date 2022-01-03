{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeOperators         #-}

module RPKI.Http.UI where

import           Control.Monad
import           Control.Lens                ((^.))

import           Data.Text                   (Text)
import qualified Data.Text                   as Text

import qualified Data.String.Interpolate.IsString as T

import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map

import qualified Data.List.NonEmpty          as NonEmpty
import qualified Data.List                   as List
import           Data.Map.Monoidal.Strict (getMonoidalMap)
import qualified Data.Map.Monoidal.Strict as MonoidalMap

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import           RPKI.AppTypes
import           RPKI.AppState
import           RPKI.Metrics
import           RPKI.Reporting
import           RPKI.Time

import RPKI.Http.Types
import RPKI.Http.Messages
import RPKI.Domain


mainPage :: Maybe WorldVersion -> [ValidationResult] -> RawMetric -> Html
mainPage worldVersion vResults metrics = 
    H.docTypeHtml $ do
        H.head $ do
            link ! rel "stylesheet" ! href "/static/styles.css"
            script ! src "/static/jquery.min.js" $ mempty
            script ! src "/static/functions.js" ! type_ "text/javascript" $ mempty            
        H.body $ do 
            H.div ! A.class_ "side-navigation" $ do  
                H.a ! A.href "#overall"            $ H.text "Overall"
                H.a ! A.href "#validation-metrics" $ H.text "Validation metrics"
                H.a ! A.href "#rrdp-metrics"       $ H.text "RRDP metrics"
                H.a ! A.href "#rsync-metrics"      $ H.text "Rsync metrics"
                H.a ! A.href "#validation-details" $ H.text "Validation details"
    
            H.div ! A.class_ "main" $ do            
                H.a ! A.id "overall" $ "" 
                H.br
                H.section $ H.text "Overall"
                H.br
                overallHtml worldVersion
                H.br >> H.br            
                H.a ! A.id "validation-metrics" $ "" 
                H.section $ H.text "Validation metrics"
                validationMetricsHtml metrics
                H.a ! A.id "rrdp-metrics" $ ""
                H.section $ H.text "RRDP metrics"
                rrdpMetricsHtml $ rrdpMetrics metrics 
                H.a ! A.id "rsync-metrics" $ ""
                H.section $ H.text "Rsync metrics"
                rsyncMetricsHtml $ rsyncMetrics metrics        
                H.a ! A.id "validation-details" $ ""
                H.section $ H.text "Validation details"
                validaionDetailsHtml vResults


overallHtml :: Maybe WorldVersion -> Html
overallHtml Nothing = pure ()
overallHtml (Just worldVersion) = do 
    let t = versionToMoment worldVersion
    H.div ! A.class_ "overall" $ do 
        H.text "Last validation: " >> space
        H.text $ Text.pack $ uiDateFormat t
        space >> H.text "(UTC)"


validationMetricsHtml :: RawMetric -> Html
validationMetricsHtml (groupedValidationMetric -> grouped) = do 
    
    let repoMetrics = MonoidalMap.toList $ grouped ^. #perRepository
    let taMetrics   = MonoidalMap.toList $ grouped ^. #perTa        

    -- this is for per-TA metrics
    H.table $ do         
        H.thead $ tr $ do 
            th $ do 
                H.text "Trust Anchor ("
                toHtml $ length taMetrics
                H.text " in total)"
            th $ H.text "Validation time"
            th $ H.text "Original VRPs"      
            th $ H.text "Unique VRPs"      
            th $ H.text "Objects"
            th $ H.text "ROAs"
            th $ H.text "Certificates"
            th $ H.text "Manifests"
            th $ H.text "CRLs"
            th $ H.text "GBRs"
        
        H.tbody $ do 
            forM_ (zip taMetrics [1 :: Int ..]) $ \((TaName ta, vm), index) ->                
                metricRow index ta 
                    (\vm' -> td $ toHtml $ vm' ^. #totalTimeMs) 
                    (\vm' -> td $ toHtml $ show $ vm' ^. #uniqueVrpNumber) 
                    vm      
            
            metricRow (MonoidalMap.size (grouped ^. #perTa) + 1) 
                        ("Total" :: Text) 
                        (const $ td $ toHtml $ text "-")
                        (const $ td $ toHtml $ show $ grouped ^. #total . #uniqueVrpNumber)
                        (grouped ^. #total)

    -- this is for per-repository metrics        
    H.table $ do         
        H.thead $ tr $ do 
            th $ H.div ! A.class_ "tooltip" $ do 
                H.text "Primary repository ("
                toHtml $ length repoMetrics
                H.text " in total)" 
                H.span ! A.class_ "tooltiptext" $ primaryRepoTooltip
            th $ H.text "Original VRPs"      
            th $ H.text "Objects"
            th $ H.text "ROAs"
            th $ H.text "Certificates"
            th $ H.text "Manifests"
            th $ H.text "CRLs"
            th $ H.text "GBRs"                

        H.tbody $ do 
            let sortedRepos = List.sortOn fst $ 
                    Prelude.map (\(u', z) -> (unURI $ getURL u', z)) repoMetrics
            forM_ (zip sortedRepos [1 :: Int ..]) $ \((url, vm), index) ->                
                metricRow index url 
                    (const $ pure ()) 
                    (const $ pure ()) vm                  
  where
    metricRow index ta validationTime uniqueVrps vm = do 
        let totalCount = vm ^. #validCertNumber + 
                         vm ^. #validRoaNumber +
                         vm ^. #validMftNumber +
                         vm ^. #validCrlNumber +
                         vm ^. #validGbrNumber
        htmlRow index $ do 
            td $ toHtml ta                                    
            void $ validationTime vm
            td $ toHtml $ show $ vm ^. #vrpCounter
            void $ uniqueVrps vm 
            td $ toHtml $ show totalCount
            td $ toHtml $ show $ vm ^. #validRoaNumber
            td $ toHtml $ show $ vm ^. #validCertNumber
            td $ toHtml $ show $ vm ^. #validMftNumber
            td $ toHtml $ show $ vm ^. #validCrlNumber
            td $ toHtml $ show $ vm ^. #validGbrNumber

rrdpMetricsHtml :: MetricMap RrdpMetric -> Html
rrdpMetricsHtml rrdpMetricMap =
    H.table $ do 
        let rrdpMap = unMetricMap rrdpMetricMap                

        H.thead $ tr $ do                         
            th $ do 
                H.text "Repository (" 
                toHtml $ MonoidalMap.size rrdpMap
                H.text " in total)"                                 
            th $ H.div ! A.class_ "tooltip" $ do
                H.text "Fetching"
                H.span ! A.class_ "tooltiptext" $ rrdpFetchTooltip            
            th $ H.div ! A.class_ "tooltip" $ do 
                H.text "RRDP Update"            
                H.span ! A.class_ "tooltiptext" $ rrdpUpdateTooltip
            th $ H.text "Added objects"
            th $ H.text "Deleted objects"
            th $ H.text "Last HTTP status"
            th $ H.text "Download time"
            th $ H.text "Save time"
            th $ H.text "Total time"                    

        H.tbody $ do 
            forM_ (zip (MonoidalMap.toList rrdpMap) [1 :: Int ..]) $ \((path, rm), index) -> do 
                let repository = NonEmpty.head $ unPath path
                htmlRow index $ do 
                    td $ toHtml repository                        
                    td ! A.class_ "no-wrap" $ toHtml $ rm ^. #fetchFreshness
                    td ! A.class_ "no-wrap" $ toHtml $ rm ^. #rrdpSource                    
                    td $ toHtml $ show $ rm ^. #added
                    td $ toHtml $ show $ rm ^. #deleted
                    td $ toHtml $ rm ^. #lastHttpStatus
                    td $ toHtml $ rm ^. #downloadTimeMs                                
                    td $ toHtml $ rm ^. #saveTimeMs
                    td $ toHtml $ rm ^. #totalTimeMs     


rsyncMetricsHtml :: MetricMap RsyncMetric -> Html
rsyncMetricsHtml rsyncMetricMap =
    H.table $ do 
        let rsyncMap = getMonoidalMap $ unMetricMap rsyncMetricMap        

        H.thead $ tr $ do 
            th $ do 
                H.text "Repository ("
                toHtml $ Map.size rsyncMap
                H.text " in total)" 
            th $ H.div ! A.class_ "tooltip" $ do
                H.text "Fetching"
                H.span ! A.class_ "tooltiptext" $ rsyncFetchTooltip            
            th $ H.text "Processed objects"
            th $ H.text "Total time"                    

        H.tbody $
            forM_ (zip (Map.toList rsyncMap) [1 :: Int ..]) $ \((path, rm), index) -> do 
                let repository = NonEmpty.head $ unPath path
                htmlRow index $ do 
                    td $ toHtml repository                                                        
                    td ! A.class_ "no-wrap" $ toHtml $ rm ^. #fetchFreshness            
                    td $ toHtml $ show $ rm ^. #processed            
                    td $ toHtml $ rm ^. #totalTimeMs            


validaionDetailsHtml :: [ValidationResult] -> Html
validaionDetailsHtml result = 
    H.table $ do 
        H.thead $ tr $ do 
            th $ H.span $ H.text "Problem"            
            th $ H.div ! A.class_ "tooltip" $ do
                H.text "URL/Path"
                H.span ! A.class_ "tooltiptext" $ validationPathTootip               
        forM_ (Map.toList $ groupByTa result) $ \(ta, vrs) -> do 
            H.tbody ! A.class_ "labels" $ do 
                tr $ td ! colspan "2" $                                         
                    H.div ! class_ "flex switch" $ do                        
                        H.div ! class_ "ta-header" $ do 
                            toHtml ta >> ":"
                            space >> space >> space
                            let (e, w) = countProblems vrs
                            toHtml e >> " errors, "
                            toHtml w >> " warnings"
                        H.div ! class_ "pointer-up-header" $ arrowUp >> space
                        H.div ! class_ "pointer-right-header" ! A.style "display: none;" $ arrowRight >> space                        
            tbody ! class_ "hide" $ do 
                forM_ (zip vrs [1 :: Int ..]) vrHtml

  where      
    vrHtml (ValidationResult{..}, index) = do 
        let objectUrl = Prelude.head context         
        forM_ problems $ \pr -> do                    
            htmlRow index $ do 
                let (marker, problem) = 
                        case pr of 
                            VErr err           -> ("red-dot",  err)                                        
                            VWarn (VWarning w) -> ("yellow-dot", w)
                td $ H.span $ do 
                    H.span ! A.class_ marker $ ""
                    space >> space
                    mapM_ (\z -> H.text z >> H.br) $ Text.lines $ toMessage problem
                td $ do
                    H.div ! class_ "flex short-link" $ do
                        H.div ! class_ "pointer-right" $ arrowRight >> space
                        H.div ! class_ "full-path" $ objectLink objectUrl
                    H.div ! class_ "flex full-link" ! A.style "display: none;" $ do
                        H.div ! class_ "pointer-up" $ arrowUp >> space
                        H.div ! class_ "full-path" $ do
                            forM_ context $ \pathUrl -> do            
                                H.div ! A.class_ "path-elem" $ objectLink pathUrl

    countProblems = 
        List.foldl' countP (0 :: Int, 0 :: Int)
        where
            countP z ValidationResult {..} = List.foldl' countEW z problems
            countEW (e, w) (VErr _)  = (e + 1, w)
            countEW (e, w) (VWarn _) = (e, w + 1)


primaryRepoTooltip :: Html
primaryRepoTooltip = 
    H.text $ "Objects are associated with a repository they are downloaded from. " <> 
            "Fallback from RRDP to rsync does not change this association, " <> 
            "so a valid object is attributed to the RRDP repository even " <> 
            "if it was downloaded from the rsync one becasue of the fall-back."

fetchTooltip :: Text -> Text -> Html
fetchTooltip repoType setting = do                
    H.div ! A.style "text-align: left;" $ do 
        space >> space >> H.text "Used values" >> H.br
        H.ul $ do 
            H.li $ H.text [T.i|'Up-to-date' - no fetch is needed, #{repoType} repository was updated less than '#{setting}' seconds ago.|]
            H.li $ H.text "'Succeeded' and 'Failed' are self-explanatory"

rrdpFetchTooltip :: Html
rrdpFetchTooltip = fetchTooltip "RRDP" "rrdp-timeout"
rsyncFetchTooltip = fetchTooltip "rsync" "rsync-timeout"

rrdpUpdateTooltip :: Html
rrdpUpdateTooltip = do
    H.div ! A.style "text-align: left;" $ do 
        space >> space >> H.text "Used values" >> H.br
        H.ul $ do 
            H.li $ H.text "'Snapshot' - snapshot was used for RRDP update"
            H.li $ H.text "'Deltas' - deltas were used for RRDP update"
            H.li $ H.text "'-' - No update is needed, local and remote serials are equal"

validationPathTootip :: Html
validationPathTootip = do   
    space >> space
    H.text "Signs " >> arrowRight >> H.text " and " >> arrowUp >> H.text " are clickable. "
    H.text "'Path' here shows the full sequence of objects from the TA to the object in question."
    space >> space
        

groupByTa :: [ValidationResult] -> Map Text [ValidationResult]
groupByTa vrs = 
    Map.fromListWith (<>) 
    $ [ (ta, [vr]) 
            | vr@ValidationResult {..} <- vrs, 
              ta <- lastOne context ]    
  where
    lastOne [] = []
    lastOne xs = [last xs]


-- TODO This is quite ugly, find a better way to get a proper URL (using servant maybe)
objectLink :: Text -> Html
objectLink url = 
    H.a ! A.href (textValue ("/api/object?uri=" <> url)) $ toHtml url

htmlRow :: Int -> Html -> Html
htmlRow index = 
    case index `mod` 2 of 
        0 -> tr ! A.class_ "even-row"
        _ -> tr 

space, arrowUp, arrowRight :: Html
space      = preEscapedToMarkup ("&nbsp;" :: Text)
arrowUp    = preEscapedToMarkup ("&#9650;" :: Text)
arrowRight = preEscapedToMarkup ("&#10095;" :: Text)


-- | Very crude formatting of a text to avoid using <pre> and screwing up the style.
--
htmlMessage :: Text -> Html
htmlMessage message = mapM_ (\z -> H.text z >> H.br) $ Text.lines message

instance ToMarkup TimeMs where 
    toMarkup (TimeMs s) = toMarkup $ show s <> "ms"

instance ToMarkup HttpStatus where 
    toMarkup (HttpStatus s) = toMarkup $ show s

instance ToMarkup FetchFreshness where 
    toMarkup UpToDate       = toMarkup ("Up-to-date" :: Text)
    toMarkup AttemptedFetch = toMarkup ("Succeeded" :: Text)
    toMarkup FailedToFetch  = toMarkup ("Failed" :: Text)

instance ToMarkup RrdpSource where 
    toMarkup RrdpNoUpdate = toMarkup ("-" :: Text)
    toMarkup RrdpDelta    = toMarkup ("Deltas" :: Text)
    toMarkup RrdpSnapshot = toMarkup ("Snapshot" :: Text)

instance ToMarkup PathSegment where 
    toMarkup = toMarkup . segmentToText