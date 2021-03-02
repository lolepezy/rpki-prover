{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeOperators         #-}

module RPKI.Http.UI where

import           Control.Monad
import           Control.Lens                ((^.))

import           Data.Text                   (Text)
import qualified Data.Text                   as Text

import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map

import           Data.List.NonEmpty          (NonEmpty (..))
import qualified Data.List.NonEmpty          as NonEmpty

import           Servant.API
import           Servant.HTML.Blaze

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import           RPKI.CommonTypes
import           RPKI.AppState
import           RPKI.Metrics
import           RPKI.Reporting
import           RPKI.Time
import           RPKI.Util                   (ifJust)

import RPKI.Http.Types
import RPKI.Http.Messages


type UI = Get '[HTML] Html

mainPage :: Maybe WorldVersion -> [ValidationResult] -> AppMetric -> Html
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
            validationMetricsHtml $ validationMetrics metrics
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


validationMetricsHtml :: MetricMap ValidationMetric -> Html
validationMetricsHtml validationMetricMap =
    H.table $ do 
        H.thead $ tr $ do 
            th $ toHtml ("Trust Anchor" :: Text)
            th $ toHtml ("Validation time" :: Text)
            th $ toHtml ("VRPs" :: Text)        
            th $ toHtml ("Objects" :: Text)        
            th $ toHtml ("ROAs" :: Text)        
            th $ toHtml ("Certificates" :: Text)        
            th $ toHtml ("Manifests" :: Text)        
            th $ toHtml ("CRLs" :: Text)        
            th $ toHtml ("GBRs" :: Text)        
        let allTaMetricPath = Path (allTAsMetricsName :| [])
        let rawMap = unMonoidMap $ unMetricMap validationMetricMap
        let taMetrics = filter (\(ta, _) -> ta /= allTaMetricPath)
                            $ Map.toList rawMap
        H.tbody $ do 
            forM_ (zip taMetrics [1 :: Int ..]) $ \((path, vm), index) -> do 
                let ta = NonEmpty.head $ unPath path
                metricRow index ta vm      
            ifJust (allTaMetricPath `Map.lookup` rawMap) 
                $ metricRow (Map.size rawMap) ("Total" :: Text)

  where
    metricRow index ta vm = do 
        let totalCount = vm ^. #validCertNumber + 
                         vm ^. #validRoaNumber +
                         vm ^. #validMftNumber +
                         vm ^. #validCrlNumber +
                         vm ^. #validGbrNumber
        htmlRow index $ do 
            td $ toHtml ta                        
            td $ toHtml $ vm ^. #totalTimeMs
            td $ toHtml $ show $ vm ^. #vrpNumber
            td $ toHtml $ show totalCount
            td $ toHtml $ show $ vm ^. #validRoaNumber
            td $ toHtml $ show $ vm ^. #validCertNumber
            td $ toHtml $ show $ vm ^. #validMftNumber
            td $ toHtml $ show $ vm ^. #validCrlNumber
            td $ toHtml $ show $ vm ^. #validGbrNumber


rrdpMetricsHtml :: MetricMap RrdpMetric -> Html
rrdpMetricsHtml rrdpMetricMap =
    H.table $ do 
        H.thead $ tr $ do 
            th $ H.text "Repository"
            th $ H.text "Source"
            th $ H.text "Download time"
            th $ H.text "Added objects"
            th $ H.text "Deleted objects"
            th $ H.text "Last HTTP status"
            th $ H.text "Save time"
            th $ H.text "Total time"
                
        let taMetrics = Map.toList $ unMonoidMap $ unMetricMap rrdpMetricMap

        H.tbody $ do 
            forM_ (zip taMetrics [1 :: Int ..]) $ \((path, rm), index) -> do 
                let repository = NonEmpty.head $ unPath path
                htmlRow index $ do 
                    td $ toHtml repository                        
                    td $ toHtml $ rm ^. #rrdpSource
                    td $ toHtml $ rm ^. #downloadTimeMs            
                    td $ toHtml $ show $ rm ^. #added
                    td $ toHtml $ show $ rm ^. #deleted
                    td $ toHtml $ rm ^. #lastHttpStatus
                    td $ toHtml $ rm ^. #saveTimeMs
                    td $ toHtml $ rm ^. #totalTimeMs     


rsyncMetricsHtml :: MetricMap RsyncMetric -> Html
rsyncMetricsHtml rrdpMetricMap =
    H.table $ do 
        H.thead $ tr $ do 
            th $ H.text "Trust Anchor"
            th $ H.text "Processed objects"
            th $ H.text "Total time"
                
        let taMetrics = Map.toList $ unMonoidMap $ unMetricMap rrdpMetricMap

        H.tbody $
            forM_ (zip taMetrics [1 :: Int ..]) $ \((path, rm), index) -> do 
                let repository = NonEmpty.head $ unPath path
                htmlRow index $ do 
                    td $ toHtml repository                                    
                    td $ toHtml $ show $ rm ^. #processed            
                    td $ toHtml $ rm ^. #totalTimeMs            


validaionDetailsHtml :: [ValidationResult] -> Html
validaionDetailsHtml result = 
    H.table $ do 
        H.thead $ tr $ do 
            th $ H.span $ toHtml ("Problem" :: Text)
            th $ H.span $ toHtml ("URL/Path" :: Text)        
        forM_ (Map.toList $ groupByTa result) $ \(ta, vrs) -> do 
            H.tbody ! A.class_ "labels" $ do 
                tr $ td ! colspan "2" $                                         
                    H.div ! class_ "flex switch" $ do                        
                        H.div ! class_ "ta-header" $ toHtml ta >> space
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

instance ToMarkup RrdpSource where 
    toMarkup RrdpNoUpdate = toMarkup ("-" :: Text)
    toMarkup RrdpDelta    = toMarkup ("Deltas" :: Text)
    toMarkup RrdpSnapshot = toMarkup ("Snapshot" :: Text)