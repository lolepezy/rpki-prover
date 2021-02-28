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
import           Control.Lens                     ((^.), (%~), (&))

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.ByteString.Short       as BSS

import           Data.Text                   (Text)
import qualified Data.Text                   as Text

import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Set             (Set)
import qualified Data.Set             as Set

import Data.Maybe (maybeToList)

import           Data.ByteArray              (convert)
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)

import           GHC.Generics                (Generic)

import qualified Data.ByteString.Base16      as Hex
import qualified Data.ByteString.Base16.Lazy as HexLazy

import           Data.List.NonEmpty          (NonEmpty (..))
import qualified Data.List.NonEmpty          as NonEmpty

import           Servant.API
import           Servant.CSV.Cassava
import           Servant.HTML.Blaze

import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept(..))

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal as I
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze

import           RPKI.Domain                 as Domain
import           RPKI.Config
import           RPKI.CommonTypes
import           RPKI.Metrics

import           RPKI.Reporting
import           RPKI.Resources.IntervalSet
import           RPKI.Resources.Types
import           RPKI.Store.Base.Storable

import           RPKI.Store.Database
import           RPKI.Time
import           RPKI.Util                   (ifJust)

import RPKI.Http.Types
import RPKI.Http.Messages


instance ToMarkup [ValidationResult] where
  toMarkup = validaionResultsHtml

type UI = "ui" :> "validation-results.html" :> Get '[HTML] Html


withUI :: Html -> Html 
withUI include = do 
    H.docTypeHtml $ do
        H.head $ do
            link ! rel "stylesheet" ! href "/styles.css"
            script ! src "/jquery.min.js" $ mempty
            script ! src "/functions.js" ! type_ "text/javascript" $ mempty
        H.body include


validaionResultsHtml :: [ValidationResult] -> Html
validaionResultsHtml result = 
    H.table $ do 
        thead $ tr $ do 
            th $ H.span $ toHtml ("Problem" :: Text)
            th $ H.span $ toHtml ("URL" :: Text)        
        forM_ (Map.toList $ groupByTa result) $ \(ta, vrs) -> do 
            tbody ! A.class_ "labels" $ do 
                tr $ td ! colspan "2" $ do
                    let taText = textValue ta                    
                    input ! A.type_ "checkbox" ! name taText ! A.id taText ! I.dataAttribute "toggle" "toggle"
                    H.label ! A.class_ "drop" ! A.for taText $ toHtml ta >> space
            tbody ! class_ "hide" $ do
                forM_ (zip vrs [1..]) vrHtml

  where
    vrHtml (ValidationResult{..}, index) = do 
        let objectUrl = Prelude.head context         
        forM_ problems $ \p -> do                    
            htmlRow index $ do 
                let (marker, problem) = 
                        case p of 
                            VErr p             -> ("red-dot",    p)                                        
                            VWarn (VWarning p) -> ("yellow-dot", p)
                td $ H.span $ do 
                    H.span ! A.class_ marker $ ""
                    space >> space
                    toHtml $ toMessage problem
                td $ do
                    H.div ! class_ "flex short-link" $ do
                        H.div ! class_ "pointer-down" $ arrowRight >> space
                        H.div ! class_ "full-path" $ objectLink objectUrl
                    H.div ! class_ "flex full-link" ! A.style "display: none;" $ do
                        H.div ! class_ "pointer-up" $ arrowUp >> space
                        H.div ! class_ "full-path" $ do
                            forM_ context $ \pathUrl -> do            
                                H.div ! A.class_ "path-elem" $ objectLink pathUrl
                

    -- TODO This is quite ugly, find a better way to get a proper URL (using servant maybe)
    


validaionMetricsHtml :: MetricMap ValidationMetric -> Html
validaionMetricsHtml validationMetricMap =
    H.table $ do 
        thead $ tr $ do 
            th $ toHtml ("TA" :: Text)
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
            forM_ (zip taMetrics [1..]) $ \((path, vm), index) -> do 
                let ta = NonEmpty.head $ unPath path
                metricRow index ta vm      
            ifJust (allTaMetricPath `Map.lookup` rawMap) 
                $ metricRow (Map.size rawMap) allTAsMetricsName

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


groupByTa :: [ValidationResult] -> Map Text [ValidationResult]
groupByTa vrs = 
    Map.fromListWith (<>) 
    $ [ (ta, [vr]) 
            | vr@(ValidationResult {..}) <- vrs, 
              ta <- lastOne context ]    
  where
    lastOne [] = []
    lastOne xs = [last xs]


objectLink :: Text -> Html
objectLink url = let 
    link = "/api/object?uri=" <> url 
    in H.a ! A.href (textValue link) $ toHtml url

htmlRow index = 
    case index `mod` 2 of 
        0 -> tr ! A.class_ "even-row"                    
        _ -> tr 


space, arrowUp, arrowRight :: Html
space      = preEscapedToMarkup ("&nbsp;" :: Text)
arrowUp    = preEscapedToMarkup ("&#9650;" :: Text)
arrowRight = preEscapedToMarkup ("&#10095;" :: Text)

lineBreak = H.br

instance ToMarkup TimeMs where 
    toMarkup (TimeMs s) = toMarkup $ show s <> "ms"