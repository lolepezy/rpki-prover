{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import           Control.Lens 

import           Data.Ord
import           Data.Text                   (Text)
import qualified Data.Text                   as Text

import qualified Data.String.Interpolate.IsString as T

import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map

import qualified Data.List.NonEmpty          as NonEmpty
import qualified Data.List                   as List
import           Data.Map.Monoidal.Strict    (getMonoidalMap)
import qualified Data.Map.Monoidal.Strict    as MonoidalMap
import           Data.Maybe                  (fromMaybe)
import           Data.String                 (IsString)

import           Data.String.Interpolate.IsString

import           Text.Blaze.Html5            as H hiding (i)
import           Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal         as BlazeI

import           RPKI.AppTypes
import           RPKI.AppState
import           RPKI.Domain
import           RPKI.Http.Types
import           RPKI.Metrics.Metrics
import           RPKI.Metrics.System
import           RPKI.Repository
import           RPKI.Reporting
import           RPKI.Time
import           RPKI.Version

mainPage :: WorldVersion
        -> SystemInfo 
        -> ValidationsDto ResolvedVDto
        -> [RepositoryUIDDto]
        -> RawMetric
        -> Html
mainPage version systemInfo validation fetchDtos rawMetric =     
    H.docTypeHtml $ do
        H.head $ do
            link ! rel "stylesheet" ! href "/static/styles.css"
            H.script ! src "/static/script.js" $ ""
        H.body $
            H.div ! A.class_ "side-navigation" $ do  
                H.a ! A.href "#overall"            $ H.text "Overall"
                H.a ! A.href "#validation-metrics" $ H.text "Validation metrics"
                H.a ! A.href "#validation-details" $ H.text "Validation details"
                H.a ! A.href "#rrdp-metrics"       $ H.text "RRDP metrics"
                H.a ! A.href "#rsync-metrics"      $ H.text "Rsync metrics"                
                H.a ! A.href "#fetch-details"      $ H.text "Fetch details"

        H.div ! A.class_ "main" $ do
            H.a ! A.id "overall" $ ""                 
            H.section $ H.h3 "Overall"                
            overallHtml systemInfo version                    

            H.a ! A.id "validation-metrics" $ "" 
            H.section $ H.h3 "Validation metrics"                        
            validationMetricsHtml $ toMetricsDto rawMetric ^. #groupedValidations
            H.a ! A.id "validation-details" $ ""
            H.section $ H.h3 "Validation details"
            validaionDetailsHtml $ validation ^. #validations            

            H.a ! A.id "rrdp-metrics" $ ""
            H.section $ H.h3 "RRDP metrics"
            rrdpMetricsHtml [ d | RrdpUIDto d <- fetchDtos ]

            H.a ! A.id "rsync-metrics" $ ""
            H.section $ H.h3 "Rsync metrics"
            rsyncMetricsHtml [ d | RsyncUIDto d <- fetchDtos ]

            -- H.a ! A.id "fetch-details" $ ""
            -- H.section $ H.h3 "Fetch details"
            -- validaionDetailsHtml fetchValidation



overallHtml :: SystemInfo -> WorldVersion -> Html
overallHtml SystemInfo {..} worldVersion = do 
    let t = versionToInstant worldVersion
    H.table ! A.class_ "gen-t" $ H.tbody $ do
        htmlRow 0 $ do 
            genTd $ H.text "Version"
            genTd $ H.text rpkiProverVersion
        htmlRow 1 $ do 
            genTd $ H.text "Last validation"
            genTd $ H.text $ Text.pack $ instantDateFormat t
        htmlRow 2 $ do 
            genTd $ H.text "Startup time"
            genTd $ H.text $ Text.pack $ instantDateFormat startUpTime
                    


validationMetricsHtml :: GroupedValidationMetric ValidationMetric -> Html
validationMetricsHtml grouped = do 
    
    let repoMetrics = MonoidalMap.toList $ grouped ^. #byRepository
    let taMetrics   = MonoidalMap.toList $ grouped ^. #byTa        
    
    -- this is for per-TA metrics
    H.table ! A.class_ "gen-t" $ do         
        H.thead $ tr $ do 
            genTh $ do 
                H.text "Trust Anchor ("
                toHtml $ length taMetrics
                H.text " in total)"
            genTh $ H.text "Validated at"
            genTh $ H.text "Validation time"
            genTh $ H.text "Original VRPs"      
            genTh $ H.text "Unique VRPs"      
            genTh $ H.text "Objects"
            genTh $ H.text "ROAs"
            genTh $ H.text "Certificates"
            genTh $ H.text "Manifests"
            genTh $ H.text "CRLs"
            genTh $ H.text "GBRs"
            genTh $ H.text "ASPAs"
            genTh $ H.text "BGP Certificates"
            genTh $ H.text "Prefix Lists"
        
        H.tbody $ do 
            forM_ (zip taMetrics [1 :: Int ..]) $ \((TaName ta, vm), index) ->                
                metricRow index ta 
                    (\vm' -> genTd $ toHtml $ vm' ^. #totalTimeMs) 
                    (\vm' -> genTd $ toHtml $ show $ vm' ^. #uniqueVrpNumber) 
                    vm      
            
            metricRow (MonoidalMap.size (grouped ^. #byTa) + 1) 
                        ("Total" :: Text) 
                        (const $ genTd $ toHtml $ text "-")
                        (const $ genTd $ toHtml $ show $ grouped ^. #total . #uniqueVrpNumber)
                        (grouped ^. #total)

    -- this is for per-repository metrics        
    H.table ! A.class_ "gen-t" $ do         
        H.thead $ tr $ do 
            genTh $ H.div ! A.class_ "tooltip" $ do 
                H.text "Primary repository ("
                toHtml $ length repoMetrics
                H.text " in total)" 
                H.span ! A.class_ "tooltiptext" $ primaryRepoTooltip
            genTh $ H.text "Validated at"      
            genTh $ H.text "Original VRPs"      
            genTh $ H.text "Objects"
            genTh $ H.text "ROAs"
            genTh $ H.text "Certificates"
            genTh $ H.text "Manifests"
            genTh $ H.text "CRLs"
            genTh $ H.text "GBRs"                
            genTh $ H.text "ASPAs"                
            genTh $ H.text "BGP Certificates"
            genTh $ H.text "Prefix Lists"

        H.tbody $ do 
            let sortedRepos = List.sortOn fst $ 
                    Prelude.map (\(u', z) -> (unURI $ getURL u', z)) repoMetrics
            forM_ (zip sortedRepos [1 :: Int ..]) $ \((url, vm), index) ->                
                metricRow index url 
                    (const $ pure ()) 
                    (const $ pure ()) vm                  
  where
    metricRow index ta validationTime uniqueVrps vm = do 
        let totalCount_ = vm ^. #validCertNumber + 
                         vm ^. #validRoaNumber +
                         vm ^. #validMftNumber +
                         vm ^. #validCrlNumber +
                         vm ^. #validGbrNumber +
                         vm ^. #validAspaNumber +
                         vm ^. #validBgpNumber + 
                         vm ^. #validSplNumber 
        htmlRow index $ do 
            genTd $ toHtml ta       
            genTd $ toHtml $ vm ^. #validatedBy                                         
            void $ validationTime vm
            genTd $ toHtml $ show $ vm ^. #vrpCounter
            void $ uniqueVrps vm 
            genTd $ toHtml $ show totalCount_
            genTd $ toHtml $ show $ vm ^. #validRoaNumber
            genTd $ toHtml $ show $ vm ^. #validCertNumber
            genTd $ toHtml $ show $ vm ^. #validMftNumber
            genTd $ toHtml $ show $ vm ^. #validCrlNumber
            genTd $ toHtml $ show $ vm ^. #validGbrNumber
            genTd $ toHtml $ show $ vm ^. #validAspaNumber
            genTd $ toHtml $ show $ vm ^. #validBgpNumber
            genTd $ toHtml $ show $ vm ^. #validSplNumber

rrdpMetricsHtml :: [RrdpRepositoryUIDto] -> Html
rrdpMetricsHtml rrdpMetrics =
    H.table ! A.class_ "gen-t" $ do 
        H.thead $ tr $ do                         
            genTh $ do 
                H.text "Repository (" 
                toHtml $ length rrdpMetrics
                H.text " in total)"                                
            genTh $ H.text "Updated at"         
            genTh $ H.div ! A.class_ "tooltip" $ do 
                H.text "RRDP Update"            
                H.span ! A.class_ "tooltiptext" $ rrdpUpdateTooltip            
            genTh $ H.text "Added objects"
            genTh $ H.text "Deleted objects"
            genTh $ H.text "Download time"
            genTh $ H.text "Total time"                    

        H.tbody $ do 
            let slowestFirst = List.sortOn (\m -> ordering $ m ^. #repository . #meta . #status) rrdpMetrics
            forM_ (zip slowestFirst [1 :: Int ..]) $ \(m, index) -> do 
                htmlClickableRow index $ do 
                    let URI u_ = getURL $ m ^. #uri
                        (statusHtml, dot, rrdpSource) = 
                            case m ^. #repository . #meta . #status of 
                                Pending     -> 
                                    ("Pending" :: Text, Nothing, "-")
                                FetchedAt t -> 
                                    ([i|Fetched at #{instantTimeFormat t}|], 
                                    Just (H.span ! A.class_ "green-dot" $ ""), 
                                    toHtml (m ^. #metrics . #rrdpSource))
                                FailedAt t  -> 
                                    ([i|Failed at #{instantTimeFormat t}|], 
                                    Just (H.span ! A.class_ "red-dot" $ ""),
                                    "-")

                    genTd $ H.a ! A.href (textValue u_) $ H.text u_
                    td ! A.class_ "gen-t no-wrap" $ forM_ dot Prelude.id >> toHtml statusHtml                                                
                    td ! A.class_ "gen-t no-wrap" $ rrdpSource
                    genTd $ toHtml $ show $ totalMapCount $ m ^. #metrics . #added
                    genTd $ toHtml $ show $ totalMapCount $ m ^. #metrics . #deleted
                    genTd $ toHtml $ m ^. #metrics . #downloadTimeMs                                
                    genTd $ toHtml $ m ^. #metrics . #totalTimeMs                    

                detailRow m index
  where
    detailRow :: RrdpRepositoryUIDto -> Int -> H.Html
    detailRow m index = H.tr ! A.id (H.toValue $ "detail-row-" <> show index)
                        ! A.class_ "detail-row"
                        ! A.style "display: none;" $ do

        H.td ! A.colspan "8" ! A.class_ "gen-t detail-content" $ 
            H.div ! A.class_ "detail-panel" $ do
                detailGrid
                unless (Prelude.null $ m ^. #validations) $ do 
                    issuesList m
      where    
        detailGrid = H.div ! A.class_ "detail-grid" $ do
            detailItem "Last Session ID:" (maybe "-" unSessionId $ m ^? #repository . #rrdpMeta . _Just . #sessionId)
            detailItem "Serial Number:" (maybe "-" show $ m ^? #repository . #rrdpMeta . _Just . #serial)
            detailItem "Refresh interval:" (maybe "-" show $ m ^. #repository . #meta . #refreshInterval)
            detailItem "Last HTTP status:" (show $ m ^. #metrics . #lastHttpStatus)
            detailItem "Uses E-Tag:" $ 
                case m ^? #repository . #eTag of 
                    Just _ -> "Yes" :: Text
                    _      -> "No"

        issuesList m =             
            H.div ! A.class_ "d-i issues-container" $ do
                H.strong "Issues:"
                H.ul ! A.class_ "issues-list" $ do
                    forM_ (m ^. #validations) $ \(ResolvedVDto (FullVDto{..})) ->
                        forM_ issues $ \issue -> do
                            let (dotClass, issueText, itemClass) = case issue of
                                    ErrorDto err -> ("red-dot", err, "error")
                                    WarningDto w -> ("yellow-dot", w, "warning")
                            H.li ! A.class_ ("issue-item " <> itemClass) $ do
                                H.span ! A.class_ dotClass $ ""
                                H.span ! A.class_ "issue-text" $ H.text issueText
            
        detailItem :: (ToMarkup a, IsString a) => a -> a -> H.Html
        detailItem label value = 
            H.div ! A.class_ "d-i" $ do
                H.strong (H.toHtml label)
                space
                H.span ! A.class_ "no-wrap" $ H.toHtml value

rsyncMetricsHtml :: [RsyncRepositoryUIDto] -> Html
rsyncMetricsHtml rsyncMetrics =
    H.table ! A.class_ "gen-t" $ do  
        H.thead $ tr $ do 
            genTh $ do 
                H.text "Repository (" >> toHtml (length rsyncMetrics) >> H.text " in total)" 
            genTh $ H.div ! A.class_ "tooltip" $ do
                H.text "Fetching"
                H.span ! A.class_ "tooltiptext" $ rsyncFetchTooltip            
            genTh $ H.text "Processed objects"
            genTh $ H.text "Total time"                    

        H.tbody $ do 
            let slowestFirst = List.sortOn (\m -> ordering $ m ^. #meta . #status) rsyncMetrics
            forM_ (zip slowestFirst [1 :: Int ..]) $ \(m, index) -> do                 
                htmlRow index $ do
                    genTd $ toHtml $ let URI u_ = getURL (m ^. #uri) in u_
                    genTd ! A.class_ "no-wrap" $ toHtml $ m ^. #metrics . #fetchFreshness            
                    genTd $ toHtml $ show $ totalMapCount $ m ^. #metrics . #processed            
                    genTd $ toHtml $ m ^. #metrics . #totalTimeMs         

ordering :: FetchStatus -> Down (Maybe (Instant, Int))
ordering status = 
    Down $ case status of     
        FetchedAt t -> Just (t, 1) 
        FailedAt t  -> Just (t, 0) 
        _           -> Nothing        

validaionDetailsHtml :: [ResolvedVDto] -> Html
validaionDetailsHtml result = 
    H.table ! A.class_ "gen-t" $ do 
        H.thead $ tr $ do 
            genTh $ H.span $ H.text "Issue"            
            genTh $ H.div ! A.class_ "tooltip" $ do
                H.text "URL/Scope"
                H.span ! A.class_ "tooltiptext" $ validationPathTootip               
        forM_ (Map.toList $ groupByTa result) $ \(ta, vrs) -> 
            H.tbody $ tr $ td ! A.class_ "even-row, gen-t" ! colspan "2" $ do 
                -- Open the small ones
                let detailElem = if length vrs < 10 then H.details ! A.open "" else H.details
                detailElem $ do 
                    H.summary $ H.strong $ do 
                        toHtml ta >> ":"
                        space >> space >> space
                        let (e, w) = countProblems vrs
                        toHtml e >> " errors, "
                        toHtml w >> " warnings"                    
                    H.table ! A.class_ "sub-t" $ 
                        H.tbody $ forM_ (zip vrs [1 :: Int ..]) vrHtml
            
  where      
    vrHtml (ResolvedVDto (FullVDto{..}), index) = do 
        let objectUrl = Prelude.head path         
        forM_ (zip issues [1 :: Int ..]) $ \(pr, jndex) ->                     
            htmlRow (index + jndex) $ do 
                let (marker, problem) = 
                        case pr of 
                            ErrorDto err -> ("red-dot",  err)                                        
                            WarningDto w -> ("yellow-dot", w)
                td ! A.class_ "sub-t" $ H.span $ do 
                    H.span ! A.class_ marker $ ""                    
                    mapM_ (\z -> H.text z >> H.br) $ Text.lines problem
                td ! A.class_ "sub-t" $ H.details $ do 
                    H.summary $ focusLink1 objectUrl
                    forM_ (Prelude.tail path) $ \f -> 
                        focusLink1 f >> H.br
    countProblems = 
        List.foldl' countP (0 :: Int, 0 :: Int)
      where
        countP z (ResolvedVDto FullVDto {..}) = List.foldl' countEW z issues
        countEW (!e, !w) (ErrorDto _)   = (e + 1, w)
        countEW (!e, !w) (WarningDto _) = (e, w + 1)


primaryRepoTooltip :: Html
primaryRepoTooltip = 
    H.text $ "For metrics purposes objects are associated with a repository they are downloaded from. " <> 
            "Fallback from RRDP to rsync does not change this association, so a valid object is attributed " <> 
            "to the RRDP repository even if it was downloaded from the rsync one because of the fall-back."

fetchTooltip :: Text -> Text -> Html
fetchTooltip repoType setting =               
    H.div ! A.style "text-align: left;" $ do 
        space >> space >> H.text "Used values" >> H.br
        H.ul $ do 
            H.li $ H.text [T.i|'Up-to-date' - no fetch is needed, #{repoType} repository was fetched less than '#{setting}' seconds ago.|]
            H.li $ H.text $ "'No updates' - there are not updates to fetch. In case of RRDP repository it " <> 
                            "means its serial didn't change since the last fetch, in case of rsync -- no new objects found after fetch."
            H.li $ H.text "'Updated' and 'Failed' are self-explanatory"

rrdpFetchTooltip, rsyncFetchTooltip :: Html
rrdpFetchTooltip  = fetchTooltip "RRDP" "rrdp-refresh-interval"
rsyncFetchTooltip = fetchTooltip "rsync" "rsync-refresh-interval"

rrdpUpdateTooltip :: Html
rrdpUpdateTooltip =
    H.div ! A.style "text-align: left;" $ do 
        space >> space >> H.text "Used values" >> H.br
        H.ul $ do 
            H.li $ H.text "'Snapshot N' - snapshot with serial N was used for RRDP update"
            H.li $ H.text "'Deltas N to M' - deltas from serial N to serial M were used for RRDP update "
            H.li $ H.text "'Up-to-date' - No update is needed, local and remote serials are equal or E-Tag didn't change"

validationPathTootip :: Html
validationPathTootip = do   
    space >> space
    H.text "Signs " >> arrowRight >> H.text " and " >> arrowUp >> H.text " are clickable. "
    H.text "'Scope' here shows the full sequence of objects from the TA to the object in question."
    space >> space
        

groupByTa :: [ResolvedVDto] -> Map Text [ResolvedVDto]
groupByTa vrs =
    Map.fromListWith (<>) 
    $ [ (resolvedFocusToText ta, [vr]) 
            | vr@(ResolvedVDto FullVDto {..}) <- vrs, 
              ta <- lastOne path ]    
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

htmlClickableRow :: Int -> Html -> Html
htmlClickableRow index = 
    tr ! A.class_ (evenRow <> "clickable-row") 
       ! BlazeI.dataAttribute ("target" :: Tag) (H.toValue $ "detail-row-" <> show index)
  where
    evenRow = 
        case index `mod` 2 of 
            0 -> "even-row "
            _ -> ""

genTd, genTh :: Html -> Html
genTd = td ! A.class_ "gen-t" 
genTh = th ! A.class_ "gen-t" 

space, arrowUp, arrowRight :: Html
space      = preEscapedToMarkup ("&nbsp;" :: Text)
arrowUp    = preEscapedToMarkup ("&#9650;" :: Text)
arrowRight = preEscapedToMarkup ("&#10095;" :: Text)


-- | Very crude formatting of a text to avoid using <pre> and screwing up the style.
--
htmlMessage :: Text -> Html
htmlMessage message = mapM_ (\z -> H.text z >> H.br) $ Text.lines message

instance ToMarkup TimeMs where 
    toMarkup (TimeMs ms) = toMarkup $ show ms <> "ms"

instance ToMarkup HttpStatus where 
    toMarkup (HttpStatus st) = toMarkup $ show st

instance ToMarkup FetchFreshness where 
    toMarkup = \case 
        NoFetchNeeded -> toMarkup ("Not yet" :: Text)
        FetchFailed   -> toMarkup ("Failed" :: Text)
        NoUpdates     -> toMarkup ("No updates" :: Text)
        Updated       -> toMarkup ("Updated" :: Text)

instance ToMarkup ValidatedBy where 
    toMarkup vb@(ValidatedBy v) = 
        -- TODO That's a hack, but will do
        if vb == mempty 
            then toMarkup ("-" :: Text)
            else toMarkup $ instantDateFormat $ versionToInstant v

instance ToMarkup RrdpSource where 
    toMarkup = \case 
        RrdpNoUpdate -> toMarkup ("Up-to-date" :: Text)
        RrdpDelta from to 
            | from == to -> [T.i|Delta #{from}|]
            | otherwise  -> [T.i|Deltas #{from} to #{to}|]                
        RrdpSnapshot serial -> let 
            message :: Text = [T.i|Snapshot #{serial}|]
            in toMarkup message
        
focusLink1 :: FocusResolvedDto -> Html
focusLink1 = \case 
    TextDto txt     -> toHtml txt
    TA_UI txt       -> toHtml txt
    ObjectLink txt  -> objectLink txt
    DirectLink uri  -> directLink uri        
  where
    directLink url = 
        H.a ! A.href (textValue url) $ toHtml url