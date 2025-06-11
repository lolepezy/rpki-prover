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
import           Control.Lens hiding (index) 

import           Data.Ord
import           Data.Text                   (Text)
import qualified Data.Text                   as Text

import qualified Data.List                   as List
import qualified Data.Map.Monoidal.Strict    as MonoidalMap
import           Data.String                 (IsString)
import           Data.Generics.Product.Fields
import           Data.Foldable               (for_)

import           Data.String.Interpolate.IsString

import           Text.Blaze.Html5            as H hiding (i)
import           Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal         as BlazeI

import           RPKI.AppTypes
import           RPKI.AppState
import           RPKI.Domain
import           RPKI.Http.Types
import           RPKI.RRDP.Types
import           RPKI.Metrics.Metrics
import           RPKI.Metrics.System
import           RPKI.Repository
import           RPKI.Reporting
import           RPKI.Time
import           RPKI.Version

mainPage :: WorldVersion
        -> SystemInfo 
        -> PerTA [ResolvedVDto]
        -> [ResolvedVDto]
        -> [RepositoryDto]
        -> MetricsDto
        -> Html
mainPage version systemInfo perTaValidations generalValidations fetchDtos metricsDto =     
    H.docTypeHtml $ do
        H.head $ do
            link ! rel "stylesheet" ! href "/static/styles.css"
            H.script ! src "/static/script.js" $ ""
        H.body $ do        
            sideBar
            mainContent
  where
    mainContent = do 
        H.div ! A.class_ "main" $ do            
            H.a ! A.id "validation-metrics" $ "" 
            H.section $ H.h3 "Validation metrics"                        
            validationMetricsHtml $ metricsDto ^. #groupedValidations

            unless (perTaValidations == mempty) $ do
                H.a ! A.id "validation-issues" $ ""
                H.section $ H.h3 "Validation issues"
                validaionIssuesHtml perTaValidations 

            unless (generalValidations == mempty) $ do
                H.a ! A.id "general-issues" $ ""
                H.section $ H.h3 "Other issues"
                generalIssuesHtml generalValidations 

            let rrdpMetrics = [ d | RrdpDto d <- fetchDtos ]
            unless (Prelude.null rrdpMetrics) $ do
                H.a ! A.id "rrdp-fetches" $ ""
                H.section $ H.h3 "RRDP fetches"
                rrdpMetricsHtml rrdpMetrics

            let rsyncMetrics = [ d | RsyncDto d <- fetchDtos ]
            unless (Prelude.null rsyncMetrics) $ do 
                H.a ! A.id "rsync-fetches" $ ""
                H.section $ H.h3 "Rsync fetches"
                rsyncMetricsHtml rsyncMetrics        

    sideBar = do
        H.div ! A.class_ "side-navigation" $ do
            navigation
            systemInfoHtml        
            linksHtml
      where 
        systemInfoHtml = do 
            let SystemInfo {..} = systemInfo
            H.div ! A.class_ "system-info-section" $ do                
                H.div ! A.class_ "info-item" $ do
                    H.span ! A.class_ "info-label" $ H.text "Version"
                    H.span ! A.class_ "info-value" $ H.text rpkiProverVersion                    
                
                H.div ! A.class_ "info-item" $ do
                    H.span ! A.class_ "info-label" $ H.text "Last validation"
                    H.span ! A.class_ "info-value" $ H.text $ Text.pack $ instantDateFormat $ versionToInstant version
                
                H.div ! A.class_ "info-item" $ do
                    H.span ! A.class_ "info-label" $ H.text "Startup time"
                    H.span ! A.class_ "info-value" $ H.text $ Text.pack $ instantDateFormat startUpTime

        linksHtml = do 
            H.div ! A.class_ "quick-links-section" $ do
                H.h3 ! A.class_ "system-info-title" $ H.text "Links"

                link_ "/swagger-ui" "Swagger API Documentation"
                link_ "/api/system" "Configuration & Metrics"
                link_ "https://github.com/lolepezy/rpki-prover" "GitHub Repository"

        navigation = do
            H.div ! A.class_ "nav-section" $ do
                H.h3 ! A.class_ "system-info-title" $ H.text "Validation"

                unless (perTaValidations == mempty) $ do
                    link_ "#validation-metrics" "Metrics"
                    link_ "#validation-issues" "Issues"                

                link_ "#rrdp-fetches" "RRDP fetches"
                link_ "#rsync-fetches" "Rsync fetches"

        link_ url linkText = do
            H.a ! A.href url ! A.class_ "quick-link" $ do
                H.span ! A.class_ "link-desc" $ H.text linkText


validationMetricsHtml :: GroupedMetric ValidationMetric -> Html
validationMetricsHtml grouped = do 
    
    let repoMetrics = MonoidalMap.toAscList $ grouped ^. #byRepository
    let taMetrics   = MonoidalMap.toAscList $ grouped ^. #byTa        
    
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
    unless (repoMetrics == mempty) $ 
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


rrdpMetricsHtml :: [RrdpRepositoryDto] -> Html
rrdpMetricsHtml rrdpMetrics =
    unless (rrdpMetrics == mempty) $ do
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
                let recentFirst = List.sortOn (\m -> ordering $ m ^. #repository . #meta . #status) rrdpMetrics
                forM_ (zip recentFirst [1 :: Int ..]) $ \(m, index) -> do 
                    htmlClickableRow index rrdpDetailRow $ do 
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
    detailRow :: RrdpRepositoryDto -> Int -> H.Html
    detailRow m index = H.tr ! A.id (H.toValue $ rrdpDetailRow index)
                        ! A.class_ "detail-row"
                        ! A.style "display: none;" $ do

        H.td ! A.colspan "7" ! A.class_ "gen-t detail-content" $ 
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
                case m ^. #repository . #eTag of 
                    Just _ -> "Yes" :: Text
                    _      -> "No"            

            for_ (m ^? #repository . #rrdpMeta . _Just . #enforcement . _Just) $ \enforcement ->                
                detailItem "Enforcement:" $ 
                    case enforcement of 
                        NextTimeFetchSnapshot t _ -> [i|Next time fetch snapshot (marked at #{instantTimeFormat t}, see logs for details)|]
                        ForcedSnaphotAt t         -> [i|Forced snapshot at #{instantTimeFormat t}|] :: Text            
                                    
        detailItem :: (ToMarkup a, IsString a) => a -> a -> H.Html
        detailItem label_ value_ = 
            H.div ! A.class_ "d-i" $ do
                H.strong (H.toHtml label_)
                space
                H.toHtml value_


rsyncMetricsHtml :: [RsyncRepositoryDto] -> Html
rsyncMetricsHtml rsyncMetrics =
    unless (rsyncMetrics == mempty) $ do    
        H.table ! A.class_ "gen-t" $ do  
            H.thead $ tr $ do 
                genTh $ do 
                    H.text "Repository (" >> toHtml (length rsyncMetrics) >> H.text " in total)" 
                genTh $ H.div ! A.class_ "tooltip" $ do
                    H.text "Updated at"                
                genTh $ H.text "Processed objects"
                genTh $ H.text "Total time"                    

            H.tbody $ do 
                let slowestFirst = List.sortOn (\m -> ordering $ m ^. #meta . #status) rsyncMetrics
                forM_ (zip slowestFirst [1 :: Int ..]) $ \(m, index) -> do          
                    htmlClickableRow index rsyncDetailRow $ do 
                        let (statusHtml, dot) = 
                                case m ^. #meta . #status of 
                                    Pending     -> 
                                        ("Pending" :: Text, Nothing)
                                    FetchedAt t -> 
                                        ([i|Fetched at #{instantTimeFormat t}|], 
                                        Just (H.span ! A.class_ "green-dot" $ ""))
                                    FailedAt t  -> 
                                        ([i|Failed at #{instantTimeFormat t}|], 
                                        Just (H.span ! A.class_ "red-dot" $ ""))
                    
                        genTd $ toHtml $ let URI u_ = getURL (m ^. #uri) in u_
                        td ! A.class_ "gen-t no-wrap" $ forM_ dot Prelude.id >> toHtml statusHtml
                        genTd $ toHtml $ show $ totalMapCount $ m ^. #metrics . #processed            
                        genTd $ toHtml $ m ^. #metrics . #totalTimeMs         
                    
                    unless (Prelude.null $ m ^. #validations) $ 
                        detailRow m index
  where
    detailRow :: RsyncRepositoryDto -> Int -> H.Html
    detailRow m index = H.tr ! A.id (H.toValue $ rsyncDetailRow index)
                        ! A.class_ "detail-row"
                        ! A.style "display: none;" $ do

        H.td ! A.colspan "4" ! A.class_ "gen-t detail-content" $ 
            H.div ! A.class_ "detail-panel" 
                  ! A.style "padding-top: 0px;" $                     
                        issuesList m                        


issuesList :: (Foldable t,  HasField' "validations" s (t ResolvedVDto)) => s -> Html
issuesList m =             
    H.div ! A.class_ "d-i issues-container" $ do
        H.strong "Issues:"
        H.ul ! A.class_ "issues-list" $ do
            forM_ (m ^. #validations) $ \(ResolvedVDto (ValidationDto{..})) ->
                forM_ issues $ \issue -> do
                    let (dotClass, issueText, itemClass) = case issue of
                            ErrorDto err -> ("red-dot", err, "error")
                            WarningDto w -> ("yellow-dot", w, "warning")
                    H.li ! A.class_ ("issue-item " <> itemClass) $ do
                        H.span ! A.class_ dotClass $ ""
                        H.span ! A.class_ "issue-text" $ H.text issueText

ordering :: FetchStatus -> Down (Maybe (Instant, Int))
ordering status = 
    Down $ case status of     
        FetchedAt t -> Just (t, 1) 
        FailedAt t  -> Just (t, 0) 
        _           -> Nothing        


validaionIssuesHtml :: PerTA [ResolvedVDto] -> Html
validaionIssuesHtml dtos = 
    unless (dtos == mempty) $ do
        H.table ! A.class_ "gen-t" $ do 
            H.thead $ tr $ do 
                genTh $ H.span $ H.text "Issue"            
                genTh $ H.div ! A.class_ "tooltip" $ do
                    H.text "URL/Scope"
                    H.span ! A.class_ "tooltiptext" $ validationPathTootip               
            forM_ (perTA dtos) $ \(TaName ta, vrs) -> 
                unless (vrs == mempty) $ do                 
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
    vrHtml (ResolvedVDto (ValidationDto{..}), index) = do 
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
        countP z (ResolvedVDto ValidationDto {..}) = List.foldl' countEW z issues
        countEW (!e, !w) (ErrorDto _)   = (e + 1, w)
        countEW (!e, !w) (WarningDto _) = (e, w + 1)


generalIssuesHtml :: [ResolvedVDto] -> Html
generalIssuesHtml dtos = 
    H.table ! A.class_ "gen-t" $ do 
        H.thead $ tr $ do 
            genTh $ H.span $ H.text "Issue"            
            genTh $ H.div ! A.class_ "tooltip" $ do
                H.text "URL/Scope"
                H.span ! A.class_ "tooltiptext" $ validationPathTootip               
        H.tbody $
            forM_ (zip dtos [1 :: Int ..]) $ \(ResolvedVDto (ValidationDto{..}), index) -> do 
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

primaryRepoTooltip :: Html
primaryRepoTooltip = 
    H.text $ "For metrics purposes objects are associated with a repository they are downloaded from. " <> 
            "Fallback from RRDP to rsync does not change this association, so a valid object is attributed " <> 
            "to the RRDP repository even if it was downloaded from the rsync one because of the fall-back."

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
    H.text "'Scope' here shows the full sequence of objects from the TA to the object in question."
    space >> space
        


-- TODO This is quite ugly, find a better way to get a proper URL (using servant maybe)
objectLink :: Text -> Html
objectLink url = 
    H.a ! A.href (textValue ("/api/object?uri=" <> url)) $ toHtml url

htmlRow :: Int -> Html -> Html
htmlRow index = 
    case index `mod` 2 of 
        0 -> tr ! A.class_ "even-row"
        _ -> tr 

rrdpDetailRow :: Int -> String
rrdpDetailRow index = "detail-row-rrdp-" <> show index

rsyncDetailRow :: Int -> String
rsyncDetailRow index = "detail-row-rsync-" <> show index

htmlClickableRow :: (Integral t, ToValue a) => t -> (t -> a) -> Html -> Html
htmlClickableRow index dataTarget = 
    tr ! A.class_ (evenRow <> "clickable-row") 
       ! BlazeI.dataAttribute ("target" :: Tag) (H.toValue $ dataTarget index)
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
        RrdpDelta from_ to_ 
            | from_ == to_ -> [i|Delta #{from_}|]
            | otherwise    -> [i|Deltas #{from_} to #{to_}|]
        RrdpSnapshot serial -> let 
            message :: Text = [i|Snapshot #{serial}|]
            in toMarkup message
        
focusLink1 :: ResolvedFocusDto -> Html
focusLink1 = \case 
    TextDto txt     -> toHtml txt
    TA_UI txt       -> toHtml txt
    ObjectLink txt  -> objectLink txt
    DirectLink uri  -> directLink uri        
  where
    directLink url = 
        H.a ! A.href (textValue url) $ toHtml url