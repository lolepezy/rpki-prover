{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StrictData           #-}

module RPKI.Http.UI where

import           Control.Monad
import           Control.Lens hiding (index)

import           Data.Bits                   ((.&.))
import           Data.Hashable               (Hashable)
import qualified Data.Hashable               as Hashable
import           Data.Maybe                  (listToMaybe)
import           Data.Ord
import           Data.String                 (IsString)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Numeric                     (showHex)

import qualified Data.List                   as List
import qualified Data.Map.Monoidal.Strict    as MonoidalMap
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
import           RPKI.Meta.Version

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
            H.title $ "RPKI Prover " <> toHtml rpkiProverVersionNumber
            link ! rel "icon" ! type_ "image/svg+xml" ! href "/static/favicon.svg"
            link ! rel "icon" ! href "/static/favicon.ico" ! sizes "32x32"
            link ! rel "stylesheet" ! href "/static/styles.css"
            H.script ! src "/static/script.js" $ ""
        H.body $
            H.div ! A.class_ "shell" $ do
                sideBar
                mainContent
  where
    grouped = metricsDto ^. #groupedValidations
    rrdpMetrics = [ d | RrdpDto d <- fetchDtos ]
    rsyncMetrics = [ d | RsyncDto d <- fetchDtos ]

    mainContent =
        H.div ! A.class_ "main" $ do
            H.div ! A.class_ "page-head" $ do
                H.h1 "Validation overview"
                H.div ! A.class_ "sub" $
                    [i|Last completed run · #{instantDateFormat $ versionToInstant version}|]

            kpiRow grouped perTaValidations generalValidations fetchDtos

            validationMetricsSection grouped

            unless (perTaValidations == mempty) $
                issuesSection perTaValidations

            unless (generalValidations == mempty) $
                generalIssuesSection generalValidations

            unless (Prelude.null rrdpMetrics) $
                rrdpMetricsHtml rrdpMetrics

            unless (Prelude.null rsyncMetrics) $
                rsyncMetricsHtml rsyncMetrics

    sideBar =
        H.aside ! A.class_ "side" $ do
            H.div ! A.class_ "side-head" $ do
                markIcon
                H.span ! A.class_ "brand" $ "RPKI Prover"
            H.nav ! A.class_ "side-scroll" $ do
                navigation
                linksHtml
            H.div ! A.class_ "side-foot" $ do
                systemInfoHtml
                themeToggle
      where
        systemInfoHtml = do
            let SystemInfo {..} = systemInfo
            infoRow "Version" rpkiProverVersion
            infoRow "Last validation" $ Text.pack $ instantDateFormat $ versionToInstant version
            infoRow "Startup time" $ Text.pack $ instantDateFormat startUpTime

        infoRow :: Text -> Text -> Html
        infoRow label_ value_ =
            H.div ! A.class_ "info-row" $ do
                H.span (toHtml label_)
                H.span (toHtml value_)

        themeToggle =
            H.button ! A.class_ "theme-toggle" ! A.id "themeToggle" ! A.type_ "button" $ do
                themeIcon
                H.span ! A.id "themeLabel" $ "Match system theme"

        linksHtml =
            H.div ! A.class_ "side-group" $ do
                H.p ! A.class_ "side-label" $ "Links"
                navLink linkIcon' "/swagger-ui" "Swagger API docs" Nothing
                navLink configIcon "/api/system" "Config & metrics" Nothing
                navLink githubIcon "https://github.com/lolepezy/rpki-prover" "GitHub repository" Nothing

        navigation = do
            H.div ! A.class_ "side-group" $ do
                H.p ! A.class_ "side-label" $ "Validation"
                navLink metricsIcon "#validation-metrics" "Metrics" Nothing

                unless (perTaValidations == mempty) $
                    navLink issuesIcon "#validation-issues" "Issues" (Just $ perTaIssueCount perTaValidations)

                unless (generalValidations == mempty) $
                    navLink issuesIcon "#general-issues" "Other issues" (Just $ let (e, w) = countProblemsAll generalValidations in e + w)

            H.div ! A.class_ "side-group" $ do
                H.p ! A.class_ "side-label" $ "Fetching"
                unless (Prelude.null rrdpMetrics) $
                    navLink rrdpIcon "#rrdp-fetches" "RRDP fetches" (Just $ length rrdpMetrics)
                unless (Prelude.null rsyncMetrics) $
                    navLink rsyncIcon "#rsync-fetches" "Rsync fetches" (Just $ length rsyncMetrics)

        navLink :: Html -> AttributeValue -> Text -> Maybe Int -> Html
        navLink icon_ url_ label_ count_ =
            H.a ! A.href url_ ! A.class_ "side-link" $ do
                icon_
                H.span (toHtml label_)
                for_ count_ $ \n ->
                    H.span ! A.class_ "count" $ toHtml n

    perTaIssueCount dtos =
        let (e, w) = countProblemsAll (concatMap snd (perTA dtos)) in e + w


-- | The KPI strip at the top of the page: the handful of numbers an
-- operator actually needs before drilling into any table.
kpiRow :: GroupedMetric ValidationMetric -> PerTA [ResolvedVDto] -> [ResolvedVDto] -> [RepositoryDto] -> Html
kpiRow grouped perTaValidations generalValidations fetchDtos =
    H.div ! A.class_ "kpi-grid" $ do        
        kpiTile (withCommas $ objectsCount total) "Objects validated"
            (Just $ withCommas (total ^. #validRoaNumber) <> " ROAs") False
        kpiTile (withCommas $ total ^. #uniqueVrpNumber) "Unique VRPs"
            (Just $ withCommas (total ^. #vrpCounter) <> " before dedup") False            
        kpiTile (withCommas $ total ^. #validAspaNumber) "ASPAs" (Nothing :: Maybe String) False
        kpiTile (withCommas errCount) "Open validation errors"
            (Just $ withCommas warnCount <> " warnings") (errCount /= 0)
        kpiTile (withCommas fetchFailures) "Fetch failures"
            (Just $ withCommas rrdpFails <> " RRDP · " <> withCommas rsyncFails <> " rsync") (fetchFailures /= 0)
  where
    total = grouped ^. #total

    allIssues = concatMap snd (perTA perTaValidations) <> generalValidations
    (errCount, warnCount) = countProblemsAll allIssues

    isFailed = \case FailedAt _ -> True; _ -> False
    rrdpFails  = length [ () | RrdpDto d  <- fetchDtos, isFailed (d ^. #repository . #meta . #status) ]
    rsyncFails = length [ () | RsyncDto d <- fetchDtos, isFailed (d ^. #meta . #status) ]
    fetchFailures = rrdpFails + rsyncFails

    kpiTile fig_ label_ sub_ isCrit =
        H.div ! A.class_ "kpi" $ do
            H.div ! A.class_ (if isCrit then "fig num crit" else "fig num") $ toHtml fig_
            H.div ! A.class_ "lbl" $ toHtml (label_ :: Text)
            for_ sub_ $ \sub' -> H.div ! A.class_ "sub num" $ toHtml sub'


validationMetricsSection :: GroupedMetric ValidationMetric -> Html
validationMetricsSection grouped =
    H.section ! A.id "validation-metrics" $ do
        let repoMetrics = MonoidalMap.toAscList $ grouped ^. #byRepository
        let taMetrics   = MonoidalMap.toAscList $ grouped ^. #byTa

        H.div ! A.class_ "sec-head" $ do
            H.h2 $ do
                "Trust anchors "
                H.span ! A.class_ "hint num" $ do "("; toHtml (length taMetrics); ")"
            H.span ! A.class_ "hint" $ "scroll for all 14 metrics · first column and header stay put"

        H.div ! A.class_ "table-card" $
            H.div ! A.class_ "table-scroll" $
                H.table $ do
                    H.thead $ tr $ do
                        th "Trust anchor"
                        th "Validated at"
                        th "Time"
                        th "Original VRPs"
                        th "Unique VRPs"
                        tailHeaders
                    H.tbody $ do
                        forM_ taMetrics $ \(TaName ta, vm) -> taMetricRow ta vm
                        totalMetricRow (grouped ^. #total)

        unless (repoMetrics == mempty) $
            H.details ! A.class_ "table-card collapsible" ! A.style "margin-top:16px;" $ do
                H.summary $ do
                    chevIcon
                    H.span "Per-repository detail"
                    H.span ! A.class_ "hint num" $ do toHtml (length repoMetrics); " repositories"
                    H.span ! A.class_ "ts-note" $
                        "objects are attributed to the RRDP repository even when a fallback to rsync fetched them"
                H.div ! A.class_ "table-scroll" $
                    H.table $ do
                        H.thead $ tr $ do
                            th "Repository"
                            th "Original VRPs"
                            tailHeaders
                        H.tbody $ do
                            let sortedRepos = List.sortOn fst $
                                    Prelude.map (\(u', z) -> (unURI $ getURL u', z)) repoMetrics
                            forM_ sortedRepos $ \(url, vm) -> repoMetricRow url vm
  where
    tailHeaders = do
        th "Objects"; th "ROAs"; th "Certs"; th "Manifests"; th "CRLs"
        th "GBRs"; th "ASPAs"; th "BGP certs"; th "Prefix lists"

    metricTail vm = do
        numTd (objectsCount vm)
        numTd (vm ^. #validRoaNumber)
        numTd (vm ^. #validCertNumber)
        numTd (vm ^. #validMftNumber)
        numTd (vm ^. #validCrlNumber)
        numTd (vm ^. #validGbrNumber)
        numTd (vm ^. #validAspaNumber)
        numTd (vm ^. #validBgpNumber)
        numTd (vm ^. #validSplNumber)

    taMetricRow ta vm =
        tr $ do
            td ! A.class_ "rowhead" $ toHtml ta
            td ! A.class_ "num" $ toHtml (vm ^. #validatedBy)
            numTdMs (vm ^. #totalTimeMs)
            numTd (vm ^. #vrpCounter)
            numTd (vm ^. #uniqueVrpNumber)
            metricTail vm

    totalMetricRow vm =
        tr ! A.class_ "total" $ do
            td ! A.class_ "rowhead" $ "Total"
            td ! A.class_ "num" $ "–"
            td ! A.class_ "num" $ "–"
            numTd (vm ^. #vrpCounter)
            numTd (vm ^. #uniqueVrpNumber)
            metricTail vm

    repoMetricRow url vm =
        tr $ do
            td ! A.class_ "rowhead url-cell" $ toHtml url
            numTd (vm ^. #vrpCounter)
            metricTail vm

-- | Sum of every valid-object counter -- how many objects a TA or
-- repository actually contributed to the tree.
objectsCount :: ValidationMetric -> Count
objectsCount vm =
       vm ^. #validCertNumber
     + vm ^. #validRoaNumber
     + vm ^. #validMftNumber
     + vm ^. #validCrlNumber
     + vm ^. #validGbrNumber
     + vm ^. #validAspaNumber
     + vm ^. #validBgpNumber
     + vm ^. #validSplNumber


rrdpMetricsHtml :: [RrdpRepositoryDto] -> Html
rrdpMetricsHtml rrdpMetrics =
    H.section ! A.id "rrdp-fetches" $ do
        H.div ! A.class_ "sec-head" $ do
            H.h2 $ do
                "RRDP fetches "
                H.span ! A.class_ "hint num" $ do "("; toHtml (length rrdpMetrics); ")"
            H.span ! A.class_ "hint" $ "Snapshot N / Deltas N–M / Up-to-date"
            filterBox "rrdpFilter"
        H.div ! A.class_ "table-card" $ do
            H.div ! A.class_ "table-scroll" $
                H.table ! A.id "rrdpTable" $ do
                    H.thead $ tr $ do
                        th "Repository"
                        th "Status"
                        th "Update"
                        th "Added"
                        th "Deleted"
                        th "Download"
                        th "Total"

                    H.tbody $ do
                        let recentFirst = List.sortOn (\m -> ordering $ m ^. #repository . #meta . #status) rrdpMetrics
                        forM_ (zip recentFirst [1 :: Int ..]) $ \(m, index) -> do
                            htmlClickableRow index rrdpDetailRow $ do
                                let URI u_ = getURL $ m ^. #uri
                                    (statusCls, statusText, rrdpSource) =
                                        case m ^. #repository . #meta . #status of
                                            Pending     ->
                                                ("pending", "Pending" :: Text, "-")
                                            FetchedAt t ->
                                                ("ok", [i|Fetched at #{instantTimeFormat t}|], toHtml (m ^. #metrics . #rrdpSource))
                                            FailedAt t  ->
                                                ("bad", [i|Failed at #{instantTimeFormat t}|], "-")

                                td ! A.class_ "url-cell" $ do
                                    chevIcon
                                    H.a ! A.href (textValue u_) $ H.text u_
                                td $ H.span ! A.class_ ("status " <> statusCls) $ do
                                    H.span ! A.class_ "dot" $ ""
                                    toHtml statusText
                                td ! A.class_ "num" $ rrdpSource
                                numTd (totalMapCount $ m ^. #metrics . #added)
                                numTd (totalMapCount $ m ^. #metrics . #deleted)
                                numTdMs (m ^. #metrics . #downloadTimeMs)
                                numTdMs (m ^. #metrics . #totalTimeMs)

                            detailRow m index
        H.div ! A.style "text-align:right; margin-top:8px;" $
            H.span ! A.class_ "hint" $ "click a row to see session details and any per-fetch issues"
  where
    detailRow :: RrdpRepositoryDto -> Int -> H.Html
    detailRow m index = H.tr ! A.id (H.toValue $ rrdpDetailRow index)
                        ! A.class_ "detail-row"
                        ! A.style "display: none;" $ do

        H.td ! A.colspan "7" ! A.class_ "detail-content" $
            H.div ! A.class_ "detail-panel" $ do
                detailGrid
                unless (Prelude.null $ m ^. #validations) $
                    issuesList m
      where
        detailGrid = H.div ! A.class_ "detail-grid" $ do
            detailItemNoWrap "Last Session ID:" (maybe "-" unSessionId $ m ^? #repository . #rrdpMeta . _Just . #sessionId)
            detailItemNoWrap "Serial Number:" (maybe "-" show $ m ^? #repository . #rrdpMeta . _Just . #serial)
            detailItemNoWrap "Refresh interval:" (maybe "-" show $ m ^. #repository . #meta . #refreshInterval)
            detailItemNoWrap "Last HTTP status:" (show $ m ^. #metrics . #lastHttpStatus)
            detailItemNoWrap "Uses E-Tag:" $
                case m ^. #repository . #eTag of
                    Just _ -> "Yes" :: Text
                    _      -> "No"

            for_ (m ^? #repository . #rrdpMeta . _Just . #enforcement . _Just) $ \enforcement ->
                detailItemWrap "Enforcement:" $
                    case enforcement of
                        NextTimeFetchSnapshot t _ -> [i|Next time fetch snapshot (marked at #{instantTimeFormat t}, see logs for details)|]
                        ForcedSnaphotAt t         -> [i|Forced snapshot at #{instantDateFormat t}|] :: Text

        detailItemWrap :: (ToMarkup a, IsString a) => a -> a -> H.Html
        detailItemWrap label_ value_ =
            detailItem label_ value_ Prelude.id

        detailItemNoWrap :: (ToMarkup a, IsString a) => a -> a -> H.Html
        detailItemNoWrap label_ value_ =
            detailItem label_ value_ $ \v ->
                H.span ! A.class_ "no-wrap" $ v

        detailItem label_ value_ wrap_ =
            H.div ! A.class_ "d-i" $ do
                H.strong (H.toHtml label_)
                wrap_ $ H.toHtml value_


rsyncMetricsHtml :: [RsyncRepositoryDto] -> Html
rsyncMetricsHtml rsyncMetrics =
    H.section ! A.id "rsync-fetches" $ do
        H.div ! A.class_ "sec-head" $ do
            H.h2 $ do
                "Rsync fetches "
                H.span ! A.class_ "hint num" $ do "("; toHtml (length rsyncMetrics); ")"
            filterBox "rsyncFilter"
        H.div ! A.class_ "table-card" $
            H.div ! A.class_ "table-scroll" $
                H.table ! A.id "rsyncTable" $ do
                    H.thead $ tr $ do
                        th "Repository"
                        th "Status"
                        th "Processed objects"
                        th "Total time"

                    H.tbody $ do
                        let slowestFirst = List.sortOn (\m -> ordering $ m ^. #meta . #status) rsyncMetrics
                        forM_ (zip slowestFirst [1 :: Int ..]) $ \(m, index) -> do
                            let hasDetail = not $ Prelude.null $ m ^. #validations
                            (if hasDetail then htmlClickableRow index rsyncDetailRow else tr) $ do
                                let (statusCls, statusText) =
                                        case m ^. #meta . #status of
                                            Pending     -> ("pending", "Pending" :: Text)
                                            FetchedAt t -> ("ok", [i|Fetched at #{instantTimeFormat t}|])
                                            FailedAt t  -> ("bad", [i|Failed at #{instantTimeFormat t}|])

                                td ! A.class_ "url-cell" $ do
                                    when hasDetail chevIcon
                                    toHtml $ let URI u_ = getURL (m ^. #uri) in u_
                                td $ H.span ! A.class_ ("status " <> statusCls) $ do
                                    H.span ! A.class_ "dot" $ ""
                                    toHtml statusText
                                numTd (totalMapCount $ m ^. #metrics . #processed)
                                numTdMs (m ^. #metrics . #totalTimeMs)

                            when hasDetail $
                                detailRow m index
  where
    detailRow :: RsyncRepositoryDto -> Int -> H.Html
    detailRow m index = H.tr ! A.id (H.toValue $ rsyncDetailRow index)
                        ! A.class_ "detail-row"
                        ! A.style "display: none;" $
        H.td ! A.colspan "4" ! A.class_ "detail-content" $
            H.div ! A.class_ "detail-panel" $
                issuesList m


issuesList :: (Foldable t,  HasField' "validations" s (t ResolvedVDto)) => s -> Html
issuesList m =
    H.div ! A.class_ "issues-container" $ do
        H.strong "Issues"
        H.ul ! A.class_ "issues-list" $
            forM_ (m ^. #validations) $ \(ResolvedVDto (ValidationDto{..})) ->
                forM_ issues $ \issue -> do
                    let (dotClass, issueText) = case issue of
                            ErrorDto err -> ("red-dot", err)
                            WarningDto w -> ("yellow-dot", w)
                    H.li ! A.class_ "issue-item" $ do
                        H.span ! A.class_ dotClass $ ""
                        H.span ! A.class_ "issue-text" $ H.text issueText

ordering :: FetchStatus -> Down (Maybe (Instant, Int))
ordering status =
    Down $ case status of
        FetchedAt t -> Just (t, 1)
        FailedAt t  -> Just (t, 0)
        _           -> Nothing


-- | Per-TA breakdown, each TA collapsible (closed once it has 10 or
-- more flagged objects, same threshold the page has always used).
issuesSection :: PerTA [ResolvedVDto] -> Html
issuesSection dtos =
    H.section ! A.id "validation-issues" $ do
        H.div ! A.class_ "sec-head" $ do
            H.h2 "Validation issues"
            H.span ! A.class_ "hint" $
                "each chain runs from the flagged object up to its trust anchor"
        forM_ (perTA dtos) $ \(TaName ta, vrs) ->
            unless (vrs == mempty) $ taIssueGroup ta vrs
  where
    taIssueGroup ta vrs =
        detailsEl ! A.class_ "ta-issue" $ do
            H.summary $ do
                chevIcon
                H.span ! A.class_ "ta-name" $ toHtml ta
                H.span ! A.class_ "counts" $ do
                    when (e > 0) $ H.span ! A.class_ "pill crit" $ do toHtml e; " err"
                    when (w > 0) $ H.span ! A.class_ "pill warn" $ do toHtml w; " warn"
            H.div ! A.class_ "body" $ issueRows ta vrs
      where
        (e, w) = countProblemsAll vrs
        detailsEl = if length vrs < 10 then H.details ! A.open "" else H.details


generalIssuesSection :: [ResolvedVDto] -> Html
generalIssuesSection dtos =
    H.section ! A.id "general-issues" $ do
        H.div ! A.class_ "sec-head" $ do
            H.h2 "Other issues"
            H.span ! A.class_ "hint" $ "issues not tied to a specific trust anchor"
        H.div ! A.class_ "ta-issue" $
            H.div ! A.class_ "body" $ issueRows "common" dtos


-- | One row per (validated object, issue) pair -- an object with two
-- problems gets two rows, each with its own copy of the chain, exactly
-- as before. Sorted by the leaf object so related issues land together.
issueRows :: Text -> [ResolvedVDto] -> Html
issueRows ta vrs =
    forM_ sortedVrs $ \(ResolvedVDto (ValidationDto{..})) ->
        forM_ issues $ \issue -> issueRow ta path issue
  where
    sortedVrs = List.sortOn (\(ResolvedVDto (ValidationDto{..})) -> listToMaybe path) vrs

issueRow :: Text -> [ResolvedFocusDto] -> IssueDto -> Html
issueRow ta path issue =
    H.div ! A.class_ "issue-row" ! A.id (textValue anchorId) $ do
        H.span ! A.class_ ("sev " <> sevCls) $ ""
        H.div ! A.class_ "txt" $ do
            H.div ! A.class_ "msg" $ do
                H.span ! A.class_ "msg-text" $ toHtml msg
                copyLinkButton anchorId
            renderPath path
  where
    (sevCls, msg) = case issue of
        ErrorDto t   -> ("crit" :: AttributeValue, t)
        WarningDto t -> ("warn", t)
    anchorId = issueAnchorId ta path msg

-- | A short, content-derived anchor for one issue, stable across
-- validation runs as long as the same object still has the same
-- problem -- so a shared link either keeps working or, correctly,
-- stops resolving once the issue is actually fixed.
issueAnchorId :: Text -> [ResolvedFocusDto] -> Text -> Text
issueAnchorId ta path msg =
    "issue-" <> Text.pack (showHex (hashNonNegative (ta, Prelude.map focusKey path, msg)) "")
  where
    focusKey = \case
        TextDto t    -> t
        TA_UI t      -> t
        ObjectLink t -> t
        DirectLink t -> t

hashNonNegative :: Hashable a => a -> Int
hashNonNegative x = Hashable.hash x .&. 0x7fffffff

renderPath :: [ResolvedFocusDto] -> Html
renderPath []             = pure ()
renderPath path@(leaf:rest)
    | Prelude.null rest = H.div ! A.class_ "path" $ leafFocusLink leaf
    | otherwise         =
        H.details ! A.class_ "path-details" $ do
            H.summary $ do
                leafFocusLink leaf
                " "
                H.span ! A.class_ "hop-count" $ do
                    "· "; toHtml (length rest); if length rest == 1 then " more hop" else " more hops"; " to the TA"
            H.ol ! A.class_ "path-chain" $
                forM_ path $ \f -> H.li $ focusLink1 f

countProblemsAll :: [ResolvedVDto] -> (Int, Int)
countProblemsAll =
    List.foldl' countVd (0 :: Int, 0 :: Int)
  where
    countVd z (ResolvedVDto ValidationDto {..}) = List.foldl' countIssue z issues
    countIssue (!e, !w) (ErrorDto _)   = (e + 1, w)
    countIssue (!e, !w) (WarningDto _) = (e, w + 1)


-- TODO This is quite ugly, find a better way to get a proper URL (using servant maybe)
focusLink1 :: ResolvedFocusDto -> Html
focusLink1 = focusLinkHtml Nothing

leafFocusLink :: ResolvedFocusDto -> Html
leafFocusLink = focusLinkHtml (Just "leaf")

focusLinkHtml :: Maybe AttributeValue -> ResolvedFocusDto -> Html
focusLinkHtml mCls = \case
    TextDto txt     -> withCls H.span $ toHtml txt
    TA_UI txt       -> withCls H.span $ toHtml txt
    ObjectLink txt  -> withCls (H.a ! A.href (textValue ("/api/object?uri=" <> txt))) $ toHtml txt
    DirectLink uri  -> withCls (H.a ! A.href (textValue uri)) $ toHtml uri
  where
    withCls :: (Html -> Html) -> (Html -> Html)
    withCls el = maybe el (\c -> el ! A.class_ c) mCls


copyLinkButton :: Text -> Html
copyLinkButton anchorId =
    H.button ! A.type_ "button" ! A.class_ "copy-link"
             ! BlazeI.dataAttribute ("anchor" :: Tag) (textValue anchorId)
             ! A.title "Copy link to this issue"
             $ linkIcon


rrdpDetailRow :: Int -> String
rrdpDetailRow index = "detail-row-rrdp-" <> show index

rsyncDetailRow :: Int -> String
rsyncDetailRow index = "detail-row-rsync-" <> show index

htmlClickableRow :: (Integral t, ToValue a) => t -> (t -> a) -> Html -> Html
htmlClickableRow index dataTarget =
    tr ! A.class_ "clickable-row"
       ! BlazeI.dataAttribute ("target" :: Tag) (H.toValue $ dataTarget index)

filterBox :: AttributeValue -> Html
filterBox inputId =
    H.div ! A.class_ "filter" $ do
        searchIcon
        H.input ! A.type_ "text" ! A.id inputId ! A.placeholder "Filter by repository…"

-- | Right-aligned, tabular-numeral cell. Zero is muted so the eye goes
-- straight to whichever columns actually have something in them.
numTd :: Show a => a -> Html
numTd n =
    let shown = show n
    in td ! A.class_ (if shown == "0" then "num zero" else "num") $ toHtml (withCommas n)

numTdMs :: TimeMs -> Html
numTdMs (TimeMs ms) = td ! A.class_ "num" $ toHtml (withCommas ms <> "ms")

-- | Thousand-separated rendering of any 'Show'-able integral-looking
-- value (works uniformly on 'Count', 'Int', 'Int64', ...).
withCommas :: Show a => a -> String
withCommas n =
    case Prelude.span (== '-') (show n) of
        (sign, digits) -> sign <> (reverse . List.intercalate "," . chunksOf3 . reverse) digits
  where
    chunksOf3 [] = []
    chunksOf3 xs = let (h, t) = splitAt 3 xs in h : chunksOf3 t

-- ---------------------------------------------------------------------
-- Small inline icons. Kept as raw markup (single-quoted attributes, no
-- escaping needed) rather than pulling in a dedicated SVG-combinator
-- dependency for a handful of static glyphs.
-- ---------------------------------------------------------------------

markIcon :: Html
markIcon = preEscapedToMarkup
    ("<svg class='mark' width='24' height='24' viewBox='0 0 256 256' aria-hidden='true' fill='none' stroke-linecap='round' stroke-linejoin='round'>\
     \<g stroke='var(--ink-muted)' stroke-width='21'><path d='M64 74 V182'/><path d='M64 128 H112'/></g>\
     \<g stroke='var(--ink)' stroke-width='25'><path d='M138 190 L172 72 L206 190'/></g>\
     \</svg>" :: Text)

chevIcon :: Html
chevIcon = preEscapedToMarkup
    ("<svg class='chev' width='13' height='13' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='3'><path d='M9 6l6 6-6 6'/></svg>" :: Text)

searchIcon :: Html
searchIcon = preEscapedToMarkup
    ("<svg width='14' height='14' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><circle cx='11' cy='11' r='7'/><path d='M21 21l-4.3-4.3'/></svg>" :: Text)

linkIcon :: Html
linkIcon = preEscapedToMarkup
    ("<svg width='13' height='13' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><path d='M10 14a3.5 3.5 0 0 0 5 0l3-3a3.5 3.5 0 0 0-5-5l-1 1'/><path d='M14 10a3.5 3.5 0 0 0-5 0l-3 3a3.5 3.5 0 0 0 5 5l1-1'/></svg>" :: Text)

themeIcon :: Html
themeIcon = preEscapedToMarkup
    ("<svg width='14' height='14' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><path d='M12 3a9 9 0 109 9 9 9 0 01-9-9z'/></svg>" :: Text)

metricsIcon :: Html
metricsIcon = preEscapedToMarkup
    ("<svg width='15' height='15' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><path d='M3 3v18h18M8 17V9m5 8V5m5 12v-6'/></svg>" :: Text)

issuesIcon :: Html
issuesIcon = preEscapedToMarkup
    ("<svg width='15' height='15' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><circle cx='12' cy='12' r='9'/><path d='M12 8v5M12 16h.01'/></svg>" :: Text)

rrdpIcon :: Html
rrdpIcon = preEscapedToMarkup
    ("<svg width='15' height='15' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><path d='M12 3v12m0 0l4-4m-4 4l-4-4M4 17v2a2 2 0 002 2h12a2 2 0 002-2v-2'/></svg>" :: Text)

rsyncIcon :: Html
rsyncIcon = preEscapedToMarkup
    ("<svg width='15' height='15' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><path d='M17 2l4 4-4 4M3 12v-2a4 4 0 014-4h14M7 22l-4-4 4-4M21 12v2a4 4 0 01-4 4H3'/></svg>" :: Text)

linkIcon' :: Html
linkIcon' = preEscapedToMarkup
    ("<svg width='15' height='15' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><path d='M4 17l6-6 4 4 6-6'/></svg>" :: Text)

configIcon :: Html
configIcon = preEscapedToMarkup
    ("<svg width='15' height='15' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><circle cx='12' cy='12' r='9'/><path d='M12 7v6l4 2'/></svg>" :: Text)

githubIcon :: Html
githubIcon = preEscapedToMarkup
    ("<svg width='15' height='15' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2'><path d='M9 19c-4 1-4-2-6-2m12 4v-3.5c0-1 .3-1.6 1-2.2-3.2-.4-6.5-1.6-6.5-7A5.6 5.6 0 0111 4.8a5.3 5.3 0 015.6 0A5.6 5.6 0 0118 8.3c0 5.4-3.3 6.6-6.5 7 .7.6 1 1.4 1 2.4V21'/></svg>" :: Text)


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
