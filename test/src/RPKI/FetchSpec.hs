module RPKI.FetchSpec where

import           Control.Lens
import           Data.Text (Text)
import           Data.String.Interpolate.IsString
import           Data.Hourglass                  (Seconds(..))

import           Test.Tasty
import           Test.Tasty.HUnit                as HU

import qualified Data.Map.Strict                 as Map
import qualified Data.Map.Monoidal.Strict        as MonoidalMap
import qualified Data.IxSet.Typed                as IxSet

import           RPKI.AppTypes
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Repository
import           RPKI.Fetch.Fetch
import           RPKI.Time
import           RPKI.TestCommons


uriTaSet :: [(Text, Text)] -> UriTaIxSet
uriTaSet pairs = IxSet.fromList [
    UrlTA (RrdpU (RrdpURL (URI url))) (TaName ta)
    | (url, ta) <- pairs
    ]

taFetchables :: [(Text, [Text])] -> Map.Map TaName Fetcheables
taFetchables = Map.fromList . map convertEntry
  where
    convertEntry (ta, urls) = (TaName ta, Fetcheables $ MonoidalMap.fromList primaryFallbacks)
      where
        primaryFallbacks = [(RrdpU (RrdpURL (URI primary)), mempty) | primary <- urls ]

repo1, repo2, repo3, oldRepo :: Text
repo1 = "https://repo1.example.com"
repo2 = "https://repo2.example.com"
repo3 = "https://repo3.example.com"
oldRepo = "https://old-repo.example.com"

ta1, ta2, ta3 :: Text
ta1 = "TA1"
ta2 = "TA2"
ta3 = "TA3"

workflowSpec :: TestTree
workflowSpec = testGroup "Fetching" [ 
        urisPerTaSpec,
        loadMarksSpec,
        refreshIntervalSpec
    ]

refreshIntervalSpec :: TestTree
refreshIntervalSpec = testGroup "Refresh interval" [ 

    HU.testCase "Takes the delay asked by the server from the error" $ do 
        serverAskedDelay (RrdpE $ RrdpServerBusy "u" (HttpStatus 503) (Just (Seconds 5))) @?= Just (Seconds 5)
        serverAskedDelay (RrdpE $ RrdpServerBusy "u" (HttpStatus 429) Nothing) @?= Nothing
        serverAskedDelay (RrdpE $ CantDownloadDelta "timeout") @?= Nothing

  , HU.testCase "Waits after a failure at least as long as the server asked, within the maximal back-off" $ do 
        Now now <- thisInstant
        let failed = FailedAt now
        let backoff = interval failed Nothing
        HU.assertBool [i|Back-off #{backoff} should be short|] (backoff < Seconds 200)
        interval failed (Just (Seconds 600)) @?= Seconds 600
        interval failed (Just (Seconds 1)) @?= backoff
        interval failed (Just (Seconds 100_000)) @?= maxBackoff

  , HU.testCase "Ignores the delay after a success" $ do 
        Now now <- thisInstant
        interval (FetchedAt now) (Just (Seconds 600)) @?= interval (FetchedAt now) Nothing
    ]
  where
    repository = updateMeta' (newRepository (RrdpU (RrdpURL (URI repo1)))) 
                    (#refreshInterval ?~ Seconds 60)
    interval status askedDelay = 
        nextRefreshInterval testConfig repository (WorldVersion 1) status Nothing askedDelay (TimeMs 100)
    maxBackoff = testConfig ^. #fetchIntervalConfig . #maxFailedBackoffInterval


urisPerTaSpec :: TestTree
urisPerTaSpec = testGroup "URIs per TA" [ 

    HU.testCase "Adds entries to empty IxSet" $
        updateUriPerTa 
            (taFetchables [(ta1, [repo1])]) 
            (uriTaSet []) 
            @?= uriTaSet [(repo1, ta1)]

  , HU.testCase "Replaces all entries for a TA" $
        updateUriPerTa 
            (taFetchables [(ta1, [repo2])]) 
            (uriTaSet [(repo1, ta1)]) 
            @?= uriTaSet [(repo2, ta1)]

  , HU.testCase "Preserves entries for TAs not in fs" $
        updateUriPerTa 
            (taFetchables [(ta1, [repo1])]) 
            (uriTaSet [(repo2, ta2)]) 
            @?= uriTaSet [(repo1, ta1), (repo2, ta2)]

  , HU.testCase "Handles multiple URLs per TA" $
        updateUriPerTa 
            (taFetchables [(ta1, [repo1, repo2])]) 
            (uriTaSet []) 
            @?= uriTaSet [(repo1, ta1), (repo2, ta1)]

  , HU.testCase "Handles multiple TAs and URLs" $
        updateUriPerTa 
            (taFetchables [(ta1, [repo1]),(ta2, [repo2])])
            (uriTaSet [(oldRepo, ta1),(repo3, ta3)])
            @?= uriTaSet [(repo1, ta1),(repo2, ta2),(repo3, ta3)]

  , HU.testCase "Handles TAs with no URLs in fs" $
        updateUriPerTa 
            (taFetchables [(ta1, []),(ta2, [repo2])])
            (uriTaSet [(repo1, ta1),(oldRepo, ta2),(repo3, ta3)])
            @?= uriTaSet [(repo2, ta2),(repo3, ta3)]

  , HU.testCase "Handles empty fs map" $
        updateUriPerTa 
            (taFetchables [])
            (uriTaSet [(repo1, ta1), (repo2, ta2)])
            @?= uriTaSet [(repo1, ta1), (repo2, ta2)]
  ]

loadMarksSpec :: TestTree
loadMarksSpec = testGroup "Load marks" [ 

    HU.testCase "A load sets the mark of its repository" $ do
        let (mark, generation) = loadMarkOf rsync1 newLoadMarks
        mark @?= Nothing
        markOf rsync1 (setLoadMark generation rsync1 moment1 newLoadMarks) @?= Just moment1

  , HU.testCase "The next load replaces the mark" $ do
        let generation = snd $ loadMarkOf rsync1 newLoadMarks
            marks = setLoadMark generation rsync1 moment2 
                  $ setLoadMark generation rsync1 moment1 newLoadMarks
        markOf rsync1 marks @?= Just moment2

  , HU.testCase "Every repository has its own mark" $ do
        let generation = snd $ loadMarkOf rsync1 newLoadMarks
            marks = setLoadMark generation rsync1 moment1 newLoadMarks
        markOf rsync1 marks @?= Just moment1
        markOf rsync2 marks @?= Nothing

  , HU.testCase "Cleanup drops every mark" $ do
        let generation = snd $ loadMarkOf rsync1 newLoadMarks
            marks = dropLoadMarks 
                  $ setLoadMark generation rsync2 moment2 
                  $ setLoadMark generation rsync1 moment1 newLoadMarks
        map (`markOf` marks) [rsync1, rsync2] @?= [Nothing, Nothing]

  , HU.testCase "A load that ran during cleanup doesn't set its mark" $ do
        let generation = snd $ loadMarkOf rsync1 newLoadMarks
            marks = setLoadMark generation rsync1 moment1 $ dropLoadMarks newLoadMarks
        markOf rsync1 marks @?= Nothing

  , HU.testCase "A load that started after cleanup sets its mark" $ do
        let afterCleanup = dropLoadMarks newLoadMarks
            generation   = snd $ loadMarkOf rsync1 afterCleanup
        markOf rsync1 (setLoadMark generation rsync1 moment1 afterCleanup) @?= Just moment1
  ]
  where
    markOf url = fst . loadMarkOf url
    rsync1 = RsyncURL (RsyncHost (RsyncHostName "host1.com") Nothing) [RsyncPathChunk "repo"]
    rsync2 = RsyncURL (RsyncHost (RsyncHostName "host1.com") Nothing) [RsyncPathChunk "repo", RsyncPathChunk "ca"]
    moment1 = Instant 1_000_000_000
    moment2 = Instant 2_000_000_000
