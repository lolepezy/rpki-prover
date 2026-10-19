module RPKI.FetchSpec where

import           Data.Text (Text)

import           Test.Tasty
import           Test.Tasty.HUnit                as HU

import qualified Data.ByteString                 as BS
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.Map.Strict                 as Map
import qualified Data.Map.Monoidal.Strict        as MonoidalMap
import qualified Data.IxSet.Typed                as IxSet
import qualified Data.Set                        as Set

import           RPKI.Domain
import           RPKI.Repository
import           RPKI.Util                       (parseRpkiURL)
import           RPKI.Fetch.Fetch


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
        primaryFallbacks = [ (RrdpU (RrdpURL (URI primary)), MonoidalMap.singleton (rootGroup (TaName ta)) mempty) 
                           | primary <- urls ]

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

fetchGroupSpec :: TestTree
fetchGroupSpec = testGroup "Fetch groups" [

    HU.testCase "A group is inside all the groups on its path" $ do
        groupPath (rootGroup ta) @?= []
        groupPath (group_ [1, 2]) @?= [group_ [1], group_ [1, 2]]
        innerGroup (group_ [1]) (ski 2) @?= group_ [1, 2]

  , HU.testCase "A repository is fetched for the group closest to the TA" $ do
        ownerGroup [group_ [1, 2], group_ [3], group_ [1]] @?= Just (group_ [1])
        ownerGroup [group_ [2], rootGroup ta] @?= Just (rootGroup ta)
        ownerGroup [] @?= Nothing

  , HU.testCase "An rsync repository is told by its host, an RRDP one by its URL" $ do
        let ids = repositoryIdentities . PublicationPointAccess
        ids (rrdpPP' "https://a.net/n.xml" :| [rsyncPP' "rsync://a.net/repo/x/"])
            @?= ids (rrdpPP' "https://a.net/n.xml" :| [rsyncPP' "rsync://a.net/other/y/"])
        HU.assertBool "A different notification URL on the same host" $
            ids (rrdpPP' "https://a.net/n.xml" :| []) /= ids (rrdpPP' "https://a.net/m.xml" :| [])

  , HU.testCase "Fetcheables keep the fallbacks of every group" $ do
        let primary   = rrdp "https://a.net/n.xml"
            fallback1 = rsync "rsync://a.net/repo/"
            fallback2 = rsync "rsync://evil.net/repo/"
            fs = newFetcheables (rootGroup ta) primary (Just fallback1) 
                    <> newFetcheables (group_ [1]) primary (Just fallback2)
                    <> newFetcheables (rootGroup ta) primary Nothing
        unFetcheables fs @?= MonoidalMap.singleton primary (MonoidalMap.fromList 
                                [ (rootGroup ta, Set.singleton fallback1), 
                                  (group_ [1], Set.singleton fallback2) ])
        fetcheableUrls fs @?= Set.fromList [primary, fallback1, fallback2]
        repositoryCount fs @?= 1
  ]
  where
    ta = TaName "ta"
    ski n = SKI $ mkKI $ BS.pack [n]
    group_ ns = FetchGroup ta (map ski ns)
    rrdp = RrdpU . RrdpURL . URI
    rsync u = either (error . show) id $ parseRpkiURL u
    rrdpPP' = rrdpPP . RrdpURL . URI
    rsyncPP' u = case parseRpkiURL u of 
                    Right (RsyncU r) -> rsyncPP r
                    other            -> error $ show other
