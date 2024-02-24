{-# LANGUAGE OverloadedStrings   #-}

module RPKI.RRDP.UpdateSpec where

import           Control.Lens
import           Data.Generics.Product.Typed
import qualified Data.Text               as Text

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Repository
import           RPKI.Reporting
import           RPKI.RRDP.Types
import           RPKI.RRDP.RrdpFetch
import           RPKI.Util

import           Test.Tasty
import qualified Test.Tasty.HUnit        as HU

rrdpUpdateSpec :: TestTree
rrdpUpdateSpec = testGroup "Unit tests for repostory updates" [ 
    testSnapshot, 
    testNoUpdates, 
    testNoDeltaLocalTooOld, 
    testNonConsecutive, 
    testIntegrity ]

testSnapshot :: TestTree
testSnapshot = 
    HU.testCase "Should generate update snapshot action" $ do
        let repo = defaultRepo & typed .~ Just (RrdpMeta (SessionId "whatever") (RrdpSerial 50) (RrdpIntegrity []))                    
        let (nextStep, _) = runPureValidator (newScopes "test") $ 
                                rrdpNextStep repo (makeNotification (SessionId "something else") (RrdpSerial 120))
        HU.assertEqual "It's a bummer" nextStep
                (Right $ FetchSnapshot (SnapshotInfo (URI "http://bla.com/snapshot.xml") (Hash "AABB")) 
                            "Resetting RRDP session from SessionId \"whatever\" to SessionId \"something else\"")

testNoUpdates :: TestTree
testNoUpdates = 
    HU.testCase "Should generate nothing when the session id and serial are the same" $ do
        let sessionId = SessionId "something"
        let serial = RrdpSerial 13
        let repo = defaultRepo & typed .~ Just (RrdpMeta sessionId serial (RrdpIntegrity []))                      
        let (nextStep, _) = runPureValidator (newScopes "test") $ 
                                rrdpNextStep repo $ makeNotification sessionId serial
        HU.assertEqual "It's a bummer" nextStep (Right $ NothingToFetch "up-to-date, SessionId \"something\", serial 13")

testDeltaUpdate :: TestTree
testDeltaUpdate = 
    HU.testCase "Should generate delta update when the session id is the same and serial is larger" $ do
        let sessionId = SessionId "something"
        let serial = RrdpSerial 13
        let nextSerial' = nextSerial serial
        let delta = makeDelta nextSerial'

        let repo = defaultRepo & typed .~ Just (RrdpMeta sessionId serial (RrdpIntegrity []))
        let (nextStep, _) = runPureValidator (newScopes "test") $ 
                                rrdpNextStep repo $ (makeNotification sessionId nextSerial') {      
                                    deltas = [delta]
                                }
        HU.assertEqual "It's a bummer" nextStep 
            (Right $ FetchDeltas [delta] (SnapshotInfo (URI "http://bla.com/snapshot.xml") (Hash "AABB")) 
            "SessionId \"something\", deltas look good.")

testNoDeltaLocalTooOld :: TestTree
testNoDeltaLocalTooOld = 
    HU.testCase "Should generate snapshot update when we are too far behind" $ do
        let sessionId = SessionId "something"
        let serial = RrdpSerial 13
        let repo = defaultRepo & typed .~ Just (RrdpMeta sessionId serial (RrdpIntegrity []))                  
        let (nextStep, _) = runPureValidator (newScopes "test") $ 
                                rrdpNextStep repo $ (makeNotification sessionId (RrdpSerial 15)) {       
                                  deltas = [DeltaInfo (URI "http://host/delta15.xml") (Hash "BBCC") (RrdpSerial 15)]
                                }
        HU.assertEqual "It's a bummer" nextStep (Right $ FetchSnapshot 
            (SnapshotInfo (URI "http://bla.com/snapshot.xml") (Hash "AABB")) 
                         "SessionId \"something\", local serial 13 is too far behind remote 15.")

testNonConsecutive :: TestTree
testNonConsecutive = 
    HU.testCase "Should generate error when deltas are not consecutive" $ do
        let sessionId = SessionId "something"
        let serial = RrdpSerial 13
        let repo = RrdpRepository { 
                        uri = RrdpURL $ URI "http://rrdp.ripe.net/notification.xml",
                        rrdpMeta = Just $ RrdpMeta sessionId serial (RrdpIntegrity []),
                        meta = RepositoryMeta {
                                status = Pending,
                                fetchType = Unknown,
                                lastFetchDuration = Nothing,
                                refreshInterval = Nothing
                            },
                        eTag = Nothing
                    } 

        let repo = defaultRepo & typed .~ Just (RrdpMeta sessionId serial (RrdpIntegrity []))

        let (nextStep, _) = runPureValidator (newScopes "test") $ 
                    rrdpNextStep repo $ (makeNotification sessionId (RrdpSerial 20)) {       
                        deltas = [
                            makeDelta $ RrdpSerial 20,
                            makeDelta $ RrdpSerial 18,
                            makeDelta $ RrdpSerial 13
                        ]
                    }
        HU.assertEqual "It's a bummer" nextStep 
            (Right (FetchSnapshot (SnapshotInfo (URI "http://bla.com/snapshot.xml") (Hash "AABB")) 
                "SessionId \"something\", there are non-consecutive delta serials: [(13,18),(18,20)]."))
    
testIntegrity :: TestTree
testIntegrity = 
    HU.testCase "Should fall-back to snapshot in case of delta mutations" $ do
        let deltaUrl serial_ = URI $ "http://rrdp.ripe.net/delta" <> fmtGen serial_  <> ".xml"
        let sessionId = SessionId "something"
        let serial11 = RrdpSerial 11        
        let serial12 = RrdpSerial 12        
        let serial13 = RrdpSerial 13        
        let serial14 = RrdpSerial 14        
        let previousDeltas = [ 
                    DeltaInfo (deltaUrl serial11) (Hash "hash11") serial11,
                    DeltaInfo (deltaUrl serial12) (Hash "hash12") serial12 
                ]

        let snapshotInfo = SnapshotInfo (URI "http://bla.com/snapshot.xml") (Hash "AABB")

        let repo = defaultRepo & typed .~ Just (RrdpMeta sessionId serial13 (RrdpIntegrity previousDeltas))

        let (nextStep, _) = runPureValidator (newScopes "test") $ 
                    rrdpNextStep repo $ (makeNotification sessionId serial14) {       
                        deltas = [
                            DeltaInfo (deltaUrl serial12) (Hash "hash12") serial12,
                            DeltaInfo (deltaUrl serial13) (Hash "hash13") serial13,
                            DeltaInfo (deltaUrl serial14) (Hash "hash14") serial14
                        ]
                    }
        HU.assertEqual "It's a bummer" nextStep 
            (Right $ FetchDeltas {
                message = "SessionId \"something\", deltas look good.",
                snapshotInfo = snapshotInfo,
                sortedDeltas = [DeltaInfo (URI "http://rrdp.ripe.net/delta14.xml") (Hash "hash14") serial14] 
            })
        
        let (nextStep1, _) = runPureValidator (newScopes "test") $ 
                    rrdpNextStep repo $ (makeNotification sessionId serial14) {       
                        deltas = [
                            DeltaInfo (deltaUrl serial12) (Hash "hash12-broken") serial12,
                            DeltaInfo (deltaUrl serial13) (Hash "hash13") serial13,
                            DeltaInfo (deltaUrl serial14) (Hash "hash14") serial14
                        ]
                    }
        HU.assertEqual "It's a bummer" nextStep1 
            (Right $ FetchSnapshot snapshotInfo 
                "These deltas have integrity issues: serial 12, used to have hash 686173683132 and now 6861736831322d62726f6b656e.")


defaultRepo = RrdpRepository { 
    uri = RrdpURL $ URI "http://rrdp.ripe.net/notification.xml",
    rrdpMeta = Nothing,
    meta = RepositoryMeta {
            status = Pending,
            fetchType = Unknown,
            lastFetchDuration = Nothing,
            refreshInterval = Nothing
        },
    eTag = Nothing
} 

makeNotification :: SessionId -> RrdpSerial -> Notification
makeNotification sessionId' serial' = Notification {
    version = Version 1,
    sessionId = sessionId',
    serial = serial',
    snapshotInfo = SnapshotInfo (URI "http://bla.com/snapshot.xml") (Hash "AABB"),
    deltas = []
  }

makeDelta :: RrdpSerial -> DeltaInfo
makeDelta serial'@(RrdpSerial s) = DeltaInfo (URI u) (Hash "AABBCC") serial'
  where u = Text.pack $ "http://somehost/delta" <> show s <> ".xml"


    
