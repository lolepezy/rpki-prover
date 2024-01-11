{-# LANGUAGE OverloadedStrings   #-}

module RPKI.RRDP.UpdateSpec where

import qualified Data.Text               as Text

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Repository
import           RPKI.Reporting
import           RPKI.RRDP.Types
import           RPKI.RRDP.RrdpFetch
import           RPKI.RRDP.Http

import           Test.Tasty
import qualified Test.Tasty.HUnit        as HU

rrdpUpdateSpec :: TestTree
rrdpUpdateSpec = testGroup "Unit tests for repostory updates" [

    HU.testCase "Should generate update snapshot action" $ do
        let repo = RrdpRepository { 
                        uri = RrdpURL $ URI "http://rrdp.ripe.net/notification.xml",
                        rrdpMeta = Just (SessionId "whatever", RrdpSerial 50),
                        status = Pending,
                        fetchType = Unknown,
                        eTag = Nothing
                    }
        let (nextStep, _) = runPureValidator (newScopes "test") $ 
                                rrdpNextStep repo (makeNotification (SessionId "something else") (RrdpSerial 120))
        HU.assertEqual "It's a bummer" nextStep
                (Right $ UseSnapshot (SnapshotInfo (URI "http://bla.com/snapshot.xml") (Hash "AABB")) 
                         "Resetting RRDP session from SessionId \"whatever\" to SessionId \"something else\""),

    HU.testCase "Should generate nothing when the session id and serial are the same" $ do
        let sessionId = SessionId "something"
        let serial = RrdpSerial 13
        let repo = RrdpRepository { 
                        uri = RrdpURL $ URI "http://rrdp.ripe.net/notification.xml",
                        rrdpMeta = Just (sessionId, serial),
                        status = Pending,
                        fetchType = Unknown,
                        eTag = Nothing
                    }                        
        let (nextStep, _) = runPureValidator (newScopes "test") $ 
                                rrdpNextStep repo $ makeNotification sessionId serial
        HU.assertEqual "It's a bummer" nextStep (Right $ NothingToDo "up-to-date, SessionId \"something\", serial 13"),

    HU.testCase "Should generate delta update when the session id is the same and serial is larger" $ do
        let sessionId = SessionId "something"
        let serial = RrdpSerial 13
        let nextSerial' = nextSerial serial
        let delta = makeDelta nextSerial'

        let repo = RrdpRepository { 
                        uri = RrdpURL $ URI "http://rrdp.ripe.net/notification.xml",
                        rrdpMeta = Just (sessionId, serial),
                        status = Pending,
                        fetchType = Unknown,
                        eTag = Nothing
                    }
        let (nextStep, _) = runPureValidator (newScopes "test") $ 
                                rrdpNextStep repo $ (makeNotification sessionId nextSerial') {      
                                    deltas = [delta]
                                }
        HU.assertEqual "It's a bummer" nextStep 
            (Right $ UseDeltas [delta] (SnapshotInfo (URI "http://bla.com/snapshot.xml") (Hash "AABB")) 
            "SessionId \"something\", deltas look good."),

    HU.testCase "Should generate snapshot update when we are too far behind" $ do
        let sessionId = SessionId "something"
        let serial = RrdpSerial 13
        let repo = RrdpRepository { 
                        uri = RrdpURL $ URI "http://rrdp.ripe.net/notification.xml",
                        rrdpMeta = Just (sessionId, serial),
                        status = Pending,
                        fetchType = Unknown,
                        eTag = Nothing
                    }                    
        let (nextStep, _) = runPureValidator (newScopes "test") $ 
                                rrdpNextStep repo $ (makeNotification sessionId (RrdpSerial 15)) {       
                                  deltas = [DeltaInfo (URI "http://host/delta15.xml") (Hash "BBCC") (RrdpSerial 15)]
                                }
        HU.assertEqual "It's a bummer" nextStep (Right $ UseSnapshot 
            (SnapshotInfo (URI "http://bla.com/snapshot.xml") (Hash "AABB")) 
                         "SessionId \"something\", local serial 13 is too far behind remote 15."),

    HU.testCase "Should generate error when deltas are not consecutive" $ do
        let sessionId = SessionId "something"
        let serial = RrdpSerial 13
        let repo = RrdpRepository { 
                        uri = RrdpURL $ URI "http://rrdp.ripe.net/notification.xml",
                        rrdpMeta = Just (sessionId, serial),
                        status = Pending,
                        fetchType = Unknown,
                        eTag = Nothing
                    } 
        let (nextStep, _) = runPureValidator (newScopes "test") $ 
                    rrdpNextStep repo $ (makeNotification sessionId (RrdpSerial 20)) {       
                        deltas = [
                            makeDelta $ RrdpSerial 20,
                            makeDelta $ RrdpSerial 18,
                            makeDelta $ RrdpSerial 13
                        ]
                    }
        HU.assertEqual "It's a bummer" nextStep 
            (Right (UseSnapshot (SnapshotInfo (URI "http://bla.com/snapshot.xml") (Hash "AABB")) 
                "SessionId \"something\", there are non-consecutive delta serials: [(13,18),(18,20)]."))
  ]
    

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


    
