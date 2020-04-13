{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RPKI.RRDP.UpdateSpec where

import qualified Data.Text               as Text

import           RPKI.Domain
import           RPKI.Errors
import           RPKI.RRDP.Types
import           RPKI.RRDP.Update

import           Test.Tasty
import qualified Test.Tasty.HUnit        as HU

rrdpUpdateSpec :: TestTree
rrdpUpdateSpec = testGroup "Unit tests for repostory updates" [

    HU.testCase "Should generate update snapshot action" $ do
      let repo = RrdpRepository (URI "http://rrdp.ripe.net/notification.xml") $
                          Just (SessionId "whatever", Serial 50)
      let nextStep = rrdpNextStep repo (makeNotification (SessionId "something else") (Serial 120))
      HU.assertEqual "It's a bummer" nextStep
            (Right $ UseSnapshot $ SnapshotInfo (URI "http://bla.com/snapshot.xml") (Hash "AABB")),

    HU.testCase "Should generate nothing when the session id and serial are the same" $ do
      let sessionId = SessionId "something"
      let serial = Serial 13
      let repo = RrdpRepository (URI "http://rrdp.ripe.net/notification.xml") $ 
                 Just (sessionId, serial)
      let nextStep = rrdpNextStep repo $ makeNotification sessionId serial
      HU.assertEqual "It's a bummer" nextStep (Right NothingToDo),

    HU.testCase "Should generate delta update when the session id is the same and serial is larger" $ do
        let sessionId = SessionId "something"
        let serial = Serial 13
        let nextSerial' = nextSerial serial
        let delta = makeDelta nextSerial'
        let repo = RrdpRepository (URI "http://rrdp.ripe.net/notification.xml") $ Just (sessionId, serial)
        let nextStep = rrdpNextStep repo $ (makeNotification sessionId nextSerial') {      
            deltas = [delta]
           }
        HU.assertEqual "It's a bummer" nextStep 
            (Right $ UseDeltas [delta] (SnapshotInfo (URI "http://bla.com/snapshot.xml") (Hash "AABB"))),

    HU.testCase "Should generate snapshot update when we are too far behind" $ do
      let sessionId = SessionId "something"
      let serial = Serial 13
      let repo = RrdpRepository (URI "http://rrdp.ripe.net/notification.xml") $ Just (sessionId, serial)
      let nextStep = rrdpNextStep repo $ (makeNotification sessionId (Serial 15)) {       
          deltas = [DeltaInfo (URI "http://host/delta15.xml") (Hash "BBCC") (Serial 15)]
        }
      HU.assertEqual "It's a bummer" nextStep (Right $ UseSnapshot 
        (SnapshotInfo (URI "http://bla.com/snapshot.xml") (Hash "AABB"))),

    HU.testCase "Should generate error when deltas are not consecutive" $ do
      let sessionId = SessionId "something"
      let serial = Serial 13
      let repo = RrdpRepository (URI "http://rrdp.ripe.net/notification.xml") $ Just (sessionId, serial)
      let nextStep = rrdpNextStep repo $ (makeNotification sessionId (Serial 20)) {       
            deltas = [
              makeDelta $ Serial 20,
              makeDelta $ Serial 18,
              makeDelta $ Serial 13
            ]
          }
      HU.assertEqual "It's a bummer" nextStep (Left $ 
        NonConsecutiveDeltaSerials [(Serial 13,Serial 18),(Serial 18,Serial 20)])
  ]
    

makeNotification :: SessionId -> Serial -> Notification
makeNotification sessionId serial = Notification {
    version = Version 1,
    sessionId = sessionId,
    serial = serial,
    snapshotInfo = SnapshotInfo (URI "http://bla.com/snapshot.xml") (Hash "AABB"),
    deltas = []
  }

makeDelta :: Serial -> DeltaInfo
makeDelta serial@(Serial s) = DeltaInfo (URI uri) (Hash "AABBCC") serial
  where uri = Text.pack $ "http://somehost/delta" <> show s <> ".xml"

