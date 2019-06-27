{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RPKI.RRDP.UpdateSpec where

import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL

import           Control.Lens
import           Data.Generics.Product
import           Data.Generics.Sum
import           Data.Maybe              (maybeToList)

import qualified Data.Text               as T

import           Control.Monad

import qualified Data.ByteString.Base64  as B64

import           RPKI.Domain
import           RPKI.RRDP.Parse
import           RPKI.RRDP.Types
import           RPKI.RRDP.Update

import           Test.Tasty
import qualified Test.Tasty.HUnit        as HU

import           Data.String.Interpolate
import           RPKI.Util               (convert)


rrdpUpdateSpec = testGroup "Unit tests for repostory updates" [

    HU.testCase "Should generate update snapshot action" $ do
      let repo = RrdpRepo (URI "http://rrdp.ripe.net/notification.xml") (SessionId "whatever") (Serial 50)
      let nextStep = rrdpNextStep repo (makeNotification (SessionId "something else") (Serial 120))
      HU.assertEqual "It's a bummer" nextStep
            (Right $ UseSnapshot $ SnapshotInfo (URI "http://bla.com/snapshot.xml") (Hash SHA256 "AABB")),

    HU.testCase "Should generate nothing when the session id and serial are the same" $ do
      let sessionId = SessionId "something"
      let serial = Serial 13
      let repo = RrdpRepo (URI "http://rrdp.ripe.net/notification.xml") sessionId serial
      let nextStep = rrdpNextStep repo $ makeNotification sessionId serial
      HU.assertEqual "It's a bummer" nextStep (Right NothingToDo),

    HU.testCase "Should generate delta update when the session id is the same and serial is larger" $ do
        let sessionId = SessionId "something"
        let serial = Serial 13
        let nextSerial = next serial
        let delta = makeDelta nextSerial
        let repo = RrdpRepo (URI "http://rrdp.ripe.net/notification.xml") sessionId serial
        let nextStep = rrdpNextStep repo $ (makeNotification sessionId nextSerial) {      
            deltas = [delta]
           }
        HU.assertEqual "It's a bummer" nextStep (Right $ UseDeltas [delta]),

    HU.testCase "Should generate snapshot update when we are too far behind" $ do
      let sessionId = SessionId "something"
      let serial = Serial 13
      let repo = RrdpRepo (URI "http://rrdp.ripe.net/notification.xml") sessionId serial
      let nextStep = rrdpNextStep repo $ (makeNotification sessionId (Serial 15)) {       
          deltas = [DeltaInfo (URI "http://host/delta15.xml") (Hash SHA256 "BBCC") (Serial 15)]
        }
      HU.assertEqual "It's a bummer" nextStep (Right $ UseSnapshot 
        (SnapshotInfo (URI "http://bla.com/snapshot.xml") (Hash SHA256 "AABB"))),

    HU.testCase "Should generate error when deltas are not consecutive" $ do
      let sessionId = SessionId "something"
      let serial = Serial 13
      let repo = RrdpRepo (URI "http://rrdp.ripe.net/notification.xml") sessionId serial
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
    snapshotInfo = SnapshotInfo (URI "http://bla.com/snapshot.xml") (Hash SHA256 "AABB"),
    deltas = []
  }

makeDelta :: Serial -> DeltaInfo
makeDelta serial@(Serial s) = DeltaInfo (URI uri) (Hash SHA256 "AABBCC") serial
  where uri = T.pack $ "http://somehost/delta" ++ show s ++ ".xml"

