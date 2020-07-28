{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}

module RPKI.RRDP.ParseSpec where

import           Data.ByteString.Short             as BSS
import           Data.Hex                          (hex)

import           RPKI.Domain
import           RPKI.RRDP.Parse
import           RPKI.RRDP.Types

import           Test.QuickCheck.Arbitrary.Generic
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck             as QC

import           Data.String.Interpolate

import           RPKI.Util                         (convert)

import           RPKI.Orphans


rrdpXmlLazyParsingGroup :: TestTree
rrdpXmlLazyParsingGroup = testGroup "RRDP Hexpat parsing"
  [
    QC.testProperty
      "Generate and parse back a snapshot"
      prop_generate_and_parse_snapshot_creates_same_object,

    QC.testProperty
      "Generate and parse back a delta"
      prop_generate_and_parse_delta_creates_same_object,

    QC.testProperty
      "Generate and parse back a notification file"      
      prop_generate_and_parse_notification_creates_same_object
  ]

prop_generate_and_parse_snapshot_creates_same_object :: QC.Property
prop_generate_and_parse_snapshot_creates_same_object = monadicIO $ do
  snapshot :: Snapshot <- pick arbitrary
  let xml = snaphostToXml snapshot
  let s = parseSnapshot (convert xml)
  assert $ Right snapshot == s  

prop_generate_and_parse_delta_creates_same_object :: QC.Property
prop_generate_and_parse_delta_creates_same_object = monadicIO $ do
  delta :: Delta <- pick arbitrary
  let xml = deltaToXml delta
  let d = parseDelta (convert xml)
  assert $ Right delta == d

prop_generate_and_parse_notification_creates_same_object :: QC.Property
prop_generate_and_parse_notification_creates_same_object = monadicIO $ do
  notification :: Notification <- pick arbitrary  
  let xml = notificationToXml notification
  let n = parseNotification (convert xml)
  assert $ Right notification == n

snaphostToXml :: Snapshot -> String
snaphostToXml (Snapshot (Version v) (SessionId sid) (Serial s) publishes) =
  [i|<snapshot version="#{v}" session_id="#{sid}" serial="#{s}">
         #{concatMap pub publishes}
  </snapshot>|]
  where
    pub (SnapshotPublish (URI u) (EncodedBase64 c)) =
      [i|<publish uri="#{u}">#{c}</publish>|]

deltaToXml :: Delta -> String
deltaToXml (Delta (Version v) (SessionId sess) (Serial s) items) =
  [i|<delta version="#{v}" session_id="#{sess}" serial="#{s}">
        #{concatMap item items}
  </delta>|]
  where
    item (DP (DeltaPublish (URI u) Nothing (EncodedBase64 c))) =
      [i|<publish uri="#{u}">#{c}</publish>|]
    item (DP (DeltaPublish (URI u) (Just (Hash hash)) (EncodedBase64 c))) =
      [i|<publish uri="#{u}" hash="#{hex $ BSS.fromShort hash}">#{c}</publish>|]
    item (DW (DeltaWithdraw (URI u) (Hash hash))) =
      [i|<withdraw uri="#{u}" hash="#{hex $ BSS.fromShort hash}"></withdraw>|]


notificationToXml :: Notification -> String
notificationToXml Notification { 
      sessionId = SessionId sid,
      serial = Serial s,
      snapshotInfo = SnapshotInfo (URI su) (Hash sh),
      version = Version v,
      ..
    } =
  [i|<notification version="#{v}" session_id="#{sid}" serial="#{s}">
      <snapshot uri="#{su}" hash="#{hex $ BSS.fromShort sh}"></snapshot>
      #{concatMap delta deltas}
  </notification>|]
  where
    delta (DeltaInfo (URI u) (Hash hash) (Serial ds)) =
      [i|<delta uri="#{u}" hash="#{hex $ BSS.fromShort hash}" serial="#{ds}"></delta>|]  

