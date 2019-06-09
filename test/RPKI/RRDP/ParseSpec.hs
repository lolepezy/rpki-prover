{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RPKI.RRDP.ParseSpec where

import qualified Data.ByteString                      as B
import qualified Data.ByteString.Lazy                 as BL

import qualified Data.Text                            as T

import           Control.Monad
import           Control.Monad.ST
import           Data.Hex                             (hex, unhex)
import           Data.STRef
import           Data.Word

import           Control.DeepSeq
import           Control.Lens                         ((^.))

import           Control.Monad.Identity
import           Control.Monad.Primitive              (PrimMonad (..), ioToPrim,
                                                       stToPrim)
import           Control.Monad.Trans.Class

import qualified Data.ByteString.Base64               as B64

import           RPKI.Domain
import           RPKI.RRDP.Parse
import           RPKI.RRDP.Types

import           Test.QuickCheck.Arbitrary.Generic
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Instances.ByteString
import           Test.QuickCheck.Instances.Text
import           Test.QuickCheck.Monadic
import           Test.Tasty
import qualified Test.Tasty.HUnit                     as HU
import           Test.Tasty.QuickCheck                as QC

import           Data.String.Interpolate

import           RPKI.Util                            (convert)

testParseSnapshot = do
  snapshot <- BL.readFile "./snapshot.xml"
  let x = parseSnapshot snapshot
  print $ x `deepseq` 1
  print $ x `deepseq` 2
  -- print $ length x
  -- runIdentityT $ parseXml (B.toStrict snapshot)
  --     (\x -> lift $ ioToPrim $ print ("e = " ++ show x))
  --     (\t -> lift $ ioToPrim $ print ("t = " ++ show t))

testParseDelta = do
  delta <- BL.readFile "./delta.xml"
  let x = parseDelta delta
  print $ x `deepseq` x
  runIdentityT $ parseXml (BL.toStrict delta)
      (\x -> lift $ ioToPrim $ print ("e = " ++ show x))
      (\t -> lift $ ioToPrim $ print ("t = " ++ show t))


rrdpXmlParsingGroup = testGroup "RRDP parsing"
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
  assert $ (Right snapshot) == s

prop_generate_and_parse_delta_creates_same_object :: QC.Property
prop_generate_and_parse_delta_creates_same_object = monadicIO $ do
  delta :: Delta <- pick arbitrary
  let xml = deltaToXml delta
  let d = parseDelta (convert xml)
  assert $ (Right delta) == d

prop_generate_and_parse_notification_creates_same_object :: QC.Property
prop_generate_and_parse_notification_creates_same_object = monadicIO $ do
  notification :: Notification <- pick arbitrary  
  let xml = notificationToXml notification
  let n = parseNotification (convert xml)
  assert $ (Right notification) == n

snaphostToXml :: Snapshot -> String
snaphostToXml (Snapshot (Version v) (SessionId sid) (Serial s) publishes) =
  [i|<snapshot version="#{v}" session_id="#{sid}" serial="#{s}">
         #{concatMap pub publishes}
  </snapshot>|]
  where
    pub (SnapshotPublish (URI u) (Content c)) =
      [i|<publish uri="#{u}">#{B64.encode c}</publish>|]

deltaToXml :: Delta -> String
deltaToXml (Delta (Version v) (SessionId sess) (Serial s) items) =
  [i|<delta version="#{v}" session_id="#{sess}" serial="#{s}">
        #{concatMap item items}
  </delta>|]
  where
    item (DP (DeltaPublish (URI u) Nothing (Content c))) =
      [i|<publish uri="#{u}">#{B64.encode c}</publish>|]
    item (DP (DeltaPublish (URI u) (Just (Hash _ hash)) (Content c))) =
      [i|<publish uri="#{u}" hash="#{hex hash}">#{B64.encode c}</publish>|]
    item (DW (DeltaWithdraw (URI u) (Hash _ hash))) =
      [i|<withdraw uri="#{u}" hash="#{hex hash}"></withdraw>|]


notificationToXml :: Notification -> String
notificationToXml Notification { 
      sessionId = SessionId sid,
      serial = Serial s,
      snapshotInfo = SnapshotInfo (URI su) (Hash _ sh),
      version = Version v,
      ..
    } =
  [i|<notification version="#{v}" session_id="#{sid}" serial="#{s}">
      <snapshot uri="#{su}" hash="#{hex sh}"></snaphost>
      #{concatMap delta deltas}
  </notification>|]
  where
    delta (DeltaInfo (URI u) (Hash _ hash) (Serial s)) =
      [i|<delta uri="#{u}" hash="#{hex hash}" serial="#{s}"></delta>|]  

instance Arbitrary URI where
  arbitrary = URI <$> do
    ext  <- elements [ ".cer", ".mft", ".roa" ]
    name <- listOf1 $ elements ['a'..'z']
    pure $ convert $ "rsync://" <> name <> ext
  shrink = genericShrink

instance Arbitrary Hash where
  arbitrary = Hash SHA256 <$> suchThat arbitrary (\b -> B.length b > 1)
  shrink = genericShrink

instance Arbitrary HashAlg where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Serial where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary SessionId where
  arbitrary = SessionId . convert <$>
    (listOf1 $ elements $ ['a'..'z'] ++ ['0'..'9'])
  shrink = genericShrink

instance Arbitrary Version where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary DeltaPublish where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary DeltaWithdraw where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary DeltaItem where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Content where
  arbitrary = Content <$> suchThat arbitrary (\b -> B.length b > 1)
  shrink = genericShrink

instance Arbitrary SnapshotInfo where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary SnapshotPublish where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary DeltaInfo where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Snapshot where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Delta where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Notification where
  arbitrary = genericArbitrary
  shrink = genericShrink

