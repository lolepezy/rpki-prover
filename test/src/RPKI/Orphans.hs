{-# LANGUAGE OverloadedStrings #-}

module RPKI.Orphans where

import Control.Monad

import qualified Data.ByteString                      as B
import qualified Data.ByteString.Lazy                 as BL

import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances.ByteString
import Test.QuickCheck.Instances.Text
import Test.QuickCheck.Monadic

import RPKI.Domain
import RPKI.RRDP.Parse
import RPKI.RRDP.Types

import RPKI.Parse.Cert
import RPKI.Parse.MFT
import RPKI.Parse.ROA
import RPKI.Store

import RPKI.Util (convert)

instance Arbitrary URI where
  arbitrary = URI <$> do
    ext  <- elements [ ".cer", ".mft", ".roa", ".crl", ".gbr" ]
    name <- listOf1 $ elements ['a'..'z']
    pure $ convert $ "rsync://" <> name <> ext
  shrink = genericShrink

instance Arbitrary Hash where
  arbitrary = Hash <$> suchThat arbitrary (\b -> B.length b > 1)
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

  
