{-# LANGUAGE OverloadedStrings   #-}

module RPKI.Parse.MFT where

import qualified Data.ByteString          as B
import qualified Data.Text                as T

import           Data.ASN1.Types
import Data.Bifunctor (first)
import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Parse

import           RPKI.Domain
import           RPKI.Parse.Common
import           RPKI.Parse.SignedObject


parseMft :: B.ByteString -> ParseResult (URI -> (RpkiMeta, MftObject))
parseMft bs = do
  asns <- first (fmtErr . show) $ decodeASN1' BER bs
  f    <- first fmtErr $ runParseASN1 (parseSignedObject parseManifest) asns
  let so = f bs
  meta <- getMeta so bs
  pure $ \location -> (meta location, MftObject so)
  where    
    parseManifest :: ParseASN1 Manifest
    parseManifest = onNextContainer Sequence $
      (,,) <$> getNext <*> getNext <*> getNext >>= \case
        (IntVal manifestNumber,
         ASN1Time TimeGeneralized thisUpdateTime _,
         ASN1Time TimeGeneralized nextUpdateTime _) -> do
          fileHashAlg <- getOID (pure . hashAlg) "Wrong hash algorithm OID"
          entries     <- getEntries fileHashAlg
          -- TODO translate to UTC
          pure $ Manifest (fromInteger manifestNumber) fileHashAlg thisUpdateTime nextUpdateTime entries

        -- TODO Check version?
        (IntVal version,
         IntVal manifestNumber,
         ASN1Time TimeGeneralized thisUpdateTime tz) -> do
          nextUpdateTime <- getTime "No NextUpdate time"
          fileHashAlg    <- getOID (pure . hashAlg) "Wrong hash algorithm OID"
          entries        <- getEntries fileHashAlg
          -- TODO translate to UTC
          pure $ Manifest (fromInteger manifestNumber) fileHashAlg thisUpdateTime nextUpdateTime entries

        s -> throwParseError $ "Unexpected ROA content: " ++ show s

    getEntries fileHashAlg = onNextContainer Sequence $
      getMany $ onNextContainer Sequence $
        (,) <$> getIA5String (pure . T.pack) "Wrong file name"
            <*> getBitString (pure . (Hash fileHashAlg)) "Wrong hash"

    getTime message = getNext >>= \case
      ASN1Time TimeGeneralized dt tz -> pure dt
      s  -> throwParseError $ message ++ ", got " ++ show s


