{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE KindSignatures #-}

module RPKI.Parse.MFT where

import qualified Data.ByteString as B  
import qualified Data.Text as T 

import Data.ASN1.Types

import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.ASN1.Parse

import RPKI.Domain  
import RPKI.Parse.Common 
import RPKI.Parse.SignedObject 


parseMft :: B.ByteString -> ParseResult (SignedObject Manifest)
parseMft bs = do
  case decodeASN1' BER bs of
    Left e     -> (Left . fmtErr . show) e
    Right asns -> mapParseErr $ runParseASN1 (parseSignedObject parseManifest) asns  
  where 
    parseManifest :: ParseASN1 Manifest
    parseManifest = onNextContainer Sequence $ do
      (,,) <$> getNext <*> getNext <*> getNext >>= \case           
        (IntVal manifestNumber, 
         ASN1Time TimeGeneralized thisUpdateTime tz1, 
         ASN1Time TimeGeneralized nextUpdateTime tz2) -> do
          fileHashAlg <- getOID (pure . hashAlg) "Wrong hash algorithm OID"
          entries     <- getEntries fileHashAlg
          pure $ Manifest (fromInteger manifestNumber) fileHashAlg thisUpdateTime nextUpdateTime entries

        (IntVal version, 
         IntVal manifestNumber, 
         ASN1Time TimeGeneralized thisUpdateTime tz) -> do
          nextUpdateTime <- getTime "No NextUpdate time"
          fileHashAlg    <- getOID (pure . hashAlg) "Wrong hash algorithm OID"
          entries        <- getEntries fileHashAlg
          pure $ Manifest (fromInteger manifestNumber) fileHashAlg thisUpdateTime nextUpdateTime entries

        s -> throwParseError $ "Unexpected ROA content: " ++ show s
    
    getEntries fileHashAlg = onNextContainer Sequence $ 
      getMany $ onNextContainer Sequence $ 
        (,) <$> getIA5String (pure . T.pack) "Wrong file name"
            <*> getBitString (pure . RefHash . (Hash fileHashAlg)) "Wrong hash"        
        
    getTime message = getNext >>= \case 
      ASN1Time TimeGeneralized dt tz -> pure dt
      s  -> throwParseError $ message ++ ", got " ++ show s
    
      
