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


parseMft :: B.ByteString -> ParseResult (SignedObject MFT)
parseMft bs = do
  case decodeASN1' BER bs of
    Left e     -> (Left . fmtErr . show) e
    Right asns -> mapParseErr $ runParseASN1 (parseSignedObject parseManifest) asns  
  where 
    parseManifest = onNextContainer Sequence $ do
      manifestNumber <- getInteger (pure . fromInteger) "Wrong manifest number"
      fileHashAlg    <- getOID (pure . hashAlg) "Wrong OID for hash algorithm"
      MFT manifestNumber fileHashAlg 
          <$> getTime "No ThisUpdate time"
          <*> getTime "No NextUpdate time"
          <*> (onNextContainer Sequence $ 
                getMany $ onNextContainer Sequence $ 
                   (,) <$> getIA5String (pure . T.pack) "Wrong file name"
                       <*> getBitString (pure . MFTRef . Left . (Hash fileHashAlg)) "Wrong hash"
              )

    getTime message = getNext >>= \case 
      ASN1Time TimeGeneralized dt tz -> pure dt
      s  -> throwParseError $ message ++ ", got " ++ show s
    
      
