{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE KindSignatures #-}

module RPKI.Parse.ROA where

import qualified Data.ByteString as B  
import qualified Data.Text as T 

import Data.Word

import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.ASN1.Parse
import Data.ASN1.BitArray

import RPKI.Domain  
import RPKI.IP
import RPKI.Parse.Common 
import RPKI.Parse.SignedObject 


parseRoa :: B.ByteString -> ParseResult (SignedObject [ROA])
parseRoa bs = 
  case decodeASN1' BER bs of
    Left e     -> (Left . fmtErr . show) e
    Right asns -> mapParseErr $ runParseASN1 (parseSignedObject parseRoa') asns  
  where 
    parseRoa' = onNextContainer Sequence $ do
      asId <- getInteger (pure . fromInteger) "Wrong ASID"
      onNextContainer Sequence $ getMany $ 
        onNextContainer Sequence $ 
          getAddressFamily "Expected an address family here" >>= \case 
              Right Ipv4F -> getRoa asId ipv4
              Right Ipv6F -> getRoa asId ipv6
              Left af     -> throwParseError $ "Unsupported address family"

    getRoa :: Int -> (B.ByteString -> Word64 -> APrefix) -> ParseASN1 ROA
    getRoa asId mkPrefix = getNextContainerMaybe Sequence >>= \case       
      Just [BitString (BitArray nonZeroBits bs)] -> 
        pure $ ROA 
          (ASN asId) 
          (mkPrefix bs nonZeroBits) 
          (fromIntegral nonZeroBits)
      Just [BitString (BitArray nonZeroBits bs), IntVal maxLength] -> 
        pure $ ROA 
          (ASN asId) 
          (mkPrefix bs nonZeroBits) 
          (fromInteger maxLength)
      Nothing -> throwParseError $ "Unexpected ROA content"

    ipv4 bs nonZeroBits = AV4 $ Ipv4P $ mkV4Prefix bs (fromIntegral nonZeroBits)
    ipv6 bs nonZeroBits = AV6 $ Ipv6P $ mkV6Prefix bs (fromIntegral nonZeroBits)


          
    
      
