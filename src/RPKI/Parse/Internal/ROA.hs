{-# LANGUAGE OverloadedStrings #-}

module RPKI.Parse.Internal.ROA where

import qualified Data.ByteString as B  

import Data.Monoid (mconcat)

import Data.Word
import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.ASN1.Parse
import Data.ASN1.BitArray

import Data.Bifunctor

import RPKI.Domain  
import RPKI.Resource.Resource
import RPKI.Parse.Internal.Common
import RPKI.Parse.Internal.SignedObject 


parseRoa :: B.ByteString -> ParseResult (URI -> RoaObject)
parseRoa bs = do    
    asns      <- first (fmtErr . show) $ decodeASN1' BER bs  
    signedRoa <- first fmtErr $ runParseASN1 (parseSignedObject parseRoas') asns
    identityMeta <- getMetaFromSigned signedRoa bs
    pure $ \location -> With (identityMeta location) (CMS signedRoa)
  where     
    parseRoas' = onNextContainer Sequence $ do      
      -- TODO Fix it so that it would work with present attestation version
      asId <- getInteger (pure . fromInteger) "Wrong ASID"
      mconcat <$> onNextContainer Sequence (getMany $
        onNextContainer Sequence $ 
          getAddressFamily "Expected an address family here" >>= \case 
              Right Ipv4F -> getRoa asId ipv4
              Right Ipv6F -> getRoa asId ipv6
              Left af     -> throwParseError $ "Unsupported address family: " ++ show af)

    getRoa :: Int -> (B.ByteString -> Word64 -> IpPrefix) -> ParseASN1 [Roa]
    getRoa asId mkPrefix = onNextContainer Sequence $ getMany $
      getNextContainerMaybe Sequence >>= \case       
        Just [BitString (BitArray nzBits bs')] -> 
          pure $ mkRoa bs' nzBits (fromIntegral nzBits) 
        Just [BitString (BitArray nzBits bs'), IntVal maxLength] -> 
          pure $ mkRoa bs' nzBits (fromInteger maxLength)           
        Just a  -> throwParseError $ "Unexpected ROA content: " <> show a
        Nothing -> throwParseError "Unexpected ROA content"
      where 
        mkRoa bs' nz = Roa (ASN asId) (mkPrefix bs' nz)

    ipv4 bs' nzBits = Ipv4P $ make bs' (fromIntegral nzBits)
    ipv6 bs' nzBits = Ipv6P $ make bs' (fromIntegral nzBits)
