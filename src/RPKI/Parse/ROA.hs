{-# LANGUAGE OverloadedStrings #-}

module RPKI.Parse.ROA where

import qualified Data.ByteString as B  

import Data.Monoid (mconcat)

import Control.Lens
import Data.Generics.Product

import Data.Word

import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.ASN1.Parse
import Data.ASN1.BitArray

import Data.Bifunctor

import RPKI.Domain  
import RPKI.IP
import RPKI.Parse.Common 
import RPKI.SignTypes 
import RPKI.Parse.SignedObject 


parseRoa :: B.ByteString -> ParseResult (URI -> [(RpkiMeta, RoaObject)])
parseRoa bs = do    
    asns <- first (fmtErr . show) $ decodeASN1' BER bs  
    f    <- first fmtErr $ runParseASN1 (parseSignedObject parseRoa') asns
    let signedRoa = f bs
    meta <- getMeta signedRoa bs
    let roaObjects = multiplySignedRoas signedRoa    
    pure $ \location -> map (\ro -> (meta location, ro)) roaObjects
  where 
    multiplySignedRoas :: SignedObject [Roa] -> [RoaObject]    
    multiplySignedRoas signedRoas = map (\roa -> 
        RoaObject (signedRoas & contentLens .~ roa)
      ) roas
      where
        roas = signedRoas ^. contentLens
        contentLens = field @"soContent" . field @"scEncapContentInfo" . field @"cContent"                 

    parseRoa' = onNextContainer Sequence $ do      
      asId <- getInteger (pure . fromInteger) "Wrong ASID"
      mconcat <$> (onNextContainer Sequence $ getMany $ 
        onNextContainer Sequence $ 
          getAddressFamily "Expected an address family here" >>= \case 
              Right Ipv4F -> getRoa asId ipv4
              Right Ipv6F -> getRoa asId ipv6
              Left af     -> throwParseError $ "Unsupported address family: " ++ show af)

    getRoa :: Int -> (B.ByteString -> Word64 -> APrefix) -> ParseASN1 [Roa]
    getRoa asId mkPrefix = onNextContainer Sequence $ getMany $
      getNextContainerMaybe Sequence >>= \case       
        Just [BitString (BitArray nzBits bs')] -> 
          pure $ mkRoa bs' nzBits (fromIntegral nzBits) 
        Just [BitString (BitArray nzBits bs'), IntVal maxLength] -> 
          pure $ mkRoa bs' nzBits (fromInteger maxLength)           
        Just a  -> throwParseError $ "Unexpected ROA content: " ++ show a
        Nothing -> throwParseError $ "Unexpected ROA content"
      where 
        mkRoa bs' nz maxL = Roa (ASN asId) (mkPrefix bs' nz) maxL

    ipv4 bs' nzBits = APrefix $ V4AF $ WithAF $ Ipv4P $ mkV4Prefix bs' (fromIntegral nzBits)
    ipv6 bs' nzBits = APrefix $ V6AF $ WithAF $ Ipv6P $ mkV6Prefix bs' (fromIntegral nzBits)


          
    
      
