{-# LANGUAGE OverloadedStrings #-}

module RPKI.Parse.Internal.ROA where

import qualified Data.ByteString as BS  

import Data.Monoid (mconcat)

import Data.Word
import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.ASN1.Parse
import Data.ASN1.BitArray

import Data.Bifunctor

import RPKI.Domain 
import RPKI.Resources.Types
import RPKI.Parse.Internal.Common
import RPKI.Parse.Internal.SignedObject 

-- | Parse ROA, https://tools.ietf.org/html/rfc6482
-- 
parseRoa :: BS.ByteString -> ParseResult (RpkiURL -> RoaObject)
parseRoa bs = do    
    asns      <- first (fmtErr . show) $ decodeASN1' BER bs  
    signedRoa <- first fmtErr $ runParseASN1 (parseSignedObject $ parseSignedContent parseRoas') asns
    meta <- getMetaFromSigned signedRoa bs
    pure $ \url -> let (hash, loc) = meta url in newCMSObject hash loc (CMS signedRoa)
    where     
        parseRoas' = onNextContainer Sequence $ do      
            -- TODO Fix it so that it would work with present attestation version
            asId <- getInteger (pure . fromInteger) "Wrong ASID"
            mconcat <$> onNextContainer Sequence (getMany $
                onNextContainer Sequence $ 
                getAddressFamily "Expected an address family here" >>= \case 
                    Right Ipv4F -> getRoa asId $ \bs' nzBits -> Ipv4P $ make bs' (fromIntegral nzBits)
                    Right Ipv6F -> getRoa asId $ \bs' nzBits -> Ipv6P $ make bs' (fromIntegral nzBits)                
                    Left af     -> throwParseError $ "Unsupported address family: " ++ show af)

        getRoa :: Int -> (BS.ByteString -> Word64 -> IpPrefix) -> ParseASN1 [Vrp]
        getRoa asId mkPrefix = onNextContainer Sequence $ getMany $
            getNextContainerMaybe Sequence >>= \case       
                Just [BitString (BitArray nzBits bs')] -> 
                    pure $ mkRoa bs' nzBits (PrefixLength $ fromIntegral nzBits) 
                Just [BitString (BitArray nzBits bs'), IntVal maxLength] -> 
                    pure $ mkRoa bs' nzBits (PrefixLength $ fromInteger maxLength)           
                Just a  -> throwParseError $ "Unexpected ROA content: " <> show a
                Nothing -> throwParseError "Unexpected ROA content"
            where 
                mkRoa bs' nz = Vrp (ASN (fromIntegral asId)) (mkPrefix bs' nz)
