{-# LANGUAGE OverloadedStrings #-}

module RPKI.Parse.Internal.ROA where

import qualified Data.ByteString as BS  

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
parseRoa :: BS.ByteString -> ParseResult RoaObject
parseRoa bs = do    
    asns      <- first (fmtErr . show) $ decodeASN1' BER bs  
    signedRoa <- first fmtErr $ runParseASN1 (parseSignedObject $ parseSignedContent parseRoas') asns
    hash' <- getMetaFromSigned signedRoa bs
    pure $ newCMSObject hash' (CMS signedRoa)
    where     
        parseRoas' = onNextContainer Sequence $ do      
            -- TODO Fix it so that it would work with present attestation version
            asId <- getInteger (pure . fromInteger) "Wrong ASID"
            mconcat <$> onNextContainer Sequence (getMany $
                onNextContainer Sequence $ 
                getAddressFamily "Expected an address family here" >>= \case 
                    Right Ipv4F -> getRoa asId Ipv4F
                    Right Ipv6F -> getRoa asId Ipv6F
                    Left af     -> throwParseError $ "Unsupported address family: " ++ show af)

        getRoa :: Int -> AddrFamily -> ParseASN1 [Vrp]
        getRoa asId addressFamily = onNextContainer Sequence $ getMany $
            getNextContainerMaybe Sequence >>= \case       
                Just [BitString (BitArray nzBits bs')] -> do
                    pLen <- getPrefixLength nzBits addressFamily
                    pure $ mkRoa bs' addressFamily nzBits pLen
                Just [BitString (BitArray nzBits bs'), IntVal maxLength] -> do
                    pLen <- getPrefixLength maxLength addressFamily
                    pure $ mkRoa bs' addressFamily nzBits pLen
                Just a  -> throwParseError $ "Unexpected ROA content: " <> show a
                Nothing -> throwParseError "Unexpected ROA content"
            where 
                mkRoa bs' Ipv4F nz len = Vrp (ASN (fromIntegral asId)) (Ipv4P $ make bs' (fromIntegral nz)) len
                mkRoa bs' Ipv6F nz len = Vrp (ASN (fromIntegral asId)) (Ipv6P $ make bs' (fromIntegral nz)) len

        getPrefixLength len = \case            
            Ipv4F 
                | len < 0  -> 
                    throwParseError $ "Negative value for IPv4 prefix length: " <> show len
                | len > 32 -> 
                    throwParseError $ "Too big value for IPv4 prefix length: " <> show len
                | otherwise ->
                    pure $ PrefixLength $ fromIntegral len
            Ipv6F 
                | len < 0  -> 
                    throwParseError $ "Negative value for IPv6 prefix length: " <> show len
                | len > 128 -> 
                    throwParseError $ "Too big value for IPv6 prefix length: " <> show len
                | otherwise ->
                    pure $ PrefixLength $ fromIntegral len
                        
