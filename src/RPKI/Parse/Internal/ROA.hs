{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module RPKI.Parse.Internal.ROA where

import qualified Data.ByteString as BS  

import Control.Monad
import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.ASN1.Parse
import Data.ASN1.BitArray

import Data.Bifunctor
import Data.String.Interpolate.IsString

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
            asId <- getInteger (pure . fromInteger) "Wrong ASid"
            mconcat <$> onNextContainer Sequence (getMany $
                onNextContainer Sequence $ 
                    getAddressFamily "Expected an address family here" >>= \case 
                        Right Ipv4F -> getRoa asId Ipv4F
                        Right Ipv6F -> getRoa asId Ipv6F
                        Left af     -> throwParseError $ "Unsupported address family: " ++ show af)

        getRoa :: Int -> AddrFamily -> ParseASN1 [Vrp]
        getRoa asId addressFamily = onNextContainer Sequence $ getMany $
            getNextContainerMaybe Sequence >>= \case       
                Just [BitString (BitArray nzBits bs')] ->
                    makeVrp asId bs' nzBits nzBits addressFamily
                Just [BitString (BitArray nzBits bs'), IntVal maxLength] ->
                    makeVrp asId bs' nzBits maxLength addressFamily
                Just a  -> throwParseError [i|Unexpected ROA content: #{a}|]
                Nothing -> throwParseError "Unexpected ROA content"

        makeVrp asId bs' nonZeroBitCount prefixMaxLength addressFamily = do
            when (nonZeroBitCount > fromIntegral prefixMaxLength) $
                throwParseError [i|Actual prefix length #{nonZeroBitCount} is bigger than the maximum length #{prefixMaxLength}.|]

            case addressFamily of
                Ipv4F 
                    | prefixMaxLength <= 0  -> 
                        throwParseError [i|Negative or zero value for IPv4 prefix max length: #{prefixMaxLength}|]
                    | prefixMaxLength > 32 -> 
                        throwParseError [i|Too big value for IPv4 prefix max length: #{prefixMaxLength}|]
                    | otherwise ->
                        pure $ mkVrp nonZeroBitCount prefixMaxLength Ipv4P
                Ipv6F 
                    | prefixMaxLength <= 0  -> 
                        throwParseError [i|Negative or zero value for IPv6 prefix max length: #{prefixMaxLength}|]
                    | prefixMaxLength > 128 -> 
                        throwParseError [i|Too big value for IPv6 prefix max length: #{prefixMaxLength}|]
                    | otherwise ->
                        pure $ mkVrp nonZeroBitCount prefixMaxLength Ipv6P
            where 
                mkVrp :: (Integral a, Integral c, Prefix b) => a -> c -> (b -> IpPrefix) -> Vrp
                mkVrp nz len mkIp = Vrp 
                            (ASN $ fromIntegral asId)
                            (mkIp $ make bs' (fromIntegral nz)) 
                            (PrefixLength $ fromIntegral len)