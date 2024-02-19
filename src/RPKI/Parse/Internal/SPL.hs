{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module RPKI.Parse.Internal.SPL where

import qualified Data.ByteString as BS  

import Control.Monad
import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.ASN1.Parse
import Data.ASN1.BitArray

import Data.Bifunctor
import Data.String.Interpolate.IsString

import RPKI.AppMonad
import RPKI.Domain 
import RPKI.Resources.Types
import RPKI.Parse.Internal.Common
import RPKI.Parse.Internal.SignedObject

import qualified RPKI.Util                  as U

-- | Parse ROA, https://tools.ietf.org/html/rfc6482
-- 
parseSpl :: BS.ByteString -> PureValidatorT SplObject
parseSpl bs = do    
    asns      <- fromEither $ first (parseErr . U.fmtGen) $ decodeASN1' BER bs  
    signedSpl <- fromEither $ first (parseErr . U.convert) 
                    $ runParseASN1 (parseSignedObject $ parseSignedContent parseSpls') asns
    hash' <- getMetaFromSigned signedSpl bs
    pure $ newCMSObject hash' (CMS signedSpl)
  where     
    parseSpls' = onNextContainer Sequence $ do      
        -- TODO Fix it so that it would work with present attestation version
        asId <- getInteger (pure . fromInteger) "Wrong ASid"
        mconcat <$> onNextContainer Sequence (getMany $
            onNextContainer Sequence $ 
                getAddressFamily "Expected an address family here" >>= \case 
                    Right Ipv4F -> getSpl asId Ipv4F
                    Right Ipv6F -> getSpl asId Ipv6F
                    Left af     -> throwParseError $ "Unsupported address family: " ++ show af)

    getSpl :: Int -> AddrFamily -> ParseASN1 [SplVrp]
    getSpl asId addressFamily = onNextContainer Sequence $ getMany $
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
        mkVrp :: (Integral a, Integral c, Prefix b) => a -> c -> (b -> IpPrefix) -> SplVrp
        mkVrp nz len mkIp = SplVrp $ Vrp 
                    (ASN $ fromIntegral asId)
                    (mkIp $ make bs' (fromIntegral nz)) 
                    (PrefixLength $ fromIntegral len)