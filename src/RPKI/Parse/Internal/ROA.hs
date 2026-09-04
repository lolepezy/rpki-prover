{-# LANGUAGE OverloadedStrings #-}

module RPKI.Parse.Internal.ROA where

import qualified Data.ByteString as BS  

import Control.Applicative
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
parseRoa :: BS.ByteString -> PureValidatorT RoaObject
parseRoa bs = do    
    asns      <- fromEither $ first (parseErr . U.fmtGen) $ decodeASN1' DER bs  
    signedRoa <- fromEither $ first (parseErr . U.convert) 
                    $ runParseASN1 (parseSignedObject $ parseSignedContent parseRoas_) asns
    hash_ <- getMetaFromSigned signedRoa bs
    pure $ newCMSObject hash_ (CMS signedRoa)
  where     
    parseRoas_ = onNextContainer Sequence $ 
        parseRoaWithoutVersion <|> parseRoaWithVersion
    
    parseRoaWithVersion = do 
        onNextContainer (Container Context 0) $ do 
            v :: Int <- getInteger (pure . fromInteger) "Wrong version"
            when (v /= 0) $ throwParseError $ "Version must be 0 but was " <> show v                
        parseRoaWithoutVersion

    parseRoaWithoutVersion = do 
        asId <- getInteger pure "Wrong ASid"
        (v4s, v6s) <- mconcat <$> onNextContainer Sequence (getMany $
            onNextContainer Sequence $ 
                getAddressFamily "Expected an address family here" >>= \case 
                    Right Ipv4F -> (, []) <$> getRoa4
                    Right Ipv6F -> ([], ) <$> getRoa6
                    Left af     -> throwParseError $ "Unsupported address family: " ++ show af)
        pure $! VrpsPerAs (ASN $ fromIntegral asId) v4s v6s

    getRoa4 :: ParseASN1 [Vrp4]
    getRoa4 = onNextContainer Sequence $ getMany $
        getNextContainerMaybe Sequence >>= \case       
            Just [BitString (BitArray nzBits bs')] ->
                makeVrp4 bs' nzBits nzBits
            Just [BitString (BitArray nzBits bs'), IntVal maxLength] ->
                makeVrp4 bs' nzBits maxLength
            Just a  -> throwParseError [i|Unexpected ROA content: #{a}|]
            Nothing -> throwParseError "Unexpected ROA content"

    getRoa6 :: ParseASN1 [Vrp6]
    getRoa6 = onNextContainer Sequence $ getMany $
        getNextContainerMaybe Sequence >>= \case       
            Just [BitString (BitArray nzBits bs')] ->
                makeVrp6 bs' nzBits nzBits
            Just [BitString (BitArray nzBits bs'), IntVal maxLength] ->
                makeVrp6 bs' nzBits maxLength
            Just a  -> throwParseError [i|Unexpected ROA content: #{a}|]
            Nothing -> throwParseError "Unexpected ROA content"

    makeVrp4 bs' nonZeroBitCount prefixMaxLength = do
        when (nonZeroBitCount > fromIntegral prefixMaxLength) $
            throwParseError [i|Actual prefix length #{nonZeroBitCount} is bigger than the maximum length #{prefixMaxLength}.|]
        case () of
            _ | prefixMaxLength <= 0  -> 
                    throwParseError [i|Negative or zero value for IPv4 prefix max length: #{prefixMaxLength}|]
              | prefixMaxLength > 32  -> 
                    throwParseError [i|Too big value for IPv4 prefix max length: #{prefixMaxLength}|]
              | otherwise ->
                    pure $! Vrp4 (makePrefix bs' (fromIntegral nonZeroBitCount))
                                 (PrefixLength $ fromIntegral prefixMaxLength)

    makeVrp6 bs' nonZeroBitCount prefixMaxLength = do
        when (nonZeroBitCount > fromIntegral prefixMaxLength) $
            throwParseError [i|Actual prefix length #{nonZeroBitCount} is bigger than the maximum length #{prefixMaxLength}.|]
        case () of
            _ | prefixMaxLength <= 0   -> 
                    throwParseError [i|Negative or zero value for IPv6 prefix max length: #{prefixMaxLength}|]
              | prefixMaxLength > 128  -> 
                    throwParseError [i|Too big value for IPv6 prefix max length: #{prefixMaxLength}|]
              | otherwise ->
                    pure $! Vrp6 (makePrefix bs' (fromIntegral nonZeroBitCount))
                                 (PrefixLength $ fromIntegral prefixMaxLength)