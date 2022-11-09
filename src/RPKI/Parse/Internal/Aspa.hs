{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module RPKI.Parse.Internal.Aspa where

import qualified Data.ByteString as BS  

import Control.Applicative
import Control.Monad

import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.ASN1.Parse

import Data.Bifunctor

import Data.Tuple.Strict

import RPKI.Domain 
import RPKI.Resources.Types
import RPKI.Parse.Internal.Common
import RPKI.Parse.Internal.SignedObject 

import RPKI.Util
import RPKI.Resources.IntervalSet as IS
import Data.Either


-- | Parse ASPA, https://datatracker.ietf.org/doc/draft-ietf-sidrops-aspa-profile/
-- 
parseAspa :: BS.ByteString -> ParseResult AspaObject
parseAspa bs = do    
    asns      <- first (fmtErr . show) $ decodeASN1' BER bs      
    signedAspa <- first fmtErr $ runParseASN1 (parseSignedObject $ parseSignedContent parseAspa') asns
    hash' <- getMetaFromSigned signedAspa bs
    pure $ newCMSObject hash' (CMS signedAspa)
  where     
    parseAspa' = onNextContainer Sequence $ do
        getAspaWithExplicitversion <|> getAspa
    
    getAspaWithExplicitversion = do 
        v <- getVersion
        when (v /= 0) $ throwParseError $ "Version must be 0 but was " <> show v
        getAspa        

    getVersion = getInteger (pure . fromInteger) "Wrong version"

    getAspa = do 
        customerAsn <- getInteger (pure . ASN . fromInteger) "Wrong customer AS"
        providerAsns <- onNextContainer Sequence $
                            getMany $ 
                                onNextContainer Sequence $ do
                                    asn <- getInteger (pure . ASN . fromInteger) "Wrong customer AS" 
                                    z <- hasNext 
                                    if z then 
                                            (asn,) <$> getAddressFamilyMaybe                                            
                                        else 
                                            pure (asn, Nothing)
        pure Aspa {..}



{-
[Start Sequence,
    IntVal 44851,
    Start Sequence,
        Start Sequence,
            IntVal 8203,
        End Sequence,
        Start Sequence,
            IntVal 8551,
        End Sequence,
        Start Sequence,
            IntVal 35488,
        End Sequence,
        Start Sequence,
            IntVal 199782,
            OctetString \"\\NUL\\STX\",
        End Sequence,
        Start Sequence,
            IntVal 211952,
            OctetString \"\\NUL\\STX\",
        End Sequence,
    End Sequence,
End Sequence]"

Aspa {
    customerAsn = ASN 44851, 
    providerAsns = [
        (ASN 8203,Nothing),
        (ASN 8551,Nothing),
        (ASN 35488,Nothing),
        (ASN 199782,Just Ipv6F),
        (ASN 211952,Just Ipv6F)
    ]}}


-}
