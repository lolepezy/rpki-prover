{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module RPKI.Parse.Internal.RSC where

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
parseRCS :: BS.ByteString -> ParseResult RscObject
parseRCS bs = do    
    asns      <- first (fmtErr . show) $ decodeASN1' BER bs  
    signedRsc <- first fmtErr $ runParseASN1 (parseSignedObject $ parseSignedContent parseRsc') asns
    hash' <- getMetaFromSigned signedRsc bs
    pure $ newCMSObject hash' (CMS signedRsc)
    where     
        parseRsc' = onNextContainer Sequence $ do
            pure RCS {}
