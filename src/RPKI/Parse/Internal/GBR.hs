{-# LANGUAGE OverloadedStrings #-}

module RPKI.Parse.Internal.GBR where

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


-- | Parse Ghostbusters record (https://tools.ietf.org/html/rfc6493)
-- 
parseGbr :: BS.ByteString -> ParseResult (RpkiURL -> GbrObject)
parseGbr bs = do    
    asns      <- first (fmtErr . show) $ decodeASN1' BER bs  
    signedGbr <- first fmtErr $ runParseASN1 (parseSignedObject parseGbr') asns
    meta <- getMetaFromSigned signedGbr bs
    pure $ \url -> let 
                        (hash, loc) = meta url 
                    in newCMSObject hash loc (CMS signedGbr)
    where     
        parseGbr' contentType bs = 
            pure $ EncapsulatedContentInfo contentType (Gbr $ toShortBS bs)
