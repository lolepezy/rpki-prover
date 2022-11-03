{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module RPKI.Parse.Internal.Aspa where

import qualified Data.ByteString as BS  

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


-- | Parse RSC, https://datatracker.ietf.org/doc/draft-ietf-sidrops-rpki-rsc/
-- 
parseAspa :: BS.ByteString -> ParseResult AspaObject
parseAspa bs = do    
    asns      <- first (fmtErr . show) $ decodeASN1' BER bs      
    signedAspa <- first fmtErr $ runParseASN1 (parseSignedObject $ parseSignedContent parseAspa') asns
    hash' <- getMetaFromSigned signedAspa bs
    pure $ newCMSObject hash' (CMS signedAspa)
  where     
    parseAspa' = onNextContainer Sequence $ do        
        pure Aspa {}
