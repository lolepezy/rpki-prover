{-# LANGUAGE OverloadedStrings #-}
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

import RPKI.AppMonad
import RPKI.Domain 
import RPKI.Resources.Types
import RPKI.Parse.Internal.Common
import RPKI.Parse.Internal.SignedObject 

import qualified RPKI.Util as U


-- | Parse ASPA, https://datatracker.ietf.org/doc/draft-ietf-sidrops-aspa-profile/
-- 
parseAspa :: BS.ByteString -> PureValidatorT AspaObject
parseAspa bs = do    
    asns       <- fromEither $ first (parseErr . U.fmtGen) $ decodeASN1' BER bs
    signedAspa <- fromEither $ first (parseErr . U.fmtGen) $ 
                    runParseASN1 (parseSignedObject $ parseSignedContent parseAspa') asns
    hash' <- getMetaFromSigned signedAspa bs
    pure $ newCMSObject hash' (CMS signedAspa)
  where     
    parseAspa' = onNextContainer Sequence $
        getAspaWithExplicitversion <|> getAspa
    
    getAspaWithExplicitversion = do 
        v :: Int <- getVersion
        when (v /= 0) $ throwParseError $ "Version must be 0 but was " <> show v
        getAspa        

    getVersion = getInteger (pure . fromInteger) "Wrong version"

    getAspa = do 
        customer  <- getInteger (pure . ASN . fromInteger) "Wrong customer AS"
        providers <- onNextContainer Sequence $
                            getMany $ 
                                onNextContainer Sequence $ do
                                    asn <- getInteger (pure . ASN . fromInteger) "Wrong customer AS" 
                                    z <- hasNext 
                                    if z then 
                                            (asn,) <$> getAddressFamilyMaybe                                            
                                        else 
                                            pure (asn, Nothing)
        pure Aspa {..}