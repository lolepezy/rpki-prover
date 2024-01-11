{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module RPKI.Parse.Internal.Aspa where

import qualified Data.ByteString as BS  

import Control.Applicative
import Control.Monad

import qualified Data.Set as Set

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
    signedAspa <- fromEither $ first (parseErr . U.convert) $ 
                    runParseASN1 (parseSignedObject $ parseSignedContent parseAspa') asns
    hash' <- getMetaFromSigned signedAspa bs
    pure $ newCMSObject hash' (CMS signedAspa)
  where     
    parseAspa' = onNextContainer Sequence $ do         
        getAspaWithExplicitVersion <|> getAspa
    
    getAspaWithExplicitVersion = do 
        onNextContainer (Container Context 0) $ do 
            v :: Int <- getVersion
            when (v /= 1) $ throwParseError $ "Version must be 1 but was " <> show v                
        getAspa        

    getVersion = getInteger (pure . fromInteger) "Wrong version"

    getAspa = do 
        customer  <- getInteger (pure . ASN . fromInteger) "Wrong customer AS"        
        providers <- fmap Set.fromList $
                        onNextContainer Sequence $ getMany $                             
                                getInteger (pure . ASN . fromInteger) "Wrong provider AS" 
        pure Aspa {..}