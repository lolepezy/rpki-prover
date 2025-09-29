{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module RPKI.Parse.Internal.Erik where

import qualified Data.ByteString as BS  
import qualified Data.Text as Text  

import Control.Applicative
import Control.Monad
import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.ASN1.Parse

import Data.Bifunctor

import RPKI.AppMonad
import RPKI.Domain 
import RPKI.Parse.Internal.Common   

import qualified RPKI.Util as U
import RPKI.Time
import Data.ASN1.BitArray

-- | Parse Erik sync protocol objects, 
-- https://datatracker.ietf.org/doc/html/draft-spaghetti-sidrops-rpki-erik-protocol
-- 

parseErikIndex :: BS.ByteString -> PureValidatorT ErikIndex
parseErikIndex bs = do    
    asn1s     <- fromEither $ first (parseErr . U.fmtGen) $ decodeASN1' BER bs
    fromEither $ first (parseErr . U.convert) $ runParseASN1 parseIndex asn1s
  where     
    parseIndex = onNextContainer Sequence $ do
        parseIndexFieldsWithVersion <|> parseIndexFields      

    parseIndexFieldsWithVersion = do
        version :: Int <- getInteger (pure . fromInteger) "Wrong version"
        when (version /= 0) $ 
            throwParseError $ "Unexpected manifest version: " ++ show version
        parseIndexFields

    parseIndexFields = do         
        indexScope    <- getIA5String (pure . Text.pack) "Wrong indexScope"
        indexTime     <- newInstant <$> getTime "No partitionTime"
        previousIndex <- getOptionalHash
        partitionList <- getPartitionList
        pure $ ErikIndex {..}    

    getOptionalHash = getNextMaybe $ \case            
        BitString (BitArray _ b) -> Just $ U.mkHash b
        _                        -> Nothing
                
    getPartitionList = onNextContainer Sequence $
        getMany $ onNextContainer Sequence $
            PartitionListEntry 
                <$> getInteger (pure . PartitionIdentifier) "Wrong serial for partition number"
                <*> getBitString (pure . U.mkHash) "Wrong hash"
                <*> getInteger (pure . Size . fromIntegral) "Wrong size for partition size"

parseErikPartition :: BS.ByteString -> PureValidatorT ErikPartition
parseErikPartition bs = do    
    asn1s     <- fromEither $ first (parseErr . U.fmtGen) $ decodeASN1' BER bs
    fromEither $ first (parseErr . U.convert) $ runParseASN1 parsePartition asn1s
  where     
    parsePartition = onNextContainer Sequence $ do      
        parsePartitionFieldsWithVersion <|> parsePartitionFields

    parsePartitionFieldsWithVersion = do
        version :: Int <- getInteger (pure . fromInteger) "Wrong version"
        when (version /= 0) $ 
            throwParseError $ "Unexpected manifest version: " ++ show version
        parsePartitionFields

    parsePartitionFields = do 
        partitionTime <- newInstant <$> getTime "No partitionTime"
        hashAlg      <- getOID (pure . DigestAlgorithmIdentifier) "Wrong hash algorithm OID"
        manifestList <- getManifestList
        pure $ ErikPartition {..}
        
    makeMftNumber n = either throwParseError pure $ makeSerial n

    getManifestList = onNextContainer Sequence $
        getMany $ onNextContainer Sequence $ do 
            hash           <- getBitString (pure . U.mkHash) "Wrong hash"
            size           <- getInteger (pure . Size . fromIntegral) "Wrong size for manifest list size"
            aki            <- getBitString (pure . AKI . mkKI) "Wrong AKI"
            manifestNumber <- getInteger makeMftNumber "Wrong serial for manifest list number"
            thisUpdate     <- newInstant <$> getTime "No partitionTime"                
            location       <- getIA5String (pure . URI . Text.pack) "Wrong location for manifest URI"            
            pure $ ManifestListEntry {..}                
                    
