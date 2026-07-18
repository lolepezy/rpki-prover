{-# LANGUAGE OverloadedStrings #-}

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
import RPKI.AppTypes
import RPKI.Time

-- | Parse Erik sync protocol objects, 
-- https://datatracker.ietf.org/doc/draft-ietf-sidrops-rpki-erik-protocol/
-- 

parseErikIndex :: BS.ByteString -> PureValidatorT ErikIndex
parseErikIndex bs = do    
    asn1s     <- fromEither $ first (parseErr . U.fmtGen) $ decodeASN1' BER bs
    fromEither $ first (parseErr . U.convert) $ runParseASN1 parseWrapper asn1s
  where     
    parseWrapper = onNextContainer Sequence $ do
        contentType <- getOID pure "Wrong OID for the index"

        when (contentType /= id_ct_rpkiErikIndex) $
            throwParseError $ "Unexpected OID for Erik index: " <> show contentType 
                            <> ", expected " <> show id_ct_rpkiErikIndex

        onNextContainer (Container Context 0) $ 
            onNextContainer Sequence $ 
                parseIndexFields <|> parseIndexFieldsWithVersion   

    parseIndexFieldsWithVersion = do
        version :: Int <- getInteger (pure . fromInteger) "Wrong version"
        when (version /= 0) $ 
            throwParseError $ "Unexpected index version: " ++ show version
        parseIndexFields

    parseIndexFields = do                    
        indexScope    <- getIA5String (pure . Text.pack) "Wrong indexScope"        
        indexTime     <- newInstant <$> getTime "No partitionTime"        
        hashAlg       <- onNextContainer Sequence $ 
                            getOID (pure . DigestAlgorithmIdentifier) "Wrong hash algorithm OID"
        partitionList <- getPartitionList
        pure $ ErikIndex {..}    
                
    getPartitionList = onNextContainer Sequence $
        getMany $ onNextContainer Sequence $
            ErikPartitionListEntry 
                <$> getOctetString (pure . U.mkHash) "Wrong hash"
                <*> getInteger (pure . Size . fromIntegral) "Wrong size for partition size"


parseErikPartition :: BS.ByteString -> PureValidatorT ErikPartition
parseErikPartition bs = do    
    asn1s <- fromEither $ first (parseErr . U.fmtGen) $ decodeASN1' BER bs
    fromEither $ first (parseErr . U.convert) $ runParseASN1 parsePartition asn1s
  where     
    parsePartition = onNextContainer Sequence $ do
        contentType <- getOID pure "Wrong OID for partition"

        when (contentType /= id_ct_rpkiErikPartition) $
            throwParseError $ "Unexpected OID for Erik partition: " <> show contentType
                            <> ", expected " <> show id_ct_rpkiErikPartition

        onNextContainer (Container Context 0) $
            onNextContainer Sequence $
                parsePartitionFields <|> parsePartitionFieldsWithVersion

    parsePartitionFieldsWithVersion = do
        version :: Int <- getInteger (pure . fromInteger) "Wrong version"
        when (version /= 0) $ 
            throwParseError $ "Unexpected partition version: " <> show version
        parsePartitionFields

    parsePartitionFields = do 
        partitionTime <- newInstant <$> getTime "No partitionTime"
        hashAlg       <- onNextContainer Sequence $
                            getOID (pure . DigestAlgorithmIdentifier) "Wrong hash algorithm OID"
        manifestList  <- getManifestList
        pure $ ErikPartition {..}

    makeMftNumber = either throwParseError pure . makeSerial

    getManifestList = onNextContainer Sequence $
        getMany $ onNextContainer Sequence $ do 
            hash           <- getOctetString (pure . U.mkHash) "Wrong hash"
            size           <- getInteger (pure . Size . fromIntegral) "Wrong size for manifest list size"
            aki            <- getOctetString (pure . AKI . mkKI) "Wrong AKI"
            manifestNumber <- getInteger makeMftNumber "Wrong serial for manifest list number"
            thisUpdate     <- newInstant <$> getTime "No partitionTime"                
            locations      <- onNextContainer Sequence $ getMany getAccessDescription                                
            pure $ ErikManifestRef {..}

    getAccessDescription =
        onNextContainer Sequence $ do
            _accessMethod <- getOID pure "Wrong access method OID"
            asn1          <- getNext
            case asn1 of
                Other Context 6 uriBytes ->
                    either (throwParseError . Text.unpack) pure $ extractURI uriBytes
                other -> throwParseError $ "Expected URI [6] in AccessDescription, got: " <> show other


parseErikSegmentIndex :: BS.ByteString -> PureValidatorT ErikSegmentIndex
parseErikSegmentIndex bs = do    
    asn1s <- fromEither $ first (parseErr . U.fmtGen) $ decodeASN1' BER bs
    fromEither $ first (parseErr . U.convert) $ runParseASN1 parseWrapper asn1s
  where     
    parseWrapper = onNextContainer Sequence $ do
        contentType <- getOID pure "Wrong OID for the segment index"

        when (contentType /= id_ct_rpkiErikSegmentIndex) $
            throwParseError $ "Unexpected OID for Erik segment index: " <> show contentType 
                            <> ", expected " <> show id_ct_rpkiErikSegmentIndex

        onNextContainer (Container Context 0) $ 
            onNextContainer Sequence $ 
                parseSegmentIndexFields <|> parseSegmentIndexFieldsWithVersion

    parseSegmentIndexFieldsWithVersion = do
        version :: Int <- getInteger (pure . fromInteger) "Wrong version"
        when (version /= 0) $ 
            throwParseError $ "Unexpected segment index version: " ++ show version
        parseSegmentIndexFields

    parseSegmentIndexFields = do
        segmentScope <- getIA5String (pure . Text.pack) "Wrong segmentScope"
        segmentTime  <- newInstant <$> getTime "No segmentTime"
        hashAlg      <- onNextContainer Sequence $
                            getOID (pure . DigestAlgorithmIdentifier) "Wrong hash algorithm OID"
        segmentList  <- getSegmentList
        pure $ ErikSegmentIndex {..}

    getSegmentList = onNextContainer Sequence $
        getMany $ onNextContainer Sequence $
            ErikSegmentRef
                <$> (newInstant <$> getTime "Wrong segment time")
                <*> getOctetString (pure . U.mkHash) "Wrong index hash"