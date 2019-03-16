{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module RPKI.Parse.SignedObject where
  
import qualified Data.ByteString as B
import qualified Data.Text as T

import Control.Applicative
import Data.Maybe

import Data.Hourglass

import Data.ASN1.OID
import Data.ASN1.Types
import Data.ASN1.Parse
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding

import Data.X509 as X509

import RPKI.Domain
import RPKI.Parse.Common


data SignedObject a = SignedObject {
    soContentType :: ContentType
  , soContent     :: SignedContent a
} deriving (Eq, Show)

data SignedContent a = SignedContent {
    scVersion          :: CMSVersion
  , scDigestAlgorithms :: DigestAlgorithmIdentifiers
  , scEncapContentInfo :: EncapsulatedContentInfo a
  , scCertificate      :: X509.Certificate
  , scSignerInfos      :: SignerInfos
} deriving (Eq, Show)

{- 
    EncapsulatedContentInfo ::= SEQUENCE {
        eContentType ContentType,
        eContent [0] EXPLICIT OCTET STRING OPTIONAL }
-}
data EncapsulatedContentInfo a = EncapsulatedContentInfo {
    eContentType :: ContentType
  , cContent     :: a    
} deriving (Eq, Ord, Show)

{-
    SignerInfo ::= SEQUENCE {
          version CMSVersion,
          sid SignerIdentifier,
          digestAlgorithm DigestAlgorithmIdentifier,
          signedAttrs [0] IMPLICIT SignedAttributes OPTIONAL,
          signatureAlgorithm SignatureAlgorithmIdentifier,
          signature SignatureValue,
          unsignedAttrs [1] IMPLICIT UnsignedAttributes OPTIONAL }
-}
data SignerInfos = SignerInfos {
    siVersion          :: CMSVersion
  , siSid              :: SignerIdentifier
  , digestAlgorithm    :: DigestAlgorithmIdentifiers
  , signedAttrs        :: SignedAttributes
  , signatureAlgorithm :: SignatureAlgorithmIdentifier
  , signature          :: SignatureValue
} deriving (Eq, Show)

newtype IssuerAndSerialNumber = IssuerAndSerialNumber T.Text deriving (Eq, Ord, Show)

newtype SignerIdentifier = SignerIdentifier SKI 
  deriving (Eq, Ord, Show)

newtype ContentType = ContentType OID deriving (Eq, Ord, Show)
newtype CMSVersion = CMSVersion Int deriving (Eq, Ord, Show)

newtype DigestAlgorithmIdentifiers = DigestAlgorithmIdentifiers [OID] deriving (Eq, Ord, Show)
newtype SignatureAlgorithmIdentifier = SignatureAlgorithmIdentifier OID  deriving (Eq, Ord, Show)

newtype SignedAttributes = SignedAttributes [Attribute] deriving (Eq, Show)  

data Attribute = ContentTypeAttr ContentType 
               | MessageDigest B.ByteString
               | SigningTime DateTime (Maybe TimezoneOffset)
               | BinarySigningTime Integer 
               | UnknownAttribute OID [ASN1]
  deriving (Eq, Show)  


newtype SignatureValue = SignatureValue B.ByteString deriving (Eq, Ord, Show)  

{- 

    contentType ContentType,
    content [0] EXPLICIT ANY DEFINED BY contentType

    SignedData ::= SEQUENCE {
        version CMSVersion,
        digestAlgorithms DigestAlgorithmIdentifiers,
        encapContentInfo EncapsulatedContentInfo,
        certificates [0] IMPLICIT CertificateSet OPTIONAL,
        crls [1] IMPLICIT RevocationInfoChoices OPTIONAL,
        signerInfos SignerInfos }

      DigestAlgorithmIdentifiers ::= SET OF DigestAlgorithmIdentifier

      SignerInfos ::= SET OF SignerInfo

      ContentType ::= OBJECT IDENTIFIER           
-}
parseSignedObject :: ParseASN1 a -> ParseASN1 (SignedObject a)
parseSignedObject eContentParse = 
  onNextContainer Sequence $
    SignedObject <$> parseContentType <*> parseContent  
  where  
    parseContentType = getOID (pure . ContentType) "Wrong OID for contentType"
    parseContent = onNextContainer (Container Context 0) $ 
      onNextContainer Sequence $
        SignedContent <$> 
          parseVersion <*> onNextContainer Set parseDigestAlgorithms <*>
          parseEncapContentInfo <*>
          parseEECertificate <*>
          onNextContainer Set (onNextContainer Sequence parseSignerInfo)

    parseVersion = getInteger (pure . CMSVersion . fromInteger) "Wrong version"

    parseDigestAlgorithms = onNextContainer Sequence $
          DigestAlgorithmIdentifiers . catMaybes <$> (getMany $ 
            getNext >>= \case
              OID oid -> pure $ Just  oid
              Null    -> pure Nothing
              s       -> throwParseError $ "DigestAlgorithms is wrong " ++ show s)

    parseEncapContentInfo = onNextContainer Sequence $ do
      contentType <- parseContentType            
      onNextContainer (Container Context 0) $                 
          (onNextContainer (Container Universal 4) (eContent contentType)) <|> 
          (eContent contentType)
        where
          eContent contentType = do 
            fullContent <- getMany getNext
            let bs = B.concat [ os | OctetString os <- fullContent ]
            case decodeASN1' BER bs of
              Left e     -> throwParseError $ "Couldn't decode embedded content: " ++ show e
              Right asns ->  
                case runParseASN1 eContentParse asns of
                  Left e  -> throwParseError $ "Couldn't parse embedded ASN1 stream: " ++ e
                  Right a -> pure $ EncapsulatedContentInfo contentType a    
        

    parseEECertificate = do
      onNextContainer (Container Context 0) $ 
                            onNextContainer Sequence $ do
                                -- TODO Parse signature separately ?
                                x <- onNextContainer Sequence getObject                                  
                                sig <- getNextContainerMaybe Sequence                                
                                bits <- getNext                                                      
                                pure x      

    parseSignerInfo = SignerInfos <$>
      parseVersion <*>
      parseSid <*>
      parseDigestAlgorithms <*>
      parseSignedAttributes <*>
      parseSignatureAlgorithm <*>
      parseSignature
      where 
        parseSid = getNext >>= \case 
          Other Context 0 ki -> pure $ SignerIdentifier $ SKI $ KI ki
          s                  -> throwParseError $ "Unknown SID : " ++ show s

        parseSignedAttributes = onNextContainer (Container Context 0) $ 
          SignedAttributes <$> attributes
            where 
              attributes = getMany $ 
                onNextContainer Sequence $ getNext >>= \case 
                  OID attrId 
                    | attrId == id_contentType -> getNextContainerMaybe Set >>= \case 
                            Just [OID ct] -> pure $ ContentTypeAttr $ ContentType ct
                            s -> throwParseError $ "Unknown contentType: " ++ show s
                    | attrId == id_messageDigest -> getNextContainerMaybe Set >>= \case
                            Just [OctetString md] -> pure $ MessageDigest md
                            s -> throwParseError $ "Unknown SID: " ++ show s
                    | attrId == id_signingTime -> getNextContainerMaybe Set >>= \case
                            Just [ASN1Time TimeUTC dt tz] -> pure $ SigningTime dt tz
                            s -> throwParseError $ "Unknown Signing Time: " ++ show s
                    | attrId == id_binarySigningTime -> getNextContainerMaybe Set >>= \case
                            Just [IntVal i] -> pure $ BinarySigningTime i
                            s -> throwParseError $ "Unknown Binary Signing Time: " ++ show s
                    | otherwise -> getNextContainerMaybe Set >>= \case
                            Just asn1 -> pure $ UnknownAttribute attrId asn1

                  s -> throwParseError $ "Unknown signed attribute OID: " ++ show s
                            
        
        parseSignature = getNext >>= \case 
            OctetString sig -> pure $ SignatureValue sig
            s               -> throwParseError $ "Unknown signature value : " ++ show s

        parseSignatureAlgorithm = onNextContainer Sequence $
            getNext >>= \case 
              OID oid -> do
                -- TODO Don't skip parameters, add them to the signature
                _ <- getMany getNext
                pure $ SignatureAlgorithmIdentifier oid