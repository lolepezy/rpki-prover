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

import qualified Data.Set as S

import Data.ASN1.OID
import Data.ASN1.Types
import Data.ASN1.Parse

import Data.X509 as X509

import RPKI.Domain
import RPKI.Parse.Common


data SignedObject a = SignedObject {
    soContentType :: ContentType
  , soContent     :: SignedContent a
} deriving (Eq, Show)

data SignedContent a = SignedContent {
    scVersion          :: CMSVersion
  , scDigestAlgorithms :: DigestAlgorithmIdentifier
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
  , digestAlgorithm    :: DigestAlgorithmIdentifier
  , signedAttrs        :: Maybe SignedAttributes
  , signatureAlgorithm :: SignatureAlgorithmIdentifier
  , signature          :: SignatureValue
} deriving (Eq, Ord, Show)

newtype IssuerAndSerialNumber = IssuerAndSerialNumber T.Text deriving (Eq, Ord, Show)

newtype SignerIdentifier = SignerIdentifier SKI 
  deriving (Eq, Ord, Show)

newtype ContentType = ContentType OID deriving (Eq, Ord, Show)
newtype CMSVersion = CMSVersion Int deriving (Eq, Ord, Show)

newtype DigestAlgorithmIdentifier = DigestAlgorithmIdentifier OID deriving (Eq, Ord, Show)
newtype SignatureAlgorithmIdentifier = SignatureAlgorithmIdentifier OID  deriving (Eq, Ord, Show)

newtype SignedAttributes = SignedAttributes [Attribute] deriving (Eq, Ord, Show)  

data Attribute = ContentTypeAttr ContentType 
               | MessageDigest B.ByteString
               | SigningTime ASN1TimeType
               | BinarySigningTime Integer              
  deriving (Eq, Ord, Show)  


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
parseSignedObject :: ASN1Object a => (B.ByteString -> ParseASN1 a) -> ParseASN1 (SignedObject a)
parseSignedObject eContentParse = 
  SignedObject <$> parseContentType <*> parseContent  
  where  
    parseContentType = getNext >>= \case
      OID oid -> pure $ ContentType oid
      s       -> throwParseError $ "Unexpected contentType: " ++ show s    
    parseContent = SignedContent <$> 
        parseVersion <*>
        parseDigestAlgorithms <*>
        parseEncapContentInfo <*>
        parseEECertificate <*>
        parseSignerInfo

    parseVersion = getNext >>= \case 
      IntVal v -> pure $ CMSVersion $ fromInteger v
      s        -> throwParseError $ "Wrong version " ++ show s

    parseDigestAlgorithms = getNextContainerMaybe Set >>= \case
      Just [OID oid] -> pure $ DigestAlgorithmIdentifier oid
      s              -> throwParseError $ "DigestAlgorithms is wrong " ++ show s

    parseEncapContentInfo = onNextContainer Sequence $ do
      eContentType <- parseContentType
      eContent     <- eContentParse eContentType
      pure $ EncapsulatedContentInfo eContentType eContent

    parseEECertificate = getObject  

    parseSignerInfo = SignerInfos <$>
      parseVersion <*>
      parseSid <*>
      parseDigestAlgorithms <*>
      parseSignedAttributes <*>
      parseSignatureAlgorithm <*>
      parseSignature
      where 
        parseSid = getNext >>= \case 
          OctetString sid -> pure $ SignerIdentifier $ SKI $ KI sid
          s               -> throwParseError $ "Unknown SID : " ++ show s

        parseSignedAttributes = onNextContainer Set $ getMany $ do
          onNextContainer Sequence $ do
            OID attrId <- getNext
            case attrId of 
              id_contentType   -> ContentTypeAttr <$> parseContentType
              id_messageDigest -> getNext >>= \case
                  OctetString md -> pure $ MessageDigest md
                  s              -> throwParseError $ "Unknown SID : " ++ show s
              id_signingTime   -> getNext >>= \case                             
                  ASN1Time time dateTime tz ->
                  s              -> throwParseError $ "Unknown SigningTime : " ++ show s




      
      