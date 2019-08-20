{-# LANGUAGE DeriveAnyClass       #-}

module RPKI.SignTypes where

import qualified Data.ByteString as B  
import qualified Data.Text as T  

import Data.ASN1.OID
import Data.ASN1.Types

import Data.Data (Typeable)

import Data.Hourglass
import GHC.Generics

import Data.X509 as X509

data SignedObject a = SignedObject {
    soContentType :: !ContentType, 
    soContent     :: !(SignedData a)
} deriving (Show, Eq, Typeable, Generic)

data CertificateWithSignature = CertificateWithSignature 
  !X509.Certificate
  !SignatureAlgorithmIdentifier
  !SignatureValue
  -- TODO Test what is better here: strict or lazy
  !B.ByteString
  deriving (Show, Eq, Typeable, Generic)

{- 
      SignedData ::= SEQUENCE {
        version CMSVersion,
        digestAlgorithms DigestAlgorithmIdentifiers,
        encapContentInfo EncapsulatedContentInfo,
        certificates [0] IMPLICIT CertificateSet OPTIONAL,
        crls [1] IMPLICIT RevocationInfoChoices OPTIONAL,
        signerInfos SignerInfos }

      DigestAlgorithmIdentifiers ::= SET OF DigestAlgorithmIdentifier

      SignerInfos ::= SET OF SignerInfo
-}
data SignedData a = SignedData {
      scVersion          :: !CMSVersion
    , scDigestAlgorithms :: !DigestAlgorithmIdentifiers
    , scEncapContentInfo :: !(EncapsulatedContentInfo a)
    , scCertificate      :: !CertificateWithSignature
    , scSignerInfos      :: !SignerInfos
  } deriving (Show, Eq, Typeable, Generic)
  
  {- 
      EncapsulatedContentInfo ::= SEQUENCE {
          eContentType ContentType,
          eContent [0] EXPLICIT OCTET STRING OPTIONAL }
  -}
data EncapsulatedContentInfo a = EncapsulatedContentInfo {
      eContentType :: !ContentType
    , cContent     :: !a    
  } deriving (Show, Eq, Ord, Typeable, Generic)
  
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
      siVersion          :: !CMSVersion
    , siSid              :: !SignerIdentifier
    , digestAlgorithm    :: !DigestAlgorithmIdentifiers
    , signedAttrs        :: !SignedAttributes
    , signatureAlgorithm :: !SignatureAlgorithmIdentifier
    , signature          :: !SignatureValue
  } deriving (Show, Eq, Typeable, Generic)
  
newtype IssuerAndSerialNumber = IssuerAndSerialNumber T.Text 
  deriving (Eq, Ord, Show)
  
newtype SignerIdentifier = SignerIdentifier B.ByteString 
  deriving (Show, Eq, Ord, Typeable, Generic)
  
newtype ContentType = ContentType OID 
  deriving (Show, Eq, Ord, Typeable, Generic)
newtype CMSVersion = CMSVersion Int 
  deriving (Show, Eq, Ord, Typeable, Generic)

newtype DigestAlgorithmIdentifiers = DigestAlgorithmIdentifiers [OID] 
  deriving (Show, Eq, Ord, Typeable, Generic)

newtype SignatureAlgorithmIdentifier = SignatureAlgorithmIdentifier SignatureALG  
  deriving (Show, Eq, Typeable, Generic)

newtype SignatureValue = SignatureValue B.ByteString 
  deriving (Show, Eq, Ord, Typeable, Generic)  


-- Axccording to https://tools.ietf.org/html/rfc5652#page-16
-- there has to be DER encoded signedAttribute set
data SignedAttributes = SignedAttributes ![Attribute] B.ByteString
  deriving (Show, Eq, Typeable, Generic)

data Attribute = ContentTypeAttr ContentType 
            | MessageDigest B.ByteString
            | SigningTime DateTime (Maybe TimezoneOffset)
            | BinarySigningTime Integer 
            | UnknownAttribute OID [ASN1]
      deriving (Show, Eq, Typeable, Generic)


-- Signed CRL
data SignCRL = SignCRL {
  crl                :: CRL,
  signatureAlgorithm :: SignatureAlgorithmIdentifier,
  signatureValue     :: SignatureValue
} deriving (Show, Eq, Typeable, Generic)



