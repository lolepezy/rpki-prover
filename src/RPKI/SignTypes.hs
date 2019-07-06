module RPKI.SignTypes where

import qualified Data.ByteString as B  
import qualified Data.Text as T  

import Data.ASN1.OID
import Data.ASN1.Types

import Data.Hourglass

import Data.X509 as X509
import RPKI.Domain

newtype SignatureValue = SignatureValue B.ByteString 
  deriving (Eq, Ord, Show)  

data SignedObject a = SignedObject {
    soContentType :: !ContentType
  , soContent     :: !(SignedContent a)
} deriving (Eq, Show)

data CertificateWithSignature = CertificateWithSignature 
  !X509.Certificate
  !SignatureAlgorithmIdentifier
  !SignatureValue
  deriving (Eq, Show)

data SignedContent a = SignedContent {
      scVersion          :: !CMSVersion
    , scDigestAlgorithms :: !DigestAlgorithmIdentifiers
    , scEncapContentInfo :: !(EncapsulatedContentInfo a)
    , scCertificate      :: !CertificateWithSignature
    , scSignerInfos      :: !SignerInfos
  } deriving (Eq, Show)
  
  {- 
      EncapsulatedContentInfo ::= SEQUENCE {
          eContentType ContentType,
          eContent [0] EXPLICIT OCTET STRING OPTIONAL }
  -}
data EncapsulatedContentInfo a = EncapsulatedContentInfo {
      eContentType :: !ContentType
    , cContent     :: !a    
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
      siVersion          :: !CMSVersion
    , siSid              :: !SignerIdentifier
    , digestAlgorithm    :: !DigestAlgorithmIdentifiers
    , signedAttrs        :: !SignedAttributes
    , signatureAlgorithm :: !SignatureAlgorithmIdentifier
    , signature          :: !SignatureValue
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
