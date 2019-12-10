{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module RPKI.Domain where

import qualified Data.Set as S
import qualified Data.ByteString as B
import qualified Data.Text as T

import Control.DeepSeq
import Codec.Serialise
import Data.Hex (hex)

import Data.Kind (Type)
import Data.Data (Typeable)
import Data.List.NonEmpty
import Data.Hourglass

import GHC.Generics

import qualified Data.X509 as X509

import Data.ASN1.OID
import Data.ASN1.Types

import RPKI.IP    
import RPKI.Serialise.Orphans


newtype ASN = ASN Int
    deriving (Show, Eq, Ord, Typeable, Generic, NFData)

data AsResource =  AS !ASN
                 | ASRange  
                    {-# UNPACK #-} !ASN 
                    {-# UNPACK #-} !ASN
    deriving (Show, Eq, Ord, Typeable, Generic, NFData)

data ValidationRFC = Strict_ | Reconsidered
    deriving (Show, Eq, Ord, Typeable, Generic, NFData)

newtype WithRFC (rfc :: ValidationRFC) (r :: ValidationRFC -> Type) = WithRFC (r rfc)
    deriving (Show, Eq, Ord, Typeable, Generic)

type AnRFC r = Either (WithRFC 'Strict_ r) (WithRFC 'Reconsidered r)

withRFC :: AnRFC r -> (forall rfc . r rfc -> a) -> a
withRFC (Left (WithRFC a)) f = f a
withRFC (Right (WithRFC a)) f = f a  

newtype IpResources = IpResources (AnRFC IpResourceSet)    
    deriving (Show, Eq, Ord, Typeable, Generic)

newtype RSet r = RSet (AnRFC (ResourceSet r))
    deriving (Show, Eq, Ord, Typeable, Generic)

data ResourceSet r (rfc :: ValidationRFC) = RS (S.Set r) | Inherit
    deriving (Show, Eq, Ord, Typeable, Generic)

newtype IpResourceSet (rfc :: ValidationRFC) = 
    IpResourceSet (ResourceSet IpResource rfc)
    deriving (Show, Eq, Ord, Typeable, Generic)                    

-- TODO Use library type?
newtype Hash = Hash B.ByteString deriving (Show, Eq, Ord, Typeable, Generic, NFData)

newtype URI  = URI { unURI :: T.Text } deriving (Show, Eq, Ord, Typeable, Generic, NFData)
newtype KI   = KI  B.ByteString deriving (Show, Eq, Ord, Typeable, Generic, NFData)
newtype SKI  = SKI KI deriving (Show, Eq, Ord, Typeable, Generic, NFData)
newtype AKI  = AKI KI deriving (Show, Eq, Ord, Typeable, Generic, NFData)

newtype SessionId = SessionId B.ByteString deriving (Show, Eq, Ord, Typeable, Generic, NFData)
newtype Serial = Serial Integer deriving (Show, Eq, Ord, Typeable, Generic, NFData)
newtype Version = Version Integer deriving (Show, Eq, Ord, Typeable, Generic, NFData)


-- | Objects

newtype CMS a = CMS {
    unCMS :: SignedObject a
 } deriving (Show, Eq, Typeable, Generic)

data WithMeta a = WithMeta !RpkiMeta !a
    deriving (Show, Eq, Typeable, Generic)

type CrlObject = (CrlMeta, SignCRL)
type CerObject = ResourceCert
type MftObject = CMS Manifest
type RoaObject = CMS [Roa]
type GbrObject = CMS Gbr
    
data CrlMeta = CrlMeta {
    locations :: NonEmpty URI, 
    hash      :: Hash, 
    aki       :: AKI    
} deriving (Show, Eq, Ord, Typeable, Generic)

data RpkiMeta = RpkiMeta {
    locations :: NonEmpty URI, 
    hash      :: Hash, 
    aki       :: Maybe AKI, 
    ski       :: SKI
} deriving (Show, Eq, Ord, Typeable, Generic)

data RpkiSpecific = CerRO !CerObject 
                  | MftRO !MftObject
                  | RoaRO !RoaObject
                  | GbrRO !GbrObject
    deriving (Show, Eq, Typeable, Generic)

data RpkiObject = RpkiObject !(WithMeta RpkiSpecific)
                | RpkiCrl !CrlObject
    deriving (Show, Eq, Typeable, Generic)

-- data RpkiCrl = RpkiCrl !CrlMeta !CrlObject
--     deriving (Show, Eq, Typeable, Generic)

data ResourceCertificate (rfc :: ValidationRFC) = ResourceCertificate {
    certX509    :: CertificateWithSignature, 
    ipResources :: Maybe (IpResourceSet rfc), 
    asResources :: Maybe (ResourceSet AsResource rfc)
} deriving (Show, Eq, Typeable, Generic)

newtype ResourceCert = ResourceCert (AnRFC ResourceCertificate)
    deriving (Show, Eq, Typeable, Generic)

data Roa = Roa     
    {-# UNPACK #-} !ASN 
    !APrefix    
    {-# UNPACK #-} !Int
    deriving (Show, Eq, Ord, Typeable, Generic)

data Manifest = Manifest {
    mftNumber   :: Int, 
    fileHashAlg :: X509.HashALG, 
    thisTime    :: DateTime, 
    nextTime    :: DateTime, 
    mftEntries  :: [(T.Text, Hash)]
} deriving (Show, Eq, Typeable, Generic)

data SignCRL = SignCRL {
  crl                :: X509.CRL,
  signatureAlgorithm :: SignatureAlgorithmIdentifier,
  signatureValue     :: SignatureValue,
  encodedValue       :: B.ByteString,
  crlNumber          :: Integer
} deriving (Show, Eq, Typeable, Generic)

data Gbr = Gbr deriving (Show, Eq, Ord, Typeable, Generic)


-- Types for the signed object template 

data SignedObject a = SignedObject {
    soContentType :: ContentType, 
    soContent     :: SignedData a
} deriving (Show, Eq, Typeable, Generic)


data CertificateWithSignature = CertificateWithSignature {
    cwsX509certificate :: X509.Certificate,
    cwsSignatureAlgorithm :: SignatureAlgorithmIdentifier,
    cwsSignature :: SignatureValue,
    cwsEncoded :: B.ByteString
  } deriving (Show, Eq, Typeable, Generic)

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
      scVersion          :: CMSVersion, 
      scDigestAlgorithms :: DigestAlgorithmIdentifiers, 
      scEncapContentInfo :: EncapsulatedContentInfo a, 
      scCertificate      :: ResourceCert, 
      scSignerInfos      :: SignerInfos
  } deriving (Show, Eq, Typeable, Generic)
  
  {- 
      EncapsulatedContentInfo ::= SEQUENCE {
          eContentType ContentType,
          eContent [0] EXPLICIT OCTET STRING OPTIONAL }
  -}
data EncapsulatedContentInfo a = EncapsulatedContentInfo {
      eContentType :: ContentType, 
      cContent     :: a    
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
      siVersion          :: CMSVersion, 
      siSid              :: SignerIdentifier, 
      digestAlgorithm    :: DigestAlgorithmIdentifiers, 
      signedAttrs        :: SignedAttributes, 
      signatureAlgorithm :: SignatureAlgorithmIdentifier, 
      signature          :: SignatureValue
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

newtype SignatureAlgorithmIdentifier = SignatureAlgorithmIdentifier X509.SignatureALG  
  deriving (Show, Eq, Typeable, Generic)

newtype SignatureValue = SignatureValue B.ByteString 
  deriving (Show, Eq, Ord, Typeable, Generic)  


-- Axccording to https://tools.ietf.org/html/rfc5652#page-16
-- there has to be DER encoded signedAttribute set
data SignedAttributes = SignedAttributes [Attribute] B.ByteString
  deriving (Show, Eq, Typeable, Generic)

data Attribute = ContentTypeAttr ContentType 
            | MessageDigest B.ByteString
            | SigningTime DateTime (Maybe TimezoneOffset)
            | BinarySigningTime Integer 
            | UnknownAttribute OID [ASN1]
      deriving (Show, Eq, Typeable, Generic)



-- Subject Public Key Info
newtype SPKI = SPKI EncodedBase64
    deriving (Show, Eq, Ord, Typeable, Generic, Serialise)

newtype EncodedBase64 = EncodedBase64 B.ByteString
    deriving (Show, Eq, Ord, Generic, NFData, Serialise)
    deriving newtype (Monoid, Semigroup)
  
newtype DecodedBase64 = DecodedBase64 B.ByteString
    deriving (Show, Eq, Ord, Generic, NFData, Serialise)
    deriving newtype (Monoid, Semigroup)
  

newtype TaName = TaName T.Text
    deriving (Show, Eq, Ord, Generic, NFData, Serialise)

data TA = TA {
    taName        :: TaName
  , taCertificate :: Maybe ResourceCert
  , taUri         :: URI
  , taSpki        :: SPKI
} deriving (Show, Eq, Generic, Serialise)


data RsyncRepository = RsyncRepository {
    uri :: URI    
} deriving (Show, Eq, Ord, Typeable, Generic)

data RrdpRepository = RrdpRepository {
    uri :: URI,
    session :: Maybe (SessionId, Serial)
} deriving (Show, Eq, Ord, Typeable, Generic)

data Repository = 
    RsyncRepo RsyncRepository | 
    RrdpRepo RrdpRepository 
    deriving (Show, Eq, Ord, Typeable, Generic)

        

-- serialisation
instance Serialise Hash
instance Serialise RpkiMeta
instance Serialise CrlMeta
instance Serialise URI
instance Serialise AKI
instance Serialise SKI
instance Serialise KI
instance Serialise Serial
instance Serialise RpkiSpecific
instance Serialise Manifest
instance Serialise Roa
instance Serialise Gbr
instance Serialise ASN
instance Serialise a => Serialise (CMS a)
instance Serialise SignCRL
instance Serialise ResourceCert
instance Serialise RpkiObject
instance Serialise a => Serialise (WithMeta a)
instance Serialise (WithRFC 'Strict_ ResourceCertificate)
instance Serialise (ResourceCertificate 'Strict_)
instance Serialise (IpResourceSet 'Strict_)
instance Serialise (ResourceSet IpResource 'Strict_)
instance Serialise (ResourceCertificate 'Reconsidered)
instance Serialise (IpResourceSet 'Reconsidered)
instance Serialise (ResourceSet IpResource 'Reconsidered)
instance Serialise (WithRFC 'Reconsidered ResourceCertificate)
instance Serialise (ResourceSet AsResource 'Strict_)
instance Serialise (ResourceSet AsResource 'Reconsidered)
instance Serialise AsResource


instance Serialise ContentType
instance Serialise a => Serialise (EncapsulatedContentInfo a)
instance Serialise a => Serialise (SignedObject a)
instance Serialise a => Serialise (SignedData a)
instance Serialise CMSVersion
instance Serialise DigestAlgorithmIdentifiers
instance Serialise SignatureAlgorithmIdentifier
instance Serialise SignatureValue
instance Serialise SignerIdentifier
instance Serialise SignedAttributes
instance Serialise Attribute
instance Serialise CertificateWithSignature
instance Serialise SignerInfos

-- 
getHash :: RpkiObject -> Hash
getHash (RpkiObject (WithMeta RpkiMeta {..} _)) = hash
getHash (RpkiCrl (CrlMeta {..}, _)) = hash

getLocations :: RpkiObject -> NonEmpty URI
getLocations (RpkiObject (WithMeta RpkiMeta {..} _)) = locations
getLocations (RpkiCrl (CrlMeta {..}, _)) = locations

getAKI :: RpkiObject -> Maybe AKI
getAKI (RpkiObject (WithMeta RpkiMeta {..} _)) = aki
getAKI (RpkiCrl (CrlMeta {..}, _)) = Just aki

getMeta :: RpkiObject -> Maybe RpkiMeta
getMeta (RpkiObject (WithMeta m _)) = Just m
getMeta _                           = Nothing

getSerial :: CerObject -> Serial
getSerial rc = Serial $ X509.certSerial $ cwsX509certificate $ withRFC cert certX509 
    where ResourceCert cert = rc

hexHash :: Hash -> String
hexHash (Hash bs) = show $ hex bs

toAKI :: SKI -> AKI
toAKI (SKI ki) = AKI ki

getCMSContent :: CMS a -> a
getCMSContent (CMS so) = cContent $ scEncapContentInfo $ soContent so

getEEResourceCert :: SignedObject a -> ResourceCert
getEEResourceCert = scCertificate . soContent

getEECert :: SignedObject a -> CertificateWithSignature
getEECert so = withRFC rc certX509 
    where
        ResourceCert rc = scCertificate $ soContent so