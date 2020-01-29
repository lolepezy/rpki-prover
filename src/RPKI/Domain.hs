{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

module RPKI.Domain where

import qualified Data.ByteString as B
import qualified Data.Text as T

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

import RPKI.Resources as RS
import RPKI.Serialise.Orphans

newtype WithRFC (rfc :: ValidationRFC) (r :: ValidationRFC -> Type) = WithRFC (r rfc)
    deriving stock (Show, Eq, Ord, Typeable, Generic)

type AnRFC r = WithRFC_ (WithRFC 'Strict_ r) (WithRFC 'Reconsidered_ r)

data WithRFC_ s r = WithStrict_       !s 
                  | WithReconsidered_ !r
    deriving stock (Show, Eq, Ord, Typeable, Generic)

withRFC :: AnRFC r -> (forall rfc . r rfc -> a) -> a
withRFC (WithStrict_ (WithRFC a)) f = f a
withRFC (WithReconsidered_ (WithRFC a)) f = f a  

forRFC :: AnRFC r -> (r 'Strict_ -> a) -> (r 'Reconsidered_ -> a) -> a
forRFC (WithStrict_ (WithRFC a))       f _ = f a
forRFC (WithReconsidered_ (WithRFC a)) _ g = g a  

-- TODO Use library type?
newtype Hash = Hash B.ByteString deriving stock (Show, Eq, Ord, Typeable, Generic)

newtype URI  = URI { unURI :: T.Text } deriving stock (Show, Eq, Ord, Typeable, Generic)
newtype KI   = KI  B.ByteString deriving stock (Show, Eq, Ord, Typeable, Generic)
newtype SKI  = SKI KI deriving stock (Show, Eq, Ord, Typeable, Generic)
newtype AKI  = AKI KI deriving stock (Show, Eq, Ord, Typeable, Generic)

newtype SessionId = SessionId B.ByteString deriving stock (Show, Eq, Ord, Typeable, Generic)
newtype Serial = Serial Integer deriving stock (Show, Eq, Ord, Typeable, Generic)
newtype Version = Version Integer deriving stock (Show, Eq, Ord, Typeable, Generic)


-- | Domain objects

newtype CMS a = CMS {
    unCMS :: SignedObject a
 } deriving stock (Show, Eq, Typeable, Generic)

class WithAKI a where
    getAKI :: a -> Maybe AKI

class WithLocations a where
    getLocations :: a -> NonEmpty URI 

class WithHash a where
    getHash :: a -> Hash

class WithSKI a where
    getSKI :: a -> SKI

class WithResourceCertificate a where
    getRC :: a -> ResourceCertificate

data IdentityMeta = IdentityMeta 
                   !Hash 
    {-# UNPACK #-} !(NonEmpty URI)
    deriving stock (Show, Eq, Ord, Typeable, Generic)

data With meta content = With !meta !content 
    deriving stock (Show, Eq, Ord, Typeable, Generic)

class Contains_ a b where
    extract :: a -> b

instance {-# OVERLAPPING #-} Contains_ whole part => Contains_ (With meta whole) part where
    extract (With _ b) = extract b

instance {-# OVERLAPPING #-} (a ~ b) => Contains_ a b where
    extract = id

type CrlObject = With IdentityMeta (With AKI SignCRL)
type CerObject = With IdentityMeta (With (Maybe AKI) (With SKI ResourceCertificate))
type MftObject = With IdentityMeta (CMS Manifest)
type RoaObject = With IdentityMeta (CMS [Roa])
type GbrObject = With IdentityMeta (CMS Gbr) 

type EECerObject = With AKI (With SKI ResourceCertificate)


data RpkiObject = CerRO !CerObject 
                | MftRO !MftObject
                | RoaRO !RoaObject
                | GbrRO !GbrObject
                | CrlRO !CrlObject
    deriving stock (Show, Eq, Typeable, Generic)


instance WithHash (With IdentityMeta a) where
    getHash (With (IdentityMeta h _) _) = h    

instance WithLocations (With IdentityMeta a) where
    getLocations (With (IdentityMeta _ loc) _) = loc
    
instance {-# OVERLAPPING #-} WithSKI a => WithSKI (With m a) where
    getSKI (With _ a) = getSKI a    

instance {-# OVERLAPPING #-} WithAKI a => WithAKI (With m a) where
    getAKI (With _ a) = getAKI a    

instance {-# OVERLAPPING #-} WithSKI (With SKI a) where
    getSKI (With ski _) = ski    

instance {-# OVERLAPPING #-} WithAKI (With (Maybe AKI) a) where
    getAKI (With aki _) = aki

instance {-# OVERLAPPING #-} WithAKI (With AKI a) where
    getAKI (With aki _) = Just aki

instance {-# OVERLAPPING #-} WithAKI (With x (CMS a)) where
    getAKI (With _ (CMS signedObject)) = getAKI $ getEEResourceCert signedObject

instance {-# OVERLAPPING #-} WithSKI (With x (CMS a)) where
    getSKI (With _ (CMS signedObject)) = getSKI $ getEEResourceCert signedObject
        
instance WithResourceCertificate CerObject where
    getRC = extract

instance WithResourceCertificate EECerObject where
    getRC = extract

instance WithAKI RpkiObject where
    getAKI (CerRO c) = getAKI c
    getAKI (MftRO c) = getAKI c
    getAKI (RoaRO c) = getAKI c
    getAKI (GbrRO c) = getAKI c
    getAKI (CrlRO c) = getAKI c

instance WithHash RpkiObject where
    getHash (CerRO c) = getHash c
    getHash (MftRO c) = getHash c
    getHash (RoaRO c) = getHash c
    getHash (GbrRO c) = getHash c
    getHash (CrlRO c) = getHash c

instance WithLocations RpkiObject where
    getLocations (CerRO c) = getLocations c
    getLocations (MftRO c) = getLocations c
    getLocations (RoaRO c) = getLocations c
    getLocations (GbrRO c) = getLocations c
    getLocations (CrlRO c) = getLocations c
    
    
-- More concrete data structures for resource certificates, CRLs, MFTs, ROAs

data ResourceCert (rfc :: ValidationRFC) = ResourceCert {
    certX509  :: CertificateWithSignature, 
    resources :: AllResources
} deriving stock (Show, Eq, Typeable, Generic)

newtype ResourceCertificate = ResourceCertificate (AnRFC ResourceCert)
    deriving stock (Show, Eq, Typeable, Generic)

data Roa = Roa     
    {-# UNPACK #-} !ASN 
    !IpPrefix    
    {-# UNPACK #-} !Int
    deriving stock (Show, Eq, Ord, Typeable, Generic)

data Manifest = Manifest {
    mftNumber   :: Int, 
    fileHashAlg :: X509.HashALG, 
    thisTime    :: DateTime, 
    nextTime    :: DateTime, 
    mftEntries  :: [(T.Text, Hash)]
} deriving stock (Show, Eq, Typeable, Generic)

data SignCRL = SignCRL {
  crl                :: X509.CRL,
  signatureAlgorithm :: SignatureAlgorithmIdentifier,
  signatureValue     :: SignatureValue,
  encodedValue       :: B.ByteString,
  crlNumber          :: Integer
} deriving stock (Show, Eq, Typeable, Generic)

-- TODO Define it
data Gbr = Gbr deriving stock (Show, Eq, Ord, Typeable, Generic)



-- | Types for the signed object template 
-- https://tools.ietf.org/html/rfc5652

data SignedObject a = SignedObject {
    soContentType :: ContentType, 
    soContent     :: SignedData a
} deriving stock (Show, Eq, Typeable, Generic)


data CertificateWithSignature = CertificateWithSignature {
    cwsX509certificate :: X509.Certificate,
    cwsSignatureAlgorithm :: SignatureAlgorithmIdentifier,
    cwsSignature :: SignatureValue,
    cwsEncoded :: B.ByteString
  } deriving stock (Show, Eq, Typeable, Generic)

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
      scCertificate      :: EECerObject, 
      scSignerInfos      :: SignerInfos
  } deriving stock (Show, Eq, Typeable, Generic)
  
  {- 
      EncapsulatedContentInfo ::= SEQUENCE {
          eContentType ContentType,
          eContent [0] EXPLICIT OCTET STRING OPTIONAL }
  -}
data EncapsulatedContentInfo a = EncapsulatedContentInfo {
      eContentType :: ContentType, 
      cContent     :: a    
  } deriving stock (Show, Eq, Ord, Typeable, Generic)
  
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
  } deriving stock (Show, Eq, Typeable, Generic)
  
newtype IssuerAndSerialNumber = IssuerAndSerialNumber T.Text 
  deriving stock (Eq, Ord, Show)
  
newtype SignerIdentifier = SignerIdentifier B.ByteString 
  deriving stock (Show, Eq, Ord, Typeable, Generic)
  
newtype ContentType = ContentType OID 
  deriving stock (Show, Eq, Ord, Typeable, Generic)
newtype CMSVersion = CMSVersion Int 
  deriving stock (Show, Eq, Ord, Typeable, Generic)

newtype DigestAlgorithmIdentifiers = DigestAlgorithmIdentifiers [OID] 
  deriving stock (Show, Eq, Ord, Typeable, Generic)

newtype SignatureAlgorithmIdentifier = SignatureAlgorithmIdentifier X509.SignatureALG  
  deriving stock (Show, Eq, Typeable, Generic)

newtype SignatureValue = SignatureValue B.ByteString 
  deriving stock (Show, Eq, Ord, Typeable, Generic)  


-- | According to https://tools.ietf.org/html/rfc5652#page-16
-- there has to be DER encoded signedAttribute set
data SignedAttributes = SignedAttributes [Attribute] B.ByteString
  deriving stock (Show, Eq, Typeable, Generic)

data Attribute = ContentTypeAttr ContentType 
            | MessageDigest B.ByteString
            | SigningTime DateTime (Maybe TimezoneOffset)
            | BinarySigningTime Integer 
            | UnknownAttribute OID [ASN1]
      deriving stock (Show, Eq, Typeable, Generic)



-- Subject Public Key Info
newtype SPKI = SPKI EncodedBase64
    deriving stock (Show, Eq, Ord, Typeable, Generic)
    deriving anyclass Serialise

newtype EncodedBase64 = EncodedBase64 B.ByteString
    deriving stock (Show, Eq, Ord, Typeable, Generic)
    deriving anyclass Serialise
    deriving newtype (Monoid, Semigroup)
  
newtype DecodedBase64 = DecodedBase64 B.ByteString
    deriving stock (Show, Eq, Ord, Typeable, Generic)
    deriving anyclass Serialise
    deriving newtype (Monoid, Semigroup)
  

newtype TaName = TaName T.Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data TA = TA {
    taName        :: TaName
  , taCertificate :: Maybe ResourceCertificate
  , taUri         :: URI
  , taSpki        :: SPKI
} 
    deriving stock (Show, Eq, Generic)
    deriving anyclass Serialise


data RsyncRepository = RsyncRepository {
    uri :: URI    
} deriving stock (Show, Eq, Ord, Typeable, Generic)

data RrdpRepository = RrdpRepository {
    uri :: URI,
    session :: Maybe (SessionId, Serial)
} deriving stock (Show, Eq, Ord, Typeable, Generic)

data Repository = 
    RsyncRepo RsyncRepository | 
    RrdpRepo RrdpRepository 
    deriving stock (Show, Eq, Ord, Typeable, Generic)

        

-- serialisation
instance Serialise Hash
instance Serialise IdentityMeta
instance (Serialise a, Serialise b) => Serialise (With a b)
instance Serialise URI
instance Serialise AKI
instance Serialise SKI
instance Serialise KI
instance Serialise Serial
instance Serialise Manifest
instance Serialise Roa
instance Serialise Gbr
instance Serialise a => Serialise (CMS a)
instance Serialise SignCRL
instance Serialise ResourceCertificate
instance Serialise RpkiObject
instance (Serialise s, Serialise r) => Serialise (WithRFC_ s r)
instance Serialise (WithRFC 'Strict_ ResourceCert)
instance Serialise (ResourceCert 'Strict_)
instance Serialise (ResourceCert 'Reconsidered_)
instance Serialise (WithRFC 'Reconsidered_ ResourceCert)


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


-- Small utility functions that don't have anywhere else to go

getSerial :: WithResourceCertificate a => a -> Serial
getSerial (getRC -> ResourceCertificate rc) = 
    Serial $ X509.certSerial $ cwsX509certificate $ withRFC rc certX509 

hexHash :: Hash -> String
hexHash (Hash bs) = show $ hex bs

toAKI :: SKI -> AKI
toAKI (SKI ki) = AKI ki

getCMSContent :: CMS a -> a
getCMSContent (CMS so) = cContent $ scEncapContentInfo $ soContent so

getEEResourceCert :: SignedObject a -> EECerObject
getEEResourceCert = scCertificate . soContent

getCertWithSignature :: WithResourceCertificate a => a -> CertificateWithSignature
getCertWithSignature (getRC -> ResourceCertificate rc) = withRFC rc certX509     

getEECert :: SignedObject a -> CertificateWithSignature
getEECert (extract . scCertificate . soContent -> ResourceCertificate rc) = withRFC rc certX509     


strictCert :: ResourceCert 'Strict_ -> ResourceCertificate
strictCert = ResourceCertificate . WithStrict_ . WithRFC

reconcideredCert :: ResourceCert 'Reconsidered_ -> ResourceCertificate
reconcideredCert = ResourceCertificate . WithReconsidered_ . WithRFC

emptyIpResources :: IpResources
emptyIpResources = IpResources $ RS.emptyIpSet 

emptyAsResources :: AsResources
emptyAsResources = AsResources $ RS.emptyRS

getMftNumber :: MftObject -> Int
getMftNumber mft = mftNumber $ getCMSContent $ extract mft

withContent :: With a b -> (a -> b -> c) -> With a c
withContent (With a b) f = With a (f a b)

withMeta :: With a b -> (a -> b -> c) -> With c b
withMeta (With a b) f = With (f a b) b

makeCrl :: URI -> AKI -> Hash -> SignCRL -> CrlObject
makeCrl u a h sc = With (IdentityMeta h (u :| [])) $ With a sc

makeCert :: URI -> Maybe AKI -> SKI -> Hash -> ResourceCertificate -> CerObject
makeCert u a s h rc = With (IdentityMeta h (u :| [])) $ With a $ With s rc

makeEECert :: AKI -> SKI -> ResourceCertificate -> EECerObject
makeEECert a s rc = With a $ With s rc
