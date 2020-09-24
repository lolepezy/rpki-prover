{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE UndecidableInstances       #-}


module RPKI.Domain where

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Short    as BSS
import           Data.Text                (Text)
import qualified Data.Text                as Text

import           Codec.Serialise
import           Data.Hex
import           Data.Int

import           Data.Hourglass
import           Data.Kind                (Type)
import           Data.List.NonEmpty

import           GHC.Generics

import qualified Data.X509                as X509

import           Data.ASN1.OID
import           Data.ASN1.Types

import           Data.Set             (Set)

import           RPKI.Resources.Resources as RS
import           RPKI.Resources.Types
import           RPKI.Time


newtype WithRFC (rfc :: ValidationRFC) (r :: ValidationRFC -> Type) = WithRFC (r rfc)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

type AnRFC r = WithRFC_ (WithRFC 'Strict_ r) (WithRFC 'Reconsidered_ r)

data WithRFC_ s r = WithStrict_       !s 
                  | WithReconsidered_ !r
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

withRFC :: AnRFC r -> (forall rfc . r rfc -> a) -> a
withRFC (WithStrict_ (WithRFC a)) f = f a
withRFC (WithReconsidered_ (WithRFC a)) f = f a  

forRFC :: AnRFC r -> (r 'Strict_ -> a) -> (r 'Reconsidered_ -> a) -> a
forRFC (WithStrict_ (WithRFC a))       f _ = f a
forRFC (WithReconsidered_ (WithRFC a)) _ g = g a  

newtype Hash = Hash BSS.ShortByteString 
    deriving stock (Eq, Ord, Generic)
    deriving anyclass Serialise

newtype URI  = URI { unURI :: Text } 
    deriving stock (Eq, Ord, Generic)
    deriving anyclass Serialise

newtype RsyncURL  = RsyncURL URI
    deriving stock (Eq, Ord, Generic)
    deriving anyclass Serialise

newtype RrdpURL  = RrdpURL URI
    deriving stock (Eq, Ord, Generic)
    deriving anyclass Serialise

data RpkiURL = RsyncU !RsyncURL | RrdpU !RrdpURL
    deriving  (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

class WithURL a where
    getURL :: a -> URI

class WithRpkiURL a where
    getRpkiURL :: a -> RpkiURL

instance WithURL URI where
    getURL = id

instance WithURL RsyncURL where
    getURL (RsyncURL u) = u

instance WithURL RrdpURL where
    getURL (RrdpURL u) = u

instance WithURL RpkiURL where
    getURL (RsyncU u) = getURL u
    getURL (RrdpU u) = getURL u    

instance WithRpkiURL RpkiURL where
    getRpkiURL = id

toText :: RpkiURL -> Text
toText = unURI . getURL 

newtype KI = KI BSS.ShortByteString 
    deriving stock (Eq, Ord, Generic)
    deriving anyclass Serialise

newtype SKI  = SKI KI 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype AKI  = AKI KI   
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype SessionId = SessionId BSS.ShortByteString 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype Serial = Serial Integer 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype Version = Version Integer 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

type Locations = NonEmpty RpkiURL

instance Show URI where
    show (URI u) = show u

instance Show RsyncURL where
    show (RsyncURL u) = show u

instance Show RrdpURL where
    show (RrdpURL u) = show u

instance Show Hash where
    show (Hash b) = hexShow b

instance Show KI where
    show (KI b) = hexShow b

hexShow :: BSS.ShortByteString -> String
hexShow = show . hex . BSS.fromShort

-- | Domain objects

newtype CMS a = CMS { unCMS :: SignedObject a } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass Serialise

class WithAKI a where
    getAKI :: a -> Maybe AKI

class WithLocations a where
    getLocations :: a -> Locations 

class WithHash a where
    getHash :: a -> Hash

class WithSKI a where
    getSKI :: a -> SKI

class WithResourceCertificate a where
    getRC :: a -> ResourceCertificate


data CrlObject = CrlObject {
        hash      :: {-# UNPACK #-} Hash,
        locations :: {-# UNPACK #-} Locations,
        aki :: {-# UNPACK #-} AKI,
        signCrl :: SignCRL
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass Serialise

data CerObject = CerObject {
        hash      :: {-# UNPACK #-} Hash,
        locations :: {-# UNPACK #-} Locations,
        ski :: SKI,
        aki :: Maybe AKI,
        certificate :: ResourceCertificate
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass Serialise

data CMSBasedObject a = CMSBasedObject {
        hash      :: {-# UNPACK #-} Hash,
        locations :: {-# UNPACK #-} Locations,
        cmsPayload :: CMS a
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass Serialise

type MftObject = CMSBasedObject Manifest
type RoaObject = CMSBasedObject [Vrp]
type GbrObject = CMSBasedObject Gbr

data EECerObject = EECerObject {
        ski :: SKI,
        aki :: AKI,
        certificate :: ResourceCertificate
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass Serialise   

    
data RpkiObject = CerRO CerObject 
                | MftRO MftObject
                | RoaRO RoaObject
                | GbrRO GbrObject
                | CrlRO CrlObject
    deriving stock (Show, Eq, Generic)
    deriving anyclass Serialise


instance WithAKI CrlObject where
    getAKI CrlObject {..} = Just aki

instance WithLocations CrlObject where
    getLocations CrlObject {..} = locations

instance WithHash CrlObject where
    getHash CrlObject {..} = hash

instance WithAKI CerObject where
    getAKI CerObject {..} = aki

instance WithLocations CerObject where
    getLocations CerObject {..} = locations

instance WithHash CerObject where
    getHash CerObject {..} = hash

instance WithSKI CerObject where
    getSKI CerObject {..} = ski
    
instance WithAKI (CMSBasedObject a) where
    getAKI CMSBasedObject {..} = getAKI $ getEEResourceCert $ unCMS cmsPayload 

instance WithLocations (CMSBasedObject a) where
    getLocations CMSBasedObject {..} = locations

instance WithHash (CMSBasedObject a) where
    getHash CMSBasedObject {..} = hash

instance WithAKI EECerObject where
    getAKI EECerObject {..} = Just aki

instance WithSKI EECerObject where
    getSKI EECerObject {..} = ski

instance WithResourceCertificate CerObject where
    getRC = certificate

instance WithResourceCertificate EECerObject where
    getRC = certificate


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
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass Serialise

newtype ResourceCertificate = ResourceCertificate (AnRFC ResourceCert)
    deriving stock (Show, Eq, Generic)
    deriving anyclass Serialise

data Vrp = Vrp 
    {-# UNPACK #-} !ASN 
    !IpPrefix 
    {-# UNPACK #-} !Int16
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data PrefixWithLength = PrefixWithLength !IpPrefix !Int16
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data Manifest = Manifest {
        mftNumber   :: Int, 
        fileHashAlg :: X509.HashALG, 
        thisTime    :: Instant, 
        nextTime    :: Instant, 
        mftEntries  :: [(Text, Hash)]
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass Serialise

data SignCRL = SignCRL {
        thisUpdateTime     :: Instant,
        nextUpdateTime     :: Maybe Instant,
        signatureAlgorithm :: SignatureAlgorithmIdentifier,
        signatureValue     :: SignatureValue,
        encodedValue       :: BSS.ShortByteString,
        crlNumber          :: Integer,
        revokenSerials     :: Set Serial
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass Serialise

-- TODO Define it
data Gbr = Gbr 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise



-- | Types for the signed object template 
-- https://tools.ietf.org/html/rfc5652

data SignedObject a = SignedObject {
        soContentType :: ContentType, 
        soContent     :: SignedData a
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass Serialise


data CertificateWithSignature = CertificateWithSignature {
        cwsX509certificate    :: X509.Certificate,
        cwsSignatureAlgorithm :: SignatureAlgorithmIdentifier,
        cwsSignature          :: SignatureValue,
        cwsEncoded            :: BSS.ShortByteString
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass Serialise

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
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass Serialise

{- 
    EncapsulatedContentInfo ::= SEQUENCE {
        eContentType ContentType,
        eContent [0] EXPLICIT OCTET STRING OPTIONAL }
-}
data EncapsulatedContentInfo a = EncapsulatedContentInfo {
        eContentType :: ContentType, 
        cContent     :: a    
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

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
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass Serialise

newtype IssuerAndSerialNumber = IssuerAndSerialNumber Text 
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass Serialise

newtype SignerIdentifier = SignerIdentifier BSS.ShortByteString 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype ContentType = ContentType OID 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype CMSVersion = CMSVersion Int 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype DigestAlgorithmIdentifiers = DigestAlgorithmIdentifiers [OID] 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype SignatureAlgorithmIdentifier = SignatureAlgorithmIdentifier X509.SignatureALG  
    deriving stock (Show, Eq, Generic)
    deriving anyclass Serialise

newtype SignatureValue = SignatureValue BSS.ShortByteString 
    deriving stock (Show, Eq, Ord, Generic)  
    deriving anyclass Serialise


-- | According to https://tools.ietf.org/html/rfc5652#page-16
-- there has to be DER encoded signedAttribute set
data SignedAttributes = SignedAttributes [Attribute] BSS.ShortByteString
    deriving stock (Show, Eq, Generic)
    deriving anyclass Serialise

data Attribute = ContentTypeAttr ContentType 
            | MessageDigest BSS.ShortByteString
            | SigningTime DateTime (Maybe TimezoneOffset)
            | BinarySigningTime Integer 
            | UnknownAttribute OID [ASN1]
    deriving stock (Show, Eq, Generic)
    deriving anyclass Serialise


-- Subject Public Key Info
newtype SPKI = SPKI EncodedBase64
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype EncodedBase64 = EncodedBase64 BS.ByteString
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving newtype (Monoid, Semigroup)

newtype DecodedBase64 = DecodedBase64 BS.ByteString
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving newtype (Monoid, Semigroup)

newtype TaName = TaName { unTaName :: Text }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data TA = TA {
        taName        :: TaName, 
        taCertificate :: Maybe ResourceCertificate,
        taUri         :: URI,
        taSpki        :: SPKI
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass Serialise
  

-- Small utility functions that don't have anywhere else to go

getSerial :: WithResourceCertificate a => a -> Serial
getSerial (getRC -> ResourceCertificate rc) = 
    Serial $ X509.certSerial $ cwsX509certificate $ withRFC rc certX509 

toAKI :: SKI -> AKI
toAKI (SKI ki) = AKI ki

mkKI :: BS.ByteString -> KI
mkKI = KI . BSS.toShort

skiLen :: SKI -> Int
skiLen (SKI (KI bs)) = BSS.length bs

getCMSContent :: CMS a -> a
getCMSContent (CMS so) = cContent $ scEncapContentInfo $ soContent so

getEEResourceCert :: SignedObject a -> EECerObject
getEEResourceCert = scCertificate . soContent

getCertWithSignature :: WithResourceCertificate a => a -> CertificateWithSignature
getCertWithSignature (getRC -> ResourceCertificate rc) = withRFC rc certX509     

getEECert :: SignedObject a -> CertificateWithSignature
getEECert (getRC . scCertificate . soContent -> ResourceCertificate rc) = withRFC rc certX509     

strictCert :: ResourceCert 'Strict_ -> ResourceCertificate
strictCert = ResourceCertificate . WithStrict_ . WithRFC

reconsideredCert :: ResourceCert 'Reconsidered_ -> ResourceCertificate
reconsideredCert = ResourceCertificate . WithReconsidered_ . WithRFC

emptyIpResources :: IpResources
emptyIpResources = IpResources RS.emptyIpSet 

emptyAsResources :: AsResources
emptyAsResources = AsResources RS.emptyRS

getMftNumber :: MftObject -> Int
getMftNumber mft = mftNumber $ getCMSContent $ cmsPayload mft

-- withContent :: With a b -> (a -> b -> c) -> With a c
-- withContent (With a b) f = With a (f a b)

-- withMeta :: With a b -> (a -> b -> c) -> With c b
-- withMeta (With a b) f = With (f a b) b

newCrl :: RpkiURL -> AKI -> Hash -> SignCRL -> CrlObject
newCrl u a h sc = CrlObject {
        hash = h,    
        locations = u :| [],
        aki = a,
        signCrl = sc
    } 

newCert :: RpkiURL -> Maybe AKI -> SKI -> Hash -> ResourceCertificate -> CerObject
newCert u a s h rc = CerObject {
        hash = h,    
        locations = u :| [],
        ski = s,
        aki = a,
        certificate = rc
    } 

newEECert :: AKI -> SKI -> ResourceCertificate -> EECerObject
newEECert a s rc = EECerObject {
        ski = s,
        aki = a,
        certificate = rc
    }

newCMSObject :: Hash -> Locations -> CMS a -> CMSBasedObject a
newCMSObject h loc cms = CMSBasedObject {
        hash = h,    
        locations = loc,
        cmsPayload = cms
    }

toShortBS :: BS.ByteString -> BSS.ShortByteString
toShortBS = BSS.toShort

toNormalBS :: BSS.ShortByteString -> BS.ByteString
toNormalBS = BSS.fromShort