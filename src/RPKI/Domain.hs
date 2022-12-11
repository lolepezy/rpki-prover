{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE InstanceSigs #-}


module RPKI.Domain where

import           Control.Lens 
import           Data.Generics.Product.Typed

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Short    as BSS
import           Data.Text                (Text)

import           Data.ByteString.Base16   as Hex

import           Data.Hourglass
import           Data.Foldable            as F
import           Data.Set.NonEmpty        (NESet)
import qualified Data.Set.NonEmpty        as NESet
import qualified Data.List.NonEmpty       as NonEmpty
import qualified Data.List                as List
import qualified Data.Set                 as Set
import           Data.Map.Monoidal.Strict (MonoidalMap)
import qualified Data.Map.Monoidal.Strict as MonoidalMap

import           Data.Monoid.Generic
import           Data.Tuple.Strict

import           GHC.Generics

import qualified Data.X509                as X509

import           Data.ASN1.OID
import           Data.ASN1.Types

import           Data.Set                 (Set)

import           RPKI.Resources.Resources as RS
import           RPKI.Resources.Types
import           RPKI.Time

import           RPKI.Store.Base.Serialisation
import Data.Kind (Type)

newtype PolyRFC r (rfc :: ValidationRFC) = PolyRFC r
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

data SomeRFC r = StrictRFC_ (PolyRFC r 'StrictRFC) 
               | ReconsideredRFC_ (PolyRFC r 'ReconsideredRFC) 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary               

polyRFC :: SomeRFC r -> r
polyRFC (StrictRFC_ (PolyRFC r))      = r
polyRFC (ReconsideredRFC_ (PolyRFC r)) = r

mkPolyRFC :: ValidationRFC -> r -> SomeRFC r
mkPolyRFC StrictRFC r       = StrictRFC_ (PolyRFC r) 
mkPolyRFC ReconsideredRFC r = ReconsideredRFC_ (PolyRFC r) 

newtype TypedCert c (t :: CertType) = TypedCert c
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary
    deriving newtype (WithSKI, WithRFC, WithRawResourceCertificate, WithAKI)

class OfCertType c (t :: CertType)    

data CertType = CACert | EECert | BGPCert
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

newtype Hash = Hash BSS.ShortByteString 
    deriving stock (Eq, Ord, Generic)
    deriving anyclass TheBinary

newtype URI = URI { unURI :: Text } 
    deriving stock (Eq, Ord, Generic)
    deriving anyclass TheBinary

newtype RsyncHost = RsyncHost { unRsyncHost :: Text }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

newtype RsyncPathChunk = RsyncPathChunk { unRsyncPathChunk :: Text }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary
    deriving newtype Monoid    
    deriving newtype Semigroup

data RsyncURL = RsyncURL RsyncHost [RsyncPathChunk]
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

newtype RrdpURL = RrdpURL URI
    deriving stock (Eq, Ord, Generic)
    deriving anyclass TheBinary

data RpkiURL = RsyncU !RsyncURL | RrdpU !RrdpURL
    deriving  (Eq, Ord, Generic)
    deriving anyclass TheBinary

class WithURL a where
    getURL :: a -> URI

class WithRpkiURL a where
    getRpkiURL :: a -> RpkiURL

class WithAKI a where
    getAKI :: a -> Maybe AKI

class WithLocations a where
    getLocations :: a -> Locations 

class WithHash a where
    getHash :: a -> Hash

class WithSKI a where
    getSKI :: a -> SKI

class WithRawResourceCertificate a where
    getRawCert :: a -> RawResourceCertificate

class WithRFC a where
    getRFC :: a -> ValidationRFC

instance WithURL URI where
    getURL = id

instance Show RpkiURL where
    show (RsyncU u) = show u
    show (RrdpU u) = show u 
  
instance WithURL RsyncURL where
    getURL (RsyncURL (RsyncHost host) path) = 
        URI $ "rsync://" <> host <> mconcat (map (\(RsyncPathChunk p) -> "/" <> p) path)

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
    deriving anyclass TheBinary

newtype SKI  = SKI { unSKI :: KI }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

newtype AKI  = AKI { unAKI :: KI }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

newtype SessionId = SessionId Text 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

newtype Serial = Serial Integer 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

newtype Version = Version Integer 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

newtype Locations = Locations { unLocations :: NESet RpkiURL } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary
    deriving newtype (Semigroup)

instance Show URI where
    show (URI u) = show u

instance Show RrdpURL where
    show (RrdpURL u) = show u

instance Show Hash where
    show (Hash b) = hexShow b

instance Show KI where
    show (KI b) = hexShow b

hexShow :: BSS.ShortByteString -> String
hexShow = show . Hex.encode . BSS.fromShort

-- | Domain objects

newtype CMS a = CMS { unCMS :: SignedObject a } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass TheBinary

data CrlObject = CrlObject {
        hash    :: {-# UNPACK #-} Hash,
        aki     :: {-# UNPACK #-} AKI,
        signCrl :: SignCRL
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass TheBinary

data CaCerObject = CaCerObject {
        hash        :: {-# UNPACK #-} Hash,
        ski         :: {-# UNPACK #-} SKI,
        aki         :: Maybe AKI,
        certificate :: TypedCert ResourceCertificate 'CACert
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass TheBinary

data BgpCerObject = BgpCerObject {
        hash        :: {-# UNPACK #-} Hash,
        ski         :: {-# UNPACK #-} SKI,
        aki         :: Maybe AKI,
        certificate :: TypedCert RawResourceCertificate 'BGPCert
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass TheBinary

data CMSBasedObject a = CMSBasedObject {
        hash       :: {-# UNPACK #-} Hash,
        cmsPayload :: CMS a
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass TheBinary

type MftObject = CMSBasedObject Manifest
type RoaObject = CMSBasedObject [Vrp]
type GbrObject = CMSBasedObject Gbr
type RscObject = CMSBasedObject RSC
type AspaObject = CMSBasedObject Aspa

data EECerObject = EECerObject {
        ski         :: {-# UNPACK #-} SKI,
        aki         :: {-# UNPACK #-} AKI,
        certificate :: TypedCert ResourceCertificate 'EECert
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass TheBinary   

    
data RpkiObject = CerRO CaCerObject 
                | MftRO MftObject
                | RoaRO RoaObject
                | GbrRO GbrObject
                | RscRO RscObject
                | AspaRO AspaObject
                | BgpRO BgpCerObject
                | CrlRO CrlObject
    deriving stock (Show, Eq, Generic)
    deriving anyclass TheBinary


instance WithAKI CrlObject where
    getAKI CrlObject {..} = Just aki

instance WithHash CrlObject where
    getHash CrlObject {..} = hash

instance WithAKI CaCerObject where
    getAKI CaCerObject {..} = aki

instance WithHash CaCerObject where
    getHash CaCerObject {..} = hash

instance WithSKI CaCerObject where
    getSKI CaCerObject {..} = ski
    
instance WithAKI (CMSBasedObject a) where
    getAKI CMSBasedObject {..} = getAKI $ getEEResourceCert $ unCMS cmsPayload 

instance WithHash (CMSBasedObject a) where
    getHash CMSBasedObject {..} = hash

instance WithAKI EECerObject where
    getAKI EECerObject {..} = Just aki

instance WithHash BgpCerObject where
    getHash BgpCerObject {..} = hash

instance WithSKI BgpCerObject where
    getSKI BgpCerObject {..} = ski    

instance WithAKI BgpCerObject where
    getAKI BgpCerObject {..} = aki

instance WithRawResourceCertificate CaCerObject where
    getRawCert CaCerObject {..} = getRawCert certificate

instance WithRawResourceCertificate EECerObject where
    getRawCert EECerObject {..} = getRawCert certificate

instance WithRawResourceCertificate c => WithRawResourceCertificate (SomeRFC c) where
    getRawCert = getRawCert . polyRFC

instance WithRawResourceCertificate ResourceCertificate where
    getRawCert (ResourceCertificate s) = polyRFC s

instance WithRFC (SomeRFC a) where
    getRFC (StrictRFC_ _)       = StrictRFC 
    getRFC (ReconsideredRFC_ _) = ReconsideredRFC

instance WithRFC EECerObject where
    getRFC EECerObject {..} = getRFC certificate

instance WithRFC CaCerObject where
    getRFC :: CaCerObject -> ValidationRFC
    getRFC CaCerObject {..} = getRFC certificate

instance OfCertType (TypedCert c (t :: CertType)) t
instance OfCertType CaCerObject 'CACert
instance OfCertType EECerObject 'EECert
instance OfCertType BgpCerObject 'BGPCert


instance WithAKI RpkiObject where
    getAKI (CerRO c) = getAKI c
    getAKI (MftRO c) = getAKI c
    getAKI (RoaRO c) = getAKI c
    getAKI (GbrRO c) = getAKI c
    getAKI (CrlRO c) = getAKI c
    getAKI (RscRO c) = getAKI c
    getAKI (AspaRO c) = getAKI c
    getAKI (BgpRO c) = getAKI c

instance WithHash RpkiObject where
    getHash (CerRO c) = getHash c
    getHash (MftRO c) = getHash c
    getHash (RoaRO c) = getHash c
    getHash (GbrRO c) = getHash c
    getHash (CrlRO c) = getHash c
    getHash (RscRO c) = getHash c
    getHash (AspaRO c) = getHash c
    getHash (BgpRO c) = getHash c

data Located a = Located { 
        locations :: Locations,
        payload   :: a
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass TheBinary


instance WithLocations (Located a) where
    getLocations Located {..} = locations

instance WithAKI a => WithAKI (Located a) where
    getAKI (Located _ o) = getAKI o    

instance WithHash a => WithHash (Located a) where
    getHash (Located _ o) = getHash o

instance WithSKI a => WithSKI (Located a) where
    getSKI (Located _ o) = getSKI o

instance WithRawResourceCertificate a => WithRawResourceCertificate (Located a) where    
    getRawCert (Located _ o) = getRawCert o

instance WithRFC a => WithRFC (Located a) where    
    getRFC (Located _ o) = getRFC o

instance OfCertType c t => OfCertType (Located c) t

-- More concrete data structures for resource certificates, CRLs, MFTs, ROAs

data RawResourceCertificate = RawResourceCertificate {
        certX509  :: CertificateWithSignature, 
        resources :: AllResources
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass TheBinary

newtype ResourceCertificate = ResourceCertificate (SomeRFC RawResourceCertificate)
    deriving stock (Show, Eq, Generic)
    deriving anyclass TheBinary
    deriving newtype (WithRFC)

data Vrp = Vrp 
    {-# UNPACK #-} !ASN 
    !IpPrefix 
    {-# UNPACK #-} !PrefixLength
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

data Manifest = Manifest {
        mftNumber   :: Serial, 
        fileHashAlg :: X509.HashALG, 
        thisTime    :: Instant, 
        nextTime    :: Instant, 
        mftEntries  :: [T2 Text Hash]
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass TheBinary

data SignCRL = SignCRL {
        thisUpdateTime     :: Instant,
        nextUpdateTime     :: Maybe Instant,
        signatureAlgorithm :: SignatureAlgorithmIdentifier,
        signatureValue     :: SignatureValue,
        encodedValue       :: BSS.ShortByteString,
        crlNumber          :: Serial,
        revokenSerials     :: Set Serial
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass TheBinary


data Gbr = Gbr BSS.ShortByteString
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary


data RSC = RSC {        
        rscResources    :: PrefixesAndAsns,        
        checkList       :: [T2 (Maybe Text) Hash],
        digestAlgorithm :: DigestAlgorithmIdentifier
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass TheBinary

-- https://datatracker.ietf.org/doc/draft-ietf-sidrops-aspa-profile/
data Aspa = Aspa {                
        customerAsn  :: ASN,
        providerAsns :: [(ASN, Maybe AddrFamily)]
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

data BGPCertPayload = BGPCertPayload {
        ski  :: SKI,
        asn  :: [ASN],
        spki :: SPKI
        -- TODO Possible store the hash of the original BGP certificate?
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

-- | Types for the signed object template 
-- https://tools.ietf.org/html/rfc5652

data SignedObject a = SignedObject {
        soContentType :: ContentType, 
        soContent     :: SignedData a
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass TheBinary


data CertificateWithSignature = CertificateWithSignature {
        cwsX509certificate    :: X509.Certificate,
        cwsSignatureAlgorithm :: SignatureAlgorithmIdentifier,
        cwsSignature          :: SignatureValue,
        cwsEncoded            :: BSS.ShortByteString
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass TheBinary

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
    deriving anyclass TheBinary

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
    deriving anyclass TheBinary

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
    deriving anyclass TheBinary

newtype IssuerAndSerialNumber = IssuerAndSerialNumber Text 
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass TheBinary

newtype SignerIdentifier = SignerIdentifier BSS.ShortByteString 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

newtype ContentType = ContentType OID 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

newtype CMSVersion = CMSVersion Int 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

newtype DigestAlgorithmIdentifiers = DigestAlgorithmIdentifiers [OID] 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

newtype DigestAlgorithmIdentifier = DigestAlgorithmIdentifier OID
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

newtype SignatureAlgorithmIdentifier = SignatureAlgorithmIdentifier X509.SignatureALG  
    deriving stock (Show, Eq, Generic)
    deriving anyclass TheBinary

newtype SignatureValue = SignatureValue BSS.ShortByteString 
    deriving stock (Show, Eq, Ord, Generic)  
    deriving anyclass TheBinary


-- | According to https://tools.ietf.org/html/rfc5652#page-16
-- there has to be DER encoded signedAttribute set
data SignedAttributes = SignedAttributes [Attribute] BSS.ShortByteString
    deriving stock (Show, Eq, Generic)
    deriving anyclass TheBinary

data Attribute = ContentTypeAttr ContentType 
            | MessageDigest BSS.ShortByteString
            | SigningTime DateTime (Maybe TimezoneOffset)
            | BinarySigningTime Integer 
            | UnknownAttribute OID [ASN1]
    deriving stock (Show, Eq, Generic)
    deriving anyclass TheBinary


-- Subject Public Key Info
newtype SPKI = SPKI EncodedBase64
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

newtype EncodedBase64 = EncodedBase64 BS.ByteString
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary
    deriving newtype (Monoid, Semigroup)

newtype DecodedBase64 = DecodedBase64 BS.ByteString
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary
    deriving newtype (Monoid, Semigroup)

newtype TaName = TaName { unTaName :: Text }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass TheBinary

instance Show TaName where
    show = show . unTaName

newtype Vrps = Vrps { unVrps :: MonoidalMap TaName (Set Vrp) }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary
    deriving Semigroup via GenericSemigroup Vrps
    deriving Monoid    via GenericMonoid Vrps

data TA = TA {
        taName        :: TaName, 
        taCertificate :: Maybe ResourceCertificate,
        taUri         :: URI,
        taSpki        :: SPKI
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass TheBinary
  

-- Small utility functions that don't have anywhere else to go

getSerial :: WithRawResourceCertificate a => a -> Serial
getSerial c = 
    Serial $ X509.certSerial $ cwsX509certificate $ certX509 $ getRawCert c

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

getCertWithSignature :: WithRawResourceCertificate a => a -> CertificateWithSignature
getCertWithSignature c = certX509 $ getRawCert c 

getEECert :: SignedObject a -> CertificateWithSignature
getEECert = certX509 . getRawCert . scCertificate . soContent

strictCert :: RawResourceCertificate -> ResourceCertificate
strictCert = ResourceCertificate . StrictRFC_ . PolyRFC

reconsideredCert :: RawResourceCertificate -> ResourceCertificate
reconsideredCert = ResourceCertificate . ReconsideredRFC_ . PolyRFC

emptyIpResources :: IpResources
emptyIpResources = IpResources RS.emptyIpSet 

emptyAsResources :: AsResources
emptyAsResources = AsResources RS.emptyRS

newCrl :: AKI -> Hash -> SignCRL -> CrlObject
newCrl a h sc = CrlObject {
        hash = h,    
        aki = a,
        signCrl = sc
    } 

-- newCaCert :: Maybe AKI -> SKI -> Hash -> RawResourceCertificate -> CaCerObject
-- newCaCert a s h rc = CaCerObject {
--         hash = h,    
--         ski = s,
--         aki = a,
--         certificate = rc
--     } 

-- newEECert :: AKI -> SKI -> ResourceCertificate -> EECerObject
-- newEECert a s rc = EECerObject {
--         ski = s,
--         aki = a,
--         certificate = rc
--     }

newCMSObject :: Hash -> CMS a -> CMSBasedObject a
newCMSObject h cms = CMSBasedObject {
        hash = h,    
        -- locations = loc,
        cmsPayload = cms
    }

toShortBS :: BS.ByteString -> BSS.ShortByteString
toShortBS = BSS.toShort

toNormalBS :: BSS.ShortByteString -> BS.ByteString
toNormalBS = BSS.fromShort

toLocations :: RpkiURL -> Locations
toLocations = Locations . NESet.singleton

pickLocation :: Locations -> RpkiURL
pickLocation = NonEmpty.head . sortRrdpFirstNE . NESet.toList . unLocations

locationsToText :: Locations -> Text
locationsToText = F.fold
    . NonEmpty.intersperse ", " 
    . NonEmpty.map (unURI . getURL) 
    . sortRrdpFirstNE
    . NESet.toList 
    . unLocations

toNESet :: Ord a => [a] -> Maybe (NESet a)
toNESet = (NESet.fromList <$>) . NonEmpty.nonEmpty

neSetToList :: NESet a -> [a]
neSetToList = NonEmpty.toList . NESet.toList

sortRrdpFirst :: [RpkiURL] -> [RpkiURL]
sortRrdpFirst = List.sortBy $ \u1 u2 -> 
    case (u1, u2) of 
        (RrdpU _, RsyncU _) -> LT
        (RsyncU _, RrdpU _) -> GT
        (r1, r2)            -> compare r1 r2        

sortRrdpFirstNE :: NonEmpty.NonEmpty RpkiURL -> NonEmpty.NonEmpty RpkiURL
sortRrdpFirstNE = NonEmpty.fromList . sortRrdpFirst . NonEmpty.toList

{- 
https://datatracker.ietf.org/doc/html/rfc5280#section-4.1.2.2
https://datatracker.ietf.org/doc/html/rfc6486#section-4.2.1
and probably others.

Serials in objects (CRL and MFT numbers, etc.) are limited to 20 octets, i.e. 160 bits.
-} 
maxSerial :: Integer
maxSerial = (2 :: Integer) ^ (160  :: Integer) - (1  :: Integer)

makeSerial :: Integer -> Either String Serial 
makeSerial i = 
    case () of
        _ | i <= 0         -> Left $ "Serial is not positive: " <> show i
          | i >= maxSerial -> Left $ "Serial is too big: " <> show i
          | otherwise      -> Right $ Serial i


estimateVrpCount :: Vrps -> Int 
estimateVrpCount (Vrps vrps) = sum $ map Set.size $ MonoidalMap.elems vrps

-- Precise but much more expensive
uniqueVrpCount :: Vrps -> Int 
uniqueVrpCount = Set.size . allVrps

newVrps :: TaName -> Set Vrp -> Vrps
newVrps taName vrpSet = Vrps $ MonoidalMap.singleton taName vrpSet

allVrps :: Vrps -> Set Vrp 
allVrps (Vrps vrps) = mconcat $ MonoidalMap.elems vrps          