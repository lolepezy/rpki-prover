{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DerivingVia                #-}

module RPKI.Domain where

import           Control.DeepSeq          (NFData)
import           Data.Int                 (Int64)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Short    as BSS
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Vector              as V

import           Data.ByteString.Base16   as Hex
import qualified Data.String.Conversions  as SC

import           Data.Hourglass
import           Data.Data
import           Data.Foldable            as F
import           Data.Set.NonEmpty        (NESet)
import qualified Data.Set.NonEmpty        as NESet
import qualified Data.List.NonEmpty       as NonEmpty
import qualified Data.List                as List
import qualified Data.Set                 as Set
import           Data.Map.Monoidal.Strict (MonoidalMap)
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import           Data.Hashable hiding (hash)
import           Data.Semigroup

import           Data.Bifunctor
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
import           RPKI.AppTypes (WorldVersion)


-- There are two validation algorithms for RPKI tree
--
-- Classical one described in RFC 6487, here referred as Strict
-- And the one described in RFC 8360, here (and in the RFC itself) referred as Reconsidered
-- 
data ValidationRFC = StrictRFC | ReconsideredRFC
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass (TheBinary, NFData)

newtype TypedCert c (t :: CertType) = TypedCert c
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)
    deriving newtype (WithSKI, WithRawResourceCertificate, WithAKI)

class OfCertType c (t :: CertType)    

data CertType = CACert | EECert | BGPCert
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

newtype Hash = Hash BSS.ShortByteString 
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

newtype URI = URI { unURI :: Text } 
    deriving stock (Eq, Ord, Generic, Data, Typeable)
    deriving anyclass (TheBinary, NFData)
    deriving anyclass Hashable

data RsyncHost = RsyncHost RsyncHostName (Maybe RsyncPort)
    deriving stock (Show, Eq, Ord, Generic, Data, Typeable)
    deriving anyclass (TheBinary, NFData)
    deriving anyclass Hashable

newtype RsyncHostName = RsyncHostName { unRsyncHostName :: Text }
    deriving stock (Show, Eq, Ord, Generic, Data, Typeable)
    deriving anyclass (TheBinary, NFData)
    deriving anyclass Hashable

newtype RsyncPort = RsyncPort { unRsyncPort :: Int }
    deriving stock (Eq, Ord, Generic, Data, Typeable)
    deriving anyclass (TheBinary, NFData)
    deriving anyclass Hashable

newtype RsyncPathChunk = RsyncPathChunk { unRsyncPathChunk :: Text }
    deriving stock (Show, Eq, Ord, Generic, Data, Typeable)
    deriving anyclass (TheBinary, NFData)
    deriving newtype Monoid    
    deriving newtype Semigroup
    deriving anyclass Hashable

data RsyncURL = RsyncURL RsyncHost [RsyncPathChunk]
    deriving stock (Eq, Ord, Generic, Data, Typeable)
    deriving anyclass (TheBinary, NFData)
    deriving anyclass Hashable

newtype RrdpURL = RrdpURL URI
    deriving stock (Eq, Ord, Generic, Data, Typeable)
    deriving anyclass (TheBinary, NFData)
    deriving anyclass Hashable

data RpkiURL = RsyncU !RsyncURL | RrdpU !RrdpURL
    deriving stock (Eq, Ord, Generic, Data, Typeable)
    deriving anyclass (TheBinary, NFData)
    deriving anyclass Hashable

class WithValidityPeriod a where
    getValidityPeriod :: a -> (Instant, Instant)

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

class WithSerial a where
    getSerial :: a -> Serial

class WithRpkiObjectType a where
    getRpkiObjectType :: a -> RpkiObjectType

instance {-# OVERLAPPING #-} WithURL URI where
    getURL = id

instance Show RpkiURL where
    show (RsyncU u) = show u
    show (RrdpU u) = show u 

instance Show RsyncURL where
    show = show . getURL

instance Show RsyncPort where
    show = show . unRsyncPort
  
instance {-# OVERLAPPING #-} WithURL RsyncURL where
    getURL (RsyncURL (RsyncHost (RsyncHostName host) port) path) = 
        URI $ "rsync://" <> 
                host <>          
                maybe "" (\p -> ":" <> Text.pack (show p)) port <>
                mconcat (map (\(RsyncPathChunk p) -> "/" <> p) path)

instance {-# OVERLAPPING #-} WithURL RrdpURL where
    getURL (RrdpURL u) = u

instance {-# OVERLAPPING #-} WithURL RpkiURL where
    getURL (RsyncU u) = getURL u
    getURL (RrdpU u) = getURL u    

instance {-# OVERLAPPING #-} WithRpkiURL RpkiURL where
    getRpkiURL = id

instance {-# OVERLAPPING #-} WithRpkiURL u => WithURL u where
    getURL = getURL . getRpkiURL 

toText :: RpkiURL -> Text
toText = unURI . getURL 

newtype KI = KI BSS.ShortByteString 
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

newtype SKI  = SKI { unSKI :: KI }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

newtype AKI  = AKI { unAKI :: KI }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

newtype SessionId = SessionId { unSessionId :: Text }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

instance Show SessionId where
    show (SessionId s) = show s

newtype Serial = Serial Integer     
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

newtype Version = Version Integer 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

newtype Locations = Locations { unLocations :: NESet RpkiURL } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)
    deriving newtype (Semigroup)

instance Show Serial where
    show (Serial u) = show u

instance Show URI where
    show (URI u) = show u

instance Show RrdpURL where
    show (RrdpURL u) = show u

instance Show Hash where
    show (Hash b) = hexShow b

instance Show KI where
    show (KI b) = hexShow b

instance {-# OVERLAPPING #-} WithSerial Serial where
    getSerial = id

hexShow :: BSS.ShortByteString -> String
hexShow = SC.cs . Hex.encode . BSS.fromShort

-- | Domain objects

newtype CMS a = CMS { unCMS :: SignedObject a } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)

data CrlObject = CrlObject {
        hash    :: {-# UNPACK #-} Hash,
        aki     :: {-# UNPACK #-} AKI,
        signCrl :: SignCRL
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)

data CaCerObject = CaCerObject {
        hash        :: {-# UNPACK #-} Hash,
        ski         :: {-# UNPACK #-} SKI,
        aki         :: Maybe AKI,
        certificate :: TypedCert ResourceCertificate 'CACert
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)

data EECerObject = EECerObject {
        ski         :: {-# UNPACK #-} SKI,
        aki         :: {-# UNPACK #-} AKI,
        certificate :: TypedCert ResourceCertificate 'EECert
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)   

data BgpCerObject = BgpCerObject {
        hash        :: {-# UNPACK #-} Hash,
        ski         :: {-# UNPACK #-} SKI,
        aki         :: Maybe AKI,
        certificate :: TypedCert RawResourceCertificate 'BGPCert
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)

data CMSBasedObject a = CMSBasedObject {
        hash       :: {-# UNPACK #-} Hash,
        cmsPayload :: CMS a
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)

-- https://datatracker.ietf.org/doc/rfc9286/
type MftObject = CMSBasedObject Manifest

-- https://datatracker.ietf.org/doc/rfc6482
type RoaObject = CMSBasedObject [Vrp]

-- https://datatracker.ietf.org/doc/draft-ietf-sidrops-rpki-prefixlist
type SplObject = CMSBasedObject SplPayload

-- https://datatracker.ietf.org/doc/rfc6493
type GbrObject = CMSBasedObject Gbr

-- https://datatracker.ietf.org/doc/draft-ietf-sidrops-rpki-rsc/
type RscObject = CMSBasedObject Rsc

-- https://datatracker.ietf.org/doc/draft-ietf-sidrops-aspa-profile/
type AspaObject = CMSBasedObject Aspa

    
data RpkiObject = CerRO CaCerObject 
                | MftRO MftObject
                | RoaRO RoaObject
                | SplRO SplObject
                | GbrRO GbrObject
                | RscRO RscObject
                | AspaRO AspaObject
                | BgpRO BgpCerObject
                | CrlRO CrlObject
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary) 

data RpkiObjectType = CER | MFT | CRL | ROA | ASPA | GBR | SPL | BGPSec | RSC
    deriving (Show, Eq, Ord, Generic)    
    deriving anyclass (TheBinary, NFData)

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

instance {-# OVERLAPPING #-} WithValidityPeriod (CMSBasedObject a) where
    getValidityPeriod CMSBasedObject {..} = 
        bimap newInstant newInstant $ X509.certValidity 
            $ cwsX509certificate $ getCertWithSignature 
            $ getEEResourceCert $ unCMS cmsPayload 

instance {-# OVERLAPPING #-} WithSerial (CMSBasedObject a) where
    getSerial CMSBasedObject {..} = 
        Serial $ X509.certSerial $ cwsX509certificate $ getCertWithSignature 
            $ getEEResourceCert $ unCMS cmsPayload 

instance WithRawResourceCertificate (CMSBasedObject a) where
    getRawCert CMSBasedObject {..} = getRawCert $ getEEResourceCert $ unCMS cmsPayload 

instance WithAKI EECerObject where
    getAKI EECerObject {..} = Just aki

instance WithHash BgpCerObject where
    getHash BgpCerObject {..} = hash

instance WithSKI BgpCerObject where
    getSKI BgpCerObject {..} = ski    

instance WithAKI BgpCerObject where
    getAKI BgpCerObject {..} = aki

instance WithSKI EECerObject where
    getSKI EECerObject {..} = ski

instance WithSKI (CMSBasedObject a) where    
    getSKI CMSBasedObject {..} = getSKI $ getEEResourceCert $ unCMS cmsPayload 

instance WithRawResourceCertificate a => WithValidityPeriod a where
    getValidityPeriod cert = 
        bimap newInstant newInstant $ X509.certValidity 
            $ cwsX509certificate $ getCertWithSignature $ getRawCert cert

instance {-# OVERLAPPING #-} WithRawResourceCertificate a => WithSerial a where
    getSerial = Serial . X509.certSerial . cwsX509certificate . certX509 . getRawCert

instance WithRawResourceCertificate CaCerObject where
    getRawCert CaCerObject {..} = getRawCert certificate

instance WithRawResourceCertificate EECerObject where
    getRawCert EECerObject {..} = getRawCert certificate

instance WithRawResourceCertificate BgpCerObject where
    getRawCert BgpCerObject {..} = getRawCert certificate

instance WithRawResourceCertificate RawResourceCertificate where
    getRawCert = id

instance WithRawResourceCertificate ResourceCertificate where
    getRawCert (ResourceCertificate s) = s

instance OfCertType (TypedCert c (t :: CertType)) t
instance OfCertType CaCerObject 'CACert
instance OfCertType EECerObject 'EECert
instance OfCertType BgpCerObject 'BGPCert

instance WithAKI RpkiObject where
    getAKI (CerRO c) = getAKI c
    getAKI (MftRO c) = getAKI c
    getAKI (RoaRO c) = getAKI c
    getAKI (SplRO c) = getAKI c
    getAKI (GbrRO c) = getAKI c
    getAKI (CrlRO c) = getAKI c
    getAKI (RscRO c) = getAKI c
    getAKI (AspaRO c) = getAKI c
    getAKI (BgpRO c)  = getAKI c

instance WithHash RpkiObject where
    getHash (CerRO c) = getHash c
    getHash (MftRO c) = getHash c
    getHash (RoaRO c) = getHash c
    getHash (SplRO c) = getHash c
    getHash (GbrRO c) = getHash c
    getHash (CrlRO c) = getHash c
    getHash (RscRO c) = getHash c
    getHash (AspaRO c) = getHash c
    getHash (BgpRO c) = getHash c

instance WithRpkiObjectType RpkiObject where
    getRpkiObjectType = \case 
        CerRO _ -> CER
        MftRO _ -> MFT
        RoaRO _ -> ROA
        SplRO _ -> SPL
        GbrRO _ -> GBR
        CrlRO _ -> CRL
        RscRO _ -> RSC
        AspaRO _ -> ASPA
        BgpRO _ -> BGPSec
        

data Located a = Located { 
        locations :: Locations,        
        payload   :: a
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)


instance WithLocations (Located a) where
    getLocations Located {..} = locations

instance WithLocations Locations where
    getLocations = id

instance WithAKI a => WithAKI (Located a) where
    getAKI (Located _ o) = getAKI o    

instance WithHash a => WithHash (Located a) where
    getHash (Located _ o) = getHash o

instance WithSKI a => WithSKI (Located a) where
    getSKI (Located _ o) = getSKI o

instance WithRawResourceCertificate a => WithRawResourceCertificate (Located a) where    
    getRawCert (Located _ o) = getRawCert o

instance WithRpkiObjectType a => WithRpkiObjectType (Located a) where    
    getRpkiObjectType (Located _ o) = getRpkiObjectType o

instance OfCertType c t => OfCertType (Located c) t


-- More concrete data structures for resource certificates, CRLs, MFTs, ROAs

data RawResourceCertificate = RawResourceCertificate {
        certX509  :: CertificateWithSignature, 
        resources :: AllResources
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)

newtype ResourceCertificate = ResourceCertificate RawResourceCertificate
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)

data Vrp = Vrp 
    {-# UNPACK #-} ASN 
    IpPrefix 
    {-# UNPACK #-} PrefixLength
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

-- Signed Prefix List normalised payload
data SplN = SplN {-# UNPACK #-} ASN IpPrefix
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

data SplPayload = SplPayload {-# UNPACK #-} ASN [IpPrefix]     
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data MftPair = MftPair {
        fileName :: Text,
        hash     :: {-# UNPACK #-} Hash
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

data Manifest = Manifest {
        mftNumber   :: {-# UNPACK #-} Serial, 
        fileHashAlg :: X509.HashALG, 
        thisTime    :: {-# UNPACK #-} Instant, 
        nextTime    :: {-# UNPACK #-} Instant, 
        mftEntries  :: [MftPair]
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)

data SignCRL = SignCRL {
        thisUpdateTime     :: {-# UNPACK #-} Instant,
        nextUpdateTime     :: Maybe Instant,
        signatureAlgorithm :: SignatureAlgorithmIdentifier,
        signatureValue     :: SignatureValue,
        encodedValue       :: BSS.ShortByteString,
        crlNumber          :: {-# UNPACK #-} Serial,
        revokedSerials     :: Set Serial
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)


data Gbr = Gbr BSS.ShortByteString
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)


data Rsc = Rsc {        
        rscResources    :: PrefixesAndAsns,        
        checkList       :: [T2 (Maybe Text) Hash],
        digestAlgorithm :: DigestAlgorithmIdentifier
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary, NFData)

-- https://datatracker.ietf.org/doc/draft-ietf-sidrops-aspa-profile/
data Aspa = Aspa {                
        customer  :: {-# UNPACK #-} ASN,
        providers :: Set ASN
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

data BGPSecPayload = BGPSecPayload {
        bgpSecSki  :: {-# UNPACK #-} SKI,
        bgpSecAsns :: [ASN],
        bgpSecSpki :: {-# UNPACK #-} SPKI
        -- TODO Possible store the hash of the original BGP certificate?
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)


data CertificateWithSignature = CertificateWithSignature {
        cwsX509certificate    :: X509.Certificate,
        cwsSignatureAlgorithm :: SignatureAlgorithmIdentifier,
        cwsSignature          :: SignatureValue,
        cwsEncoded            :: BSS.ShortByteString
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)


-- | Types for the signed object template 
-- https://tools.ietf.org/html/rfc5652

data SignedObject a = SignedObject {
        soContentType :: ContentType, 
        soContent     :: SignedData a
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)

-- deriving instance NFData ASN1

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
        scVersion          :: {-# UNPACK #-} CMSVersion, 
        scDigestAlgorithms :: DigestAlgorithmIdentifiers, 
        scEncapContentInfo :: EncapsulatedContentInfo a, 
        scCertificate      :: EECerObject, 
        scSignerInfos      :: SignerInfos
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)

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
    deriving anyclass (TheBinary)

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
        siVersion          :: {-# UNPACK #-} CMSVersion, 
        siSid              :: SignerIdentifier, 
        digestAlgorithm    :: DigestAlgorithmIdentifiers, 
        signedAttrs        :: SignedAttributes, 
        signatureAlgorithm :: SignatureAlgorithmIdentifier, 
        signature          :: SignatureValue
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)

newtype IssuerAndSerialNumber = IssuerAndSerialNumber Text 
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

newtype SignerIdentifier = SignerIdentifier BSS.ShortByteString 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

newtype ContentType = ContentType OID 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

newtype CMSVersion = CMSVersion Int 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

newtype DigestAlgorithmIdentifiers = DigestAlgorithmIdentifiers [OID] 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

newtype DigestAlgorithmIdentifier = DigestAlgorithmIdentifier OID
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

newtype SignatureAlgorithmIdentifier = SignatureAlgorithmIdentifier X509.SignatureALG  
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)

newtype SignatureValue = SignatureValue BSS.ShortByteString 
    deriving stock (Show, Eq, Ord, Generic)  
    deriving anyclass (TheBinary, NFData)


-- | According to https://tools.ietf.org/html/rfc5652#page-16
-- there has to be DER encoded signedAttribute set
data SignedAttributes = SignedAttributes [Attribute] BSS.ShortByteString
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)

data Attribute = ContentTypeAttr ContentType 
            | MessageDigest BSS.ShortByteString
            | SigningTime DateTime (Maybe TimezoneOffset)
            | BinarySigningTime Integer 
            | UnknownAttribute OID [ASN1]
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)


-- Subject Public Key Info
newtype SPKI = SPKI { unSPKI :: EncodedBase64 }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

newtype EncodedBase64 = EncodedBase64 BS.ByteString
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)
    deriving newtype (Monoid, Semigroup)

newtype DecodedBase64 = DecodedBase64 BS.ByteString
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)
    deriving newtype (Monoid, Semigroup)

newtype TaName = TaName { unTaName :: Text }
    deriving stock (Eq, Ord, Generic, Typeable, Data)
    deriving anyclass (TheBinary, NFData)

instance Show TaName where
    show = show . unTaName

newtype Vrps = Vrps { unVrps :: V.Vector Vrp }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)
    deriving Semigroup via GenericSemigroup Vrps
    deriving Monoid    via GenericMonoid Vrps

newtype Roas = Roas { unRoas :: MonoidalMap ObjectKey (V.Vector Vrp) }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)
    deriving Semigroup via GenericSemigroup Roas
    deriving Monoid    via GenericMonoid Roas

data TA = TA {
        taName        :: TaName, 
        taCertificate :: Maybe ResourceCertificate,
        taUri         :: URI,
        taSpki        :: SPKI
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)
  

data Payloads = Payloads {
        roas     :: Roas,
        spls     :: Set.Set SplN,
        aspas    :: Set.Set Aspa,
        gbrs     :: Set.Set (T2 Hash Gbr),
        bgpCerts :: Set.Set BGPSecPayload  
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)
    deriving Semigroup via GenericSemigroup Payloads
    deriving Monoid    via GenericMonoid Payloads

newtype PerTA a = PerTA { unPerTA :: MonoidalMap TaName a }
    deriving stock (Show, Eq, Ord, Generic, Functor, Traversable, Foldable)
    deriving anyclass (TheBinary, NFData)
    deriving Semigroup via GenericSemigroup (PerTA a)
    deriving Monoid    via GenericMonoid (PerTA a)


-- Some auxiliary types
newtype Size = Size { unSize :: Int64 }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (Num)
    deriving anyclass (TheBinary)
    deriving Semigroup via Sum Size
    deriving Monoid via Sum Size

newtype UrlKey = UrlKey ArtificialKey
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

newtype ObjectKey = ObjectKey ArtificialKey
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

newtype ArtificialKey = ArtificialKey Int64
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

data ObjectIdentity = KeyIdentity {-# UNPACK #-} ObjectKey
                    | HashIdentity Hash
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

data ValidationVersion = ValidationVersion { 
        validatedBy    :: WorldVersion,
        validationsKey :: ArtificialKey,
        metricsKey     :: ArtificialKey,
        roasKey        :: ArtificialKey,
        aspasKey       :: ArtificialKey,
        splsKey        :: ArtificialKey,            
        gbrsKey        :: ArtificialKey,
        bgpCertsKey    :: ArtificialKey
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)    


data VersionMeta = VersionMeta { 
        perTa               :: PerTA ValidationVersion,
        commonValidationKey :: ArtificialKey,
        commonMetricsKey    :: ArtificialKey
    }      
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)


newtype EarliestToExpire = EarliestToExpire Instant
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass (TheBinary)
    deriving Semigroup via Min EarliestToExpire


instance Monoid EarliestToExpire where
    -- It is 2262-04-11 23:47:16.000Z, it's 
    -- 1) far enough to set it as "later that anything else"
    -- 2) Anything bigger than that wraps around to the year 1677
    mempty = EarliestToExpire $ Instant $ 1000_000_000 * 9_223_372_036

-- Small utility functions that don't have anywhere else to go

toAKI :: SKI -> AKI
toAKI (SKI ki) = AKI ki

mkKI :: BS.ByteString -> KI
mkKI = KI . BSS.toShort

skiLen :: SKI -> Int
skiLen (SKI (KI bs)) = BSS.length bs

getCMSContent :: CMS a -> a
getCMSContent = cContent . scEncapContentInfo . soContent . unCMS

getEEResourceCert :: SignedObject a -> EECerObject
getEEResourceCert = scCertificate . soContent

getCertWithSignature :: WithRawResourceCertificate a => a -> CertificateWithSignature
getCertWithSignature = certX509 . getRawCert

getEECert :: SignedObject a -> CertificateWithSignature
getEECert = certX509 . getRawCert . scCertificate . soContent

getMftNumber :: MftObject -> Serial 
getMftNumber mft = let 
    Manifest {..} = getCMSContent (cmsPayload mft) 
    in mftNumber

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
    
locationsToList :: Locations -> [Text]
locationsToList = toList . locationsToNEList    

locationsToNEList :: Locations -> NonEmpty.NonEmpty Text
locationsToNEList =    
      NonEmpty.map (unURI . getURL) 
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

oneOfLocations :: Locations -> RpkiURL -> Bool
oneOfLocations (Locations urls) url = url `elem` neSetToList urls

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


estimateVrpCount :: PerTA Vrps -> Int 
estimateVrpCount = sum . map (V.length . unVrps . snd) . perTA

estimateVrpCountRoas :: Roas -> Int 
estimateVrpCountRoas = sum . map V.length . MonoidalMap.elems . unRoas

-- Precise but much more expensive
uniqueVrpCount :: PerTA Vrps -> Int 
uniqueVrpCount = Set.size . Set.fromList . concatMap (V.toList . unVrps . snd) . perTA
-- uniqueVrpCount _ = 0 

createVrps :: Foldable f => f Vrp -> Vrps
createVrps vrps = Vrps $ V.fromList $ toList vrps

toVrps :: Roas -> Vrps
toVrps (Roas roas) = Vrps $ V.concat $ MonoidalMap.elems roas

perTA :: PerTA a -> [(TaName, a)]
perTA (PerTA a) = MonoidalMap.toList a

toPerTA :: [(TaName, a)] -> PerTA a
toPerTA = PerTA . MonoidalMap.fromList

allTAs :: Monoid a => PerTA a -> a
allTAs (PerTA a) = mconcat $ MonoidalMap.elems a

getForTA :: PerTA a -> TaName -> Maybe a
getForTA (PerTA a) taName = MonoidalMap.lookup taName a

divSize :: Size -> Size -> Size
divSize (Size s1) (Size n) = Size $ s1 `div` n