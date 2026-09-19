{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}

module RPKI.Domain where

import           Control.Lens
import           Control.DeepSeq          (NFData)
import           Control.Monad.ST         (runST)

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Short    as BSS
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Vector              as V
import qualified Data.Vector.Algorithms.Merge as VectorSort
import qualified Data.Vector.Algorithms.Intro as VectorSortU
import qualified Data.Vector.Unboxed      as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Data.Generics.Product.Typed

import           Data.ByteString.Base16   as Hex
import qualified Data.String.Conversions  as SC

import           Data.Int
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
import           Data.Monoid.Generic
import           Data.Tuple.Strict

import           GHC.Generics

import qualified Data.X509                as X509

import           Data.ASN1.OID
import           Data.ASN1.Types

import           Data.Set                 (Set)

import           RPKI.AppTypes
import           RPKI.Resources.Resources as RS
import           RPKI.Resources.Types
import           RPKI.Time

import           RPKI.Domain.Packed
import           RPKI.Store.Base.Serialisation


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
    deriving newtype (WithSKI, WithRawResourceCertificate, WithAKI, 
                      WithResources, WithValidityPeriod, WithSerial)

class OfCertType c (t :: CertType)    

data CertType = CACert | EECert | BGPCert
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

newtype Hash = Hash { unHash :: BSS.ShortByteString } 
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

data ValidityPeriod = ValidityPeriod {
        notBefore :: Instant,
        notAfter  :: Instant
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

data SignMaterial = SignMaterial {
        algorithm  :: SignatureAlgorithmIdentifier,
        signature  :: SignatureValue,
        signedData :: BSS.ShortByteString
    }
    deriving stock (Show, Eq, Generic) 

class WithValidityPeriod a where
    getValidityPeriod :: a -> ValidityPeriod

class WithURL a where
    getURL :: a -> URI

class WithRpkiURL a where
    getRpkiURL :: a -> RpkiURL

class WithAKI a where
    getAKI :: a -> Maybe AKI

class WithLocations a where
    getLocations :: a -> Maybe Locations

class WithHash a where
    getHash :: a -> Hash

class WithSKI a where
    getSKI :: a -> SKI

class WithPubKey a where
    getPubKey :: a -> X509.PubKey

class WithRawResourceCertificate a where
    getRawCert :: a -> RawResourceCertificate

class WithSerial a where
    getSerial :: a -> Serial

class WithRpkiObjectType a where
    getRpkiObjectType :: a -> RpkiObjectType

class WithResources a where
    getResources :: a -> AllResources

class WithSignMaterial a where
    getSignMaterial :: a -> SignMaterial

newtype Validated a = Validated a
    deriving stock (Show, Eq, Generic)
    deriving newtype (WithSKI, WithAKI, WithHash, WithPubKey, WithRpkiObjectType)

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
    deriving newtype (TheBinary, NFData)

newtype SKI  = SKI { unSKI :: KI }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (TheBinary, NFData)

newtype AKI  = AKI { unAKI :: KI }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (TheBinary, NFData)

newtype SessionId = SessionId { unSessionId :: Text }
    deriving stock (Eq, Ord, Generic)
    deriving newtype (TheBinary, NFData)

instance Show SessionId where
    show (SessionId s) = show s

newtype Serial = Serial Integer     
    deriving stock (Eq, Ord, Generic)
    deriving newtype (TheBinary, NFData)

newtype Version = Version Integer 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

newtype Locations = Locations { unLocations :: NESet RpkiURL } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (TheBinary, NFData)
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
    deriving newtype (TheBinary)

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
type RoaObject = CMSBasedObject VrpsPerAs

-- https://datatracker.ietf.org/doc/draft-ietf-sidrops-rpki-prefixlist
type SplObject = CMSBasedObject SplPayload

-- https://datatracker.ietf.org/doc/rfc6493
type GbrObject = CMSBasedObject Gbr

-- https://datatracker.ietf.org/doc/draft-ietf-sidrops-rpki-rsc/
type RscObject = CMSBasedObject Rsc

-- https://datatracker.ietf.org/doc/draft-ietf-sidrops-aspa-profile/
type AspaObject = CMSBasedObject Aspa

data RpkiObject_ ca mft roa spl gbr rsc aspa bgpSec crl = 
                  CerRO ca 
                | MftRO mft
                | RoaRO roa
                | SplRO spl
                | GbrRO gbr
                | RscRO rsc
                | AspaRO aspa
                | BgpRO bgpSec
                | CrlRO crl
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary) 

type ParsedRpkiObject = RpkiObject_ 
        CaCerObject 
        MftObject 
        RoaObject 
        SplObject 
        GbrObject 
        RscObject 
        AspaObject 
        BgpCerObject 
        CrlObject

type WellStructuredMft  = WellStructuredCms Manifest
type WellStructuredRoa  = WellStructuredCms VrpsPerAs
type WellStructuredSpl  = WellStructuredCms SplPayload
type WellStructuredGbr  = WellStructuredCms Gbr
type WellStructuredRsc  = WellStructuredCms Rsc
type WellStructuredAspa = WellStructuredCms Aspa

type WellStructuredRpkiObject = RpkiObject_ 
        WellStructuredCaCert 
        WellStructuredMft
        WellStructuredRoa
        WellStructuredSpl
        WellStructuredGbr
        WellStructuredRsc  
        WellStructuredAspa
        WellStructuredBgpCert
        CrlObject

{-# INLINE foldRpkiObject #-}
foldRpkiObject ::
    (ca -> r) ->
    (mft -> r) ->
    (roa -> r) ->
    (spl -> r) ->
    (gbr -> r) ->
    (rsc -> r) ->
    (aspa -> r) ->
    (bgpSec -> r) ->
    (crl -> r) ->
    RpkiObject_ ca mft roa spl gbr rsc aspa bgpSec crl ->
    r
foldRpkiObject onCer onMft onRoa onSpl onGbr onRsc onAspa onBgp onCrl = \case
    CerRO c  -> onCer c
    MftRO c  -> onMft c
    RoaRO c  -> onRoa c
    SplRO c  -> onSpl c
    GbrRO c  -> onGbr c
    RscRO c  -> onRsc c
    AspaRO c -> onAspa c
    BgpRO c  -> onBgp c
    CrlRO c  -> onCrl c


data RpkiObjectType = CER | MFT | CRL | ROA | ASPA | GBR | SPL | BGPSec | RSC
    deriving (Show, Read, Eq, Ord, Generic)    
    deriving anyclass (TheBinary, NFData)

instance {-# OVERLAPPING #-} (Generic o, HasType Hash o) => WithHash o where
    getHash o = o ^. typed @Hash

instance {-# OVERLAPPING #-} (Generic o, HasType SKI o) => WithSKI o where
    getSKI o = o ^. typed @SKI

instance {-# OVERLAPPING #-} (Generic o, HasType X509.PubKey o) => WithPubKey o where
    getPubKey o = o ^. typed @X509.PubKey

instance {-# OVERLAPPING #-} (Generic o, HasType AKI o) => WithAKI o where
    getAKI o = Just $ o ^. typed @AKI

instance {-# OVERLAPPING #-} WithAKI CrlObject where
    getAKI CrlObject {..} = Just aki

instance {-# OVERLAPPING #-} WithAKI CaCerObject where
    getAKI CaCerObject {..} = aki

instance WithResources CaCerObject where
    getResources CaCerObject { certificate } = getResources certificate
    
instance {-# OVERLAPPING #-} WithPubKey CaCerObject where
    getPubKey CaCerObject { certificate } = X509.certPubKey $ cwsX509certificate $ getCertWithSignature certificate

instance {-# OVERLAPPING #-} WithAKI (CMSBasedObject a) where
    getAKI CMSBasedObject {..} = getAKI $ getEEResourceCert cmsPayload 

instance {-# OVERLAPPING #-} WithPubKey (CMSBasedObject a) where
    getPubKey CMSBasedObject { cmsPayload } = getPubKey $ getEEResourceCert cmsPayload

instance WithResources (CMSBasedObject a) where
    getResources CMSBasedObject { cmsPayload } = getResources $ getEEResourceCert cmsPayload

instance {-# OVERLAPPING #-} WithValidityPeriod (CMSBasedObject a) where
    getValidityPeriod CMSBasedObject {..} = 
        let (nb, na) = X509.certValidity $ cwsX509certificate $ getCertWithSignature 
                     $ getEEResourceCert cmsPayload 
        in ValidityPeriod (newInstant nb) (newInstant na)

instance {-# OVERLAPPING #-} WithSerial (CMSBasedObject a) where
    getSerial CMSBasedObject {..} = 
        Serial $ X509.certSerial $ cwsX509certificate $ getCertWithSignature 
            $ getEEResourceCert cmsPayload 

instance WithRawResourceCertificate (CMSBasedObject a) where
    getRawCert CMSBasedObject {..} = getRawCert $ getEEResourceCert cmsPayload 

instance {-# OVERLAPPING #-} WithAKI BgpCerObject where
    getAKI BgpCerObject {..} = aki

instance {-# OVERLAPPING #-} WithPubKey BgpCerObject where
    getPubKey BgpCerObject { certificate } = X509.certPubKey $ cwsX509certificate $ getCertWithSignature certificate

instance WithResources BgpCerObject where
    getResources BgpCerObject { certificate } = getResources certificate

instance {-# OVERLAPPING #-} WithPubKey EECerObject where
    getPubKey EECerObject { certificate } = X509.certPubKey $ cwsX509certificate $ getCertWithSignature certificate

instance WithResources EECerObject where
    getResources EECerObject { certificate } = getResources certificate

instance {-# OVERLAPPING #-} WithSKI (CMSBasedObject a) where    
    getSKI CMSBasedObject {..} = getSKI $ getEEResourceCert cmsPayload 

instance WithRawResourceCertificate a => WithValidityPeriod a where
    getValidityPeriod cert =
        let (nb, na) = X509.certValidity $ cwsX509certificate $ getCertWithSignature $ getRawCert cert
        in ValidityPeriod (newInstant nb) (newInstant na)

instance {-# OVERLAPPING #-} WithRawResourceCertificate a => WithSerial a where
    getSerial = Serial . X509.certSerial . cwsX509certificate . certX509 . getRawCert

instance {-# OVERLAPPABLE #-} WithRawResourceCertificate a => WithSignMaterial a where
    getSignMaterial (certX509 . getRawCert -> CertificateWithSignature {                
                cwsSignatureAlgorithm = algorithm,
                cwsSignature          = signature,
                cwsEncoded            = signedData
            }) = SignMaterial {..}

instance WithSignMaterial CrlObject where
    getSignMaterial CrlObject {
        signCrl = SignCRL {
            signatureAlgorithm = algorithm,
            signatureValue = signature,
            encodedValue = signedData
        }
    } = SignMaterial {..}

instance WithSignMaterial (CMS a) where
    getSignMaterial (CMS so) = let        
            SignerInfos {
                signature = cmsSignature,
                signedAttrs = SignedAttributes _ signedAttrsBS
            } = scSignerInfos $ soContent so            
        in SignMaterial {
            algorithm = cwsSignatureAlgorithm $ certX509 $ getRawCert $ scCertificate $ soContent so,
            signature = cmsSignature,
            signedData = signedAttrsBS
        }

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

instance WithResources RawResourceCertificate where
    getResources RawResourceCertificate { resources } = resources

instance WithResources ResourceCertificate where
    getResources = getResources . getRawCert

instance OfCertType (TypedCert c (t :: CertType)) t
instance OfCertType CaCerObject 'CACert
instance OfCertType EECerObject 'EECert
instance OfCertType BgpCerObject 'BGPCert


-- Ehm, it looks pretty terrible, but it works.

instance {-# OVERLAPPING #-} (WithAKI ca, WithAKI mft, WithAKI roa, WithAKI spl, WithAKI gbr, 
          WithAKI rsc, WithAKI aspa, WithAKI bgpSec, WithAKI crl) => 
    WithAKI (RpkiObject_ ca mft roa spl gbr rsc aspa bgpSec crl) where
    getAKI = foldRpkiObject getAKI getAKI getAKI getAKI getAKI getAKI getAKI getAKI getAKI

instance {-# OVERLAPPING #-} (WithPubKey ca, WithPubKey mft, WithPubKey roa, WithPubKey spl, WithPubKey gbr, 
          WithPubKey rsc, WithPubKey aspa, WithPubKey bgpSec, WithPubKey crl) => 
    WithPubKey (RpkiObject_ ca mft roa spl gbr rsc aspa bgpSec crl) where
    getPubKey = foldRpkiObject getPubKey getPubKey getPubKey getPubKey getPubKey getPubKey getPubKey getPubKey getPubKey

instance {-# OVERLAPPING #-} (WithHash ca, WithHash mft, WithHash roa, WithHash spl, WithHash gbr, 
          WithHash rsc, WithHash aspa, WithHash bgpSec, WithHash crl) => 
    WithHash (RpkiObject_ ca mft roa spl gbr rsc aspa bgpSec crl) where
    getHash = foldRpkiObject getHash getHash getHash getHash getHash getHash getHash getHash getHash

instance WithRpkiObjectType (RpkiObject_ ca mft roa spl gbr rsc aspa bgpSec crl) where
    getRpkiObjectType = foldRpkiObject 
                         (const CER) (const MFT) (const ROA) (const SPL) (const GBR) 
                         (const RSC) (const ASPA) (const BGPSec) (const CRL)


-- | 'locations' is 'Nothing' for objects fetched from an Erik relay: they
-- are addressed by content hash, so there is nothing to reconstruct a
-- location from and no location-based checks apply to them.
data Located a = Located {
        locations :: Maybe Locations,
        payload   :: a
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)


instance WithLocations (Located a) where
    getLocations Located {..} = locations

instance WithLocations Locations where
    getLocations = Just

instance {-# OVERLAPPING #-} WithAKI a => WithAKI (Located a) where
    getAKI (Located _ o) = getAKI o    

instance {-# OVERLAPPING #-} WithHash a => WithHash (Located a) where
    getHash (Located _ o) = getHash o

instance {-# OVERLAPPING #-} WithPubKey a => WithPubKey (Located a) where
    getPubKey (Located _ o) = getPubKey o

instance {-# OVERLAPPING #-} WithSKI a => WithSKI (Located a) where
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
    deriving newtype (TheBinary)

data Vrp = Vrp ASN IpPrefix PrefixLength
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

-- ROA-internal prefix types: split by family to eliminate the IpPrefix sum-type
-- wrapper and allow UNPACK on the address fields.
data Vrp4 = Vrp4 {-# UNPACK #-} !Ipv4Prefix {-# UNPACK #-} !PrefixLength
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

data Vrp6 = Vrp6 {-# UNPACK #-} !Ipv6Prefix {-# UNPACK #-} !PrefixLength
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

-- ROA payload: ASN stored once; IPv4 and IPv6 entries kept in separate lists.
data VrpsPerAs = VrpsPerAs {
        roaAsn :: {-# UNPACK #-} ASN,
        roaV4  :: [Vrp4],
        roaV6  :: [Vrp6]
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

instance Semigroup VrpsPerAs where
    -- ROA key collisions are unexpected; keep the first ASN and aggregate
    -- all prefixes so MonoidalMap can still combine duplicate keys.
    VrpsPerAs asn1 v41 v61 <> VrpsPerAs _ v42 v62 =
        VrpsPerAs asn1 (v41 <> v42) (v61 <> v62)

roaPayloadToVrps :: VrpsPerAs -> [Vrp]
roaPayloadToVrps (VrpsPerAs asn v4s v6s) =
    map (\(Vrp4 p len) -> Vrp asn (Ipv4P p) len) v4s <>
    map (\(Vrp6 p len) -> Vrp asn (Ipv6P p) len) v6s


-- Signed Prefix List normalised payload
data SplN = SplN ASN IpPrefix
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

data SplPayload = SplPayload ASN [IpPrefix]     
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data MftPair = MftPair {
        fileName :: Text,
        hash     :: Hash
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

data Manifest = Manifest {
        mftNumber   :: Serial, 
        fileHashAlg :: X509.HashALG, 
        thisTime    :: {-# UNPACK #-} Instant, 
        nextTime    :: {-# UNPACK #-} Instant, 
        mftEntries  :: [MftPair]
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)

data SignCRL = SignCRL {
        thisUpdateTime     :: Instant,
        nextUpdateTime     :: Instant,
        signatureAlgorithm :: SignatureAlgorithmIdentifier,
        signatureValue     :: SignatureValue,
        encodedValue       :: BSS.ShortByteString,
        crlNumber          :: Serial,
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
        customer  :: ASN,
        providers :: Set ASN
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

data BGPSecPayload = BGPSecPayload {
        bgpSecSki  :: SKI,
        bgpSecAsns :: [ASN],
        bgpSecSpki :: SPKI
        -- TODO Possible store the hash of the original BGP certificate?
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)


-- https://datatracker.ietf.org/doc/html/draft-spaghetti-sidrops-rpki-erik-protocol

newtype FQDN = FQDN { unFQDN :: Text }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

instance Show FQDN where
    show = Text.unpack . unFQDN

data ErikIndex = ErikIndex {
        indexScope    :: Text,
        indexTime     :: Instant,  
        hashAlg       :: DigestAlgorithmIdentifier,
        partitionList :: [ErikPartitionRef]
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)   

data ErikPartitionRef = ErikPartitionRef {
        hash       :: Hash,
        size       :: Size
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)   

data ErikPartition = ErikPartition {
        partitionTime :: Instant,   
        hashAlg       :: DigestAlgorithmIdentifier,
        manifestList  :: [ErikManifestRef]
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

data ErikManifestRef = ErikManifestRef {
        hash           :: Hash,
        size           :: Size,
        aki            :: AKI,
        manifestNumber :: Serial,
        thisUpdate     :: Instant,
        locations      :: [URI]
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)   

data ErikSegmentIndex = ErikSegmentIndex {
        segmentScope :: Text,
        segmentTime  :: Instant,
        hashAlg      :: DigestAlgorithmIdentifier,
        segmentList  :: [ErikSegmentRef]
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

data ErikSegmentRef = ErikSegmentRef {
        segment :: Instant,
        index   :: Hash
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
    deriving anyclass (TheBinary)

{- 
    EncapsulatedContentInfo ::= SEQUENCE {
        eContentType ContentType,
        eContent [0] EXPLICIT OCTET STRING OPTIONAL }
-}
data EncapsulatedContentInfo a = EncapsulatedContentInfo {
        eContentType  :: ContentType, 
        -- Raw eContent octets are retained to verify messageDigest against
        -- the exact encapsulated payload during CMS validation.
        -- https://www.rfc-editor.org/rfc/rfc6488#section-2.1.6.4.2
        -- https://www.rfc-editor.org/rfc/rfc6488#section-3
        eContentBytes :: BS.ByteString,
        cContent      :: a    
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
        siVersion          :: CMSVersion, 
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
    deriving newtype (TheBinary)

newtype SignerIdentifier = SignerIdentifier BSS.ShortByteString 
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (TheBinary)

newtype ContentType = ContentType OID 
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (TheBinary)

newtype CMSVersion = CMSVersion Int 
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (TheBinary)

newtype DigestAlgorithmIdentifiers = DigestAlgorithmIdentifiers [OID] 
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (TheBinary, NFData)

newtype DigestAlgorithmIdentifier = DigestAlgorithmIdentifier OID
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (TheBinary, NFData)

newtype SignatureAlgorithmIdentifier = SignatureAlgorithmIdentifier X509.SignatureALG  
    deriving stock (Show, Eq, Generic)
    deriving newtype (TheBinary)

newtype SignatureValue = SignatureValue BSS.ShortByteString 
    deriving stock (Show, Eq, Ord, Generic)  
    deriving newtype (TheBinary, NFData)


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
    deriving newtype (TheBinary, NFData)

newtype EncodedBase64 = EncodedBase64 BS.ByteString
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (TheBinary, NFData)
    deriving newtype (Monoid, Semigroup)

newtype DecodedBase64 = DecodedBase64 BS.ByteString
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (TheBinary, NFData)
    deriving newtype (Monoid, Semigroup)

newtype TaName = TaName { unTaName :: Text }
    deriving stock (Eq, Ord, Generic, Typeable, Data)
    deriving newtype (TheBinary, NFData)

instance Show TaName where
    show = show . unTaName


-- | VRPs stored packed, in an unboxed vector.
--
-- This used to be a boxed @V.Vector Vrp@, which cost around 60 bytes per VRP
-- against the 10-22 bytes a VRP actually carries. Worse, it was built lazily
-- from 'Roas' -- @V.fromList@ forces the list spine but not the elements -- so
-- every slot held a thunk that kept its source 'Vrp4'\/'Vrp6' alive, and the
-- process paid for both representations at once plus the thunks in between.
-- An unboxed vector cannot hold a thunk, so building one forces the conversion
-- and lets the 'Roas' go.
-- | VRPs stored packed, split by address family.
--
-- Deliberately not 'TheBinary': VRPs are persisted as 'Roas', and 'Vrps' is
-- only ever derived from them in memory.
--
-- Two vectors rather than one uniform one because an IPv4 VRP is 10 bytes of
-- information and an IPv6 one is 22; a shared layout has to be as wide as the
-- larger, which wastes 13 bytes on ~85% of the set.
data Vrps = Vrps {
        packed4 :: VU.Vector PackedVrp4,
        packed6 :: VU.Vector PackedVrp6
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

instance Semigroup Vrps where
    Vrps a4 a6 <> Vrps b4 b6 = Vrps (a4 VU.++ b4) (a6 VU.++ b6)

instance Monoid Vrps where
    mempty = Vrps VU.empty VU.empty

newtype Roas = Roas { unRoas :: MonoidalMap ObjectKey VrpsPerAs }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (TheBinary, NFData)
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
    deriving newtype (TheBinary, NFData)
    deriving Semigroup via GenericSemigroup (PerTA a)
    deriving Monoid    via GenericMonoid (PerTA a)

newtype UrlKey = UrlKey ArtificialKey
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (TheBinary)

newtype ObjectKey = ObjectKey ArtificialKey
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (TheBinary, NFData)

newtype ArtificialKey = ArtificialKey Int64
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (TheBinary, NFData)

data ObjectIdentity = KeyIdentity ObjectKey
                    | HashIdentity Hash
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

newtype EarliestToExpire = EarliestToExpire Instant
    deriving stock (Show, Eq, Ord, Generic)    
    deriving newtype (TheBinary)
    deriving Semigroup via Min EarliestToExpire


instance Monoid EarliestToExpire where
    -- It is 2262-04-11 23:47:16.000Z, it's 
    -- 1) far enough to set it as "later that anything else"
    -- 2) Anything bigger than that wraps around to the year 1677
    mempty = EarliestToExpire $ Instant $ 1000_000_000 * 9_223_372_036


data WellStructuredCert (t :: CertType) = WellStructuredCert {
        hash       :: Hash,
        ski        :: SKI,
        aki        :: Maybe AKI,
        resources  :: AllResources,
        pubKey     :: X509.PubKey,
        serial     :: Serial,
        validity   :: ValidityPeriod,
        certUris   :: CertUris,
        encoded    :: BSS.ShortByteString,
        signature  :: SignatureValue,
        sigAlg     :: SignatureAlgorithmIdentifier
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)    

instance OfCertType (WellStructuredCert t) t

instance {-# OVERLAPPING #-} WithSerial (WellStructuredCert t) where getSerial (WellStructuredCert { serial }) = serial
instance {-# OVERLAPPING #-} WithAKI (WellStructuredCert t) where getAKI (WellStructuredCert { aki }) = aki
instance {-# OVERLAPPING #-} WithPubKey (WellStructuredCert t) where getPubKey (WellStructuredCert { pubKey }) = pubKey
instance {-# OVERLAPPING #-} WithResources (WellStructuredCert t) where getResources (WellStructuredCert { resources }) = resources
instance {-# OVERLAPPING #-} WithValidityPeriod (WellStructuredCert t) where getValidityPeriod (WellStructuredCert { validity }) = validity


-- | Minimized CA certificate.
type WellStructuredCaCert = WellStructuredCert 'CACert

-- | Minimized BGPSec certificate.
type WellStructuredBgpCert = WellStructuredCert 'BGPCert

-- | Minimized EE certificate, stripped of fields that are either
-- constant-after-prevalidation (CMS versions, digest algorithms) or
-- derivable from other stored data (SID == SKI, etc.).
-- Retains only what is needed for chain validation and CMS signature
-- verification.
data CertUris = CertUris {
        aiaCaIssuersUri :: Maybe URI,
        crlDPUri        :: Maybe URI,
        repositoryUri   :: Maybe URI,
        manifestUri     :: Maybe URI,
        rrdpNotifyUri   :: Maybe URI
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

data WellStructuredEECert = WellStructuredEECert {
        ski        :: SKI,
        aki        :: AKI,
        resources  :: AllResources,
        pubKey     :: X509.PubKey,
        serial     :: Serial,
        validity   :: ValidityPeriod,
        certUris   :: CertUris,
        encoded    :: BSS.ShortByteString,   -- TBSCertificate DER bytes
        signature  :: SignatureValue,
        sigAlg     :: SignatureAlgorithmIdentifier
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)

instance OfCertType WellStructuredEECert 'EECert

instance {-# OVERLAPPING #-} WithPubKey WellStructuredEECert where getPubKey (WellStructuredEECert { pubKey }) = pubKey
instance WithResources WellStructuredEECert where getResources (WellStructuredEECert { resources }) = resources
instance {-# OVERLAPPING #-} WithValidityPeriod WellStructuredEECert where getValidityPeriod (WellStructuredEECert { validity }) = validity
instance {-# OVERLAPPING #-} WithSignMaterial WellStructuredEECert where
    getSignMaterial WellStructuredEECert { sigAlg = algorithm, signature, encoded = signedData } =
        SignMaterial {..}

instance {-# OVERLAPPING #-} WithSignMaterial (WellStructuredCert t) where
    getSignMaterial WellStructuredCert { sigAlg = algorithm, signature, encoded = signedData } =
        SignMaterial {..}


-- | Minimized CMS-based signed object.  Replaces 'CMSBasedObject a' in the
-- post-prevalidation in-memory path.
data WellStructuredCms a = WellStructuredCms {
        hash          :: Hash,
        content       :: a,
        eeCert        :: WellStructuredEECert,
        signingTime   :: Instant,
        cmsSignature  :: SignatureValue,
        signedAttrsBS :: BSS.ShortByteString   -- raw DER signed-attributes
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (TheBinary)

instance {-# OVERLAPPING #-} WithAKI  (WellStructuredCms a) where getAKI  (WellStructuredCms { eeCert = WellStructuredEECert { aki } }) = Just aki
instance {-# OVERLAPPING #-} WithPubKey (WellStructuredCms a) where getPubKey (WellStructuredCms { eeCert }) = getPubKey eeCert
instance {-# OVERLAPPING #-} WithResources (WellStructuredCms a) where getResources (WellStructuredCms { eeCert }) = getResources eeCert
instance {-# OVERLAPPING #-} WithValidityPeriod (WellStructuredCms a) where getValidityPeriod (WellStructuredCms { eeCert }) = getValidityPeriod eeCert

instance {-# OVERLAPPING #-} WithSerial WellStructuredEECert  where 
    getSerial (WellStructuredEECert  { serial }) = serial

instance {-# OVERLAPPING #-} WithSerial (WellStructuredCms a) where
    getSerial (WellStructuredCms { eeCert = WellStructuredEECert { serial } }) = serial


-- Small utility functions that don't have anywhere else to go

asKey :: Int64 -> ArtificialKey
asKey = ArtificialKey

toAKI :: SKI -> AKI
toAKI (SKI ki) = AKI ki

mkKI :: BS.ByteString -> KI
mkKI = KI . BSS.toShort

skiLen :: SKI -> Int
skiLen (SKI (KI bs)) = BSS.length bs

getCMSContent :: CMS a -> a
getCMSContent = cContent . scEncapContentInfo . soContent . unCMS

getEEResourceCert :: CMS a -> EECerObject
getEEResourceCert = scCertificate . soContent . unCMS

getCertWithSignature :: WithRawResourceCertificate a => a -> CertificateWithSignature
getCertWithSignature = certX509 . getRawCert

emptyIpResources :: IpResources
emptyIpResources = IpResources RS.emptyIpSet 

emptyAsResources :: AsResources
emptyAsResources = AsResources RS.emptyRS

newCrl :: AKI -> Hash -> SignCRL -> CrlObject
newCrl aki hash signCrl = CrlObject {..}        

newCMSObject :: Hash -> CMS a -> CMSBasedObject a
newCMSObject hash cmsPayload = CMSBasedObject {..}        

toShortBS :: BS.ByteString -> BSS.ShortByteString
toShortBS = BSS.toShort

toNormalBS :: BSS.ShortByteString -> BS.ByteString
toNormalBS = BSS.fromShort

toLocations :: RpkiURL -> Locations
toLocations = Locations . NESet.singleton

pickLocation :: Locations -> RpkiURL
pickLocation = NonEmpty.head . sortRrdpFirstNE . NESet.toList . unLocations

-- | Text description of a located object for log messages: its picked
-- location, or its hash when it doesn't have one (Erik-fetched objects).
describeLocated :: WithHash a => Located a -> Text
describeLocated (Located locations x) =
    case locations of
        Just ls -> toText $ pickLocation ls
        Nothing -> "hash:" <> Text.pack (show $ getHash x)

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
estimateVrpCount = sum . map (vrpsCount . snd) . perTA

estimateVrpCountRoas :: Roas -> Int 
estimateVrpCountRoas =
    sum
        . map payloadVrpCount
        . MonoidalMap.elems
        . unRoas
  where
    payloadVrpCount (VrpsPerAs _ v4 v6) = length v4 + length v6

-- | Distinct VRP count per TA and over all TAs together, from a single sort
-- of each TA's payload.
--
-- Both numbers are reported for every validation, and computing them
-- separately meant sorting everything twice: once per TA, and once more over
-- the concatenation of all TAs, which is the more expensive of the two. The
-- per-TA sorted vectors are all the overall count needs, since sorted stays
-- sorted -- so it is a merge over them rather than a second sort.
uniqueVrpCounts :: PerTA Vrps -> (MonoidalMap TaName Int, Int)
uniqueVrpCounts (PerTA perTaVrps) = (perTaCounts, total)
  where
    sorted = MonoidalMap.map (\(Vrps v4 v6) -> (sortPacked v4, sortPacked v6)) perTaVrps

    perTaCounts = MonoidalMap.map
        (\(v4, v6) -> countSortedDistinct v4 + countSortedDistinct v6) sorted

    total = countDistinctUnion (map fst families) + countDistinctUnion (map snd families)
    families = MonoidalMap.elems sorted

uniqueVrpCount :: PerTA Vrps -> Int 
uniqueVrpCount = snd . uniqueVrpCounts

-- | Sort and deduplicate, in place on the vector.
--
-- This used to go through a list -- @V.toList@, @List.sortBy@, dedup, then
-- @V.fromList@ -- which for a million VRPs allocated ~1.5gb of cons cells per
-- call and took ~2s. Sorting the vector itself does the same work in ~0.5s for
-- ~170mb. It matters because this is rebuilt from scratch on every payload
-- re-read, and the transient peak is what the RTS then sizes the heap for.
--
-- A stable sort keeps this exactly equivalent to the list version: dedup drops
-- structurally equal neighbours, so for a `cmp` that can call two distinct VRPs
-- EQ (neither of the two used here -- 'cmpVrps' and 'compare' -- can), their
-- relative order stays the input order, as it did before.
uniqVrpsBy :: (Vrp -> Vrp -> Ordering) -> Vrps -> V.Vector Vrp
uniqVrpsBy cmp vrps = dedupSorted $ V.modify (VectorSort.sortBy cmp) (vrpsToVector vrps)
  where
    dedupSorted sorted =
        V.ifilter (\i x -> i == 0 || sorted V.! (i - 1) /= x) sorted

createVrps :: Foldable f => f Vrp -> Vrps
createVrps vrps = Vrps (VU.fromList v4s) (VU.fromList v6s)
  where
    (v4s, v6s) = foldr split ([], []) (toList vrps)
    split (Vrp asn (Ipv4P p) maxLen) (as, bs) = (pack4 asn p maxLen : as, bs)
    split (Vrp asn (Ipv6P p) maxLen) (as, bs) = (as, pack6 asn p maxLen : bs)

-- | Convert stored ROA payloads into the packed VRP set.
--
-- Goes straight from 'Vrp4'\/'Vrp6' to packed words without building 'Vrp'
-- values on the way, and the unboxed vector forces the whole conversion, so
-- the 'Roas' it came from become garbage immediately rather than being kept
-- alive behind unevaluated elements.
toVrps :: Roas -> Vrps
toVrps (Roas roas) = Vrps (VU.fromList (concatMap toPacked4 payloads))
                          (VU.fromList (concatMap toPacked6 payloads))
  where
    payloads = MonoidalMap.elems roas
    toPacked4 (VrpsPerAs asn v4s _) = [ pack4 asn p len | Vrp4 p len <- v4s ]
    toPacked6 (VrpsPerAs asn _ v6s) = [ pack6 asn p len | Vrp6 p len <- v6s ]

perTA :: PerTA a -> [(TaName, a)]
perTA (PerTA a) = MonoidalMap.toList a

toPerTA :: [(TaName, a)] -> PerTA a
toPerTA = PerTA . MonoidalMap.fromList

allTAs :: Monoid a => PerTA a -> a
allTAs (PerTA a) = mconcat $ MonoidalMap.elems a

getForTA :: PerTA a -> TaName -> Maybe a
getForTA (PerTA a) taName = MonoidalMap.lookup taName a

pack4 :: ASN -> Ipv4Prefix -> PrefixLength -> PackedVrp4
pack4 (ASN asn) p (PrefixLength maxLen) =
    let (addr, len) = ipv4PrefixWords p in PackedVrp4 asn addr len maxLen

pack6 :: ASN -> Ipv6Prefix -> PrefixLength -> PackedVrp6
pack6 (ASN asn) p (PrefixLength maxLen) =
    let (hi, lo, len) = ipv6PrefixWords p in PackedVrp6 asn hi lo len maxLen

unpack4 :: PackedVrp4 -> Vrp
unpack4 (PackedVrp4 asn addr len maxLen) =
    Vrp (ASN asn) (Ipv4P (mkIpv4Prefix addr len)) (PrefixLength maxLen)

unpack6 :: PackedVrp6 -> Vrp
unpack6 (PackedVrp6 asn hi lo len maxLen) =
    Vrp (ASN asn) (Ipv6P (mkIpv6Prefix hi lo len)) (PrefixLength maxLen)

-- | Materialise the VRPs. Only worth doing where a consumer genuinely needs
-- boxed 'Vrp' values; the packed form is what is kept in memory.
vrpsToVector :: Vrps -> V.Vector Vrp
vrpsToVector = V.fromList . vrpsToList

-- | All the VRPs, IPv4 first. This is storage order, not any meaningful one --
-- for the RTR ordering see 'mergeVrpsBy'.
vrpsToList :: Vrps -> [Vrp]
vrpsToList (Vrps v4 v6) = map unpack4 (VU.toList v4) <> map unpack6 (VU.toList v6)

vrpsCount :: Vrps -> Int
vrpsCount (Vrps v4 v6) = VU.length v4 + VU.length v6

filterVrps :: (Vrp -> Bool) -> Vrps -> Vrps
filterVrps f (Vrps v4 v6) =
    Vrps (VU.filter (f . unpack4) v4) (VU.filter (f . unpack6) v6)

-- | Sort and deduplicate each family in the packed form. Nothing is
-- materialised, so the result costs the same as the input.
uniqVrpsPackedBy :: (PackedVrp4 -> PackedVrp4 -> Ordering)
                 -> (PackedVrp6 -> PackedVrp6 -> Ordering)
                 -> Vrps -> Vrps
uniqVrpsPackedBy cmp4 cmp6 (Vrps v4 v6) =
    Vrps (sortDedup cmp4 v4) (sortDedup cmp6 v6)

sortDedup :: (VU.Unbox a, Eq a) => (a -> a -> Ordering) -> VU.Vector a -> VU.Vector a
sortDedup cmp v
    | VU.null v = v
    | otherwise = let sorted = VU.modify (VectorSortU.sortBy cmp) v
                  in VU.ifilter (\i x -> i == 0 || sorted VU.! (i - 1) /= x) sorted

-- | Merge two already-sorted families into one sequence.
--
-- The caller supplies the cross-family ordering, since which family comes
-- first for a given key is a property of the ordering being reproduced and not
-- of the storage (for the RTR order, see 'RPKI.RTR.Types.cmpPacked4Against6').
mergeVrpsBy :: (PackedVrp4 -> PackedVrp6 -> Ordering) -> Vrps -> [Vrp]
mergeVrpsBy cross (Vrps v4 v6) = go (VU.toList v4) (VU.toList v6)
  where
    go [] bs = map unpack6 bs
    go as [] = map unpack4 as
    go as@(a : as') bs@(b : bs') =
        case cross a b of
            GT -> unpack6 b : go as bs'
            _  -> unpack4 a : go as' bs

-- | Sort in place, without deduplicating.
sortPacked :: (VU.Unbox a, Ord a) => VU.Vector a -> VU.Vector a
sortPacked v
    | VU.null v = v
    | otherwise = VU.modify VectorSortU.sort v

-- | Number of distinct values in an already sorted vector.
--
-- Counting does not need the deduplicated vector that 'sortDedup' builds, and
-- for a million VRPs that vector is the larger half of the cost.
countSortedDistinct :: (VU.Unbox a, Eq a) => VU.Vector a -> Int
countSortedDistinct v
    | VU.null v = 0
    | otherwise = go 1 1 (VU.unsafeHead v)
  where
    len = VU.length v
    go !i !n !prev
        | i >= len  = n
        | otherwise =
            let x = VU.unsafeIndex v i
            in if x == prev then go (i + 1) n prev else go (i + 1) (n + 1) x

-- | Number of distinct values in the union of several already sorted vectors.
--
-- A k-way merge, so it is linear in the total length with @k@ (the number of
-- TAs, five in practice) comparisons per element, and it materialises nothing.
-- Sorting the concatenation instead would redo, at @n log n@, work the inputs
-- already carry.
countDistinctUnion :: (VU.Unbox a, Ord a) => [VU.Vector a] -> Int
countDistinctUnion vectors =
    case filter (not . VU.null) vectors of
        []  -> 0
        [v] -> countSortedDistinct v
        vs  -> runST $ do
            let sources = V.fromList vs
                k       = V.length sources
            cursors <- VUM.replicate k (0 :: Int)
            let
                -- The source whose next unconsumed value is smallest, or -1
                -- once every source is exhausted.
                smallest !i !best
                    | i >= k    = pure best
                    | otherwise = do
                        ci <- VUM.unsafeRead cursors i
                        let vi = V.unsafeIndex sources i
                        if ci >= VU.length vi
                            then smallest (i + 1) best
                            else if best < 0
                                then smallest (i + 1) i
                                else do
                                    cb <- VUM.unsafeRead cursors best
                                    let vb = V.unsafeIndex sources best
                                    smallest (i + 1) $
                                        if VU.unsafeIndex vi ci < VU.unsafeIndex vb cb
                                            then i else best

                go !n !seen !prev = do
                    i <- smallest 0 (-1)
                    if i < 0
                        then pure n
                        else do
                            ci <- VUM.unsafeRead cursors i
                            VUM.unsafeWrite cursors i (ci + 1)
                            let x = VU.unsafeIndex (V.unsafeIndex sources i) ci
                            if seen && x == prev
                                then go n seen prev
                                else go (n + 1) True x

            go 0 False (VU.unsafeHead (V.unsafeHead sources))

getMftChildren :: MftObject -> [MftPair]
getMftChildren = mftEntries . getCMSContent . cmsPayload
