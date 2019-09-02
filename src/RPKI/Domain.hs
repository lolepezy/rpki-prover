{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass       #-}

module RPKI.Domain where

import qualified Data.Set as S
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Short as TS

import Control.DeepSeq

import Data.Ord (comparing)

import Data.Kind (Type)
import Data.Data (Typeable)

import Data.List.NonEmpty

import Data.Hourglass

import GHC.Generics

import qualified Data.X509 as X509

import RPKI.IP    
import RPKI.SignTypes

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

withRFC :: AnRFC r -> (forall rfc . (r rfc) -> a) -> a
withRFC (Left (WithRFC a)) f = f a
withRFC (Right (WithRFC a)) f = f a  

newtype IpResources = IpResources (AnRFC IpResourceSet)    
    deriving (Show, Eq, Ord, Typeable, Generic)

newtype RSet r = RSet (AnRFC (ResourceSet r))
    deriving (Show, Eq, Ord, Typeable, Generic)

data ResourceSet r (rfc :: ValidationRFC) = RS (S.Set r) | Inherit
    deriving (Show, Eq, Ord, Typeable, Generic)

data IpResourceSet (rfc :: ValidationRFC) = 
    IpResourceSet !(ResourceSet (IpResource 'Ipv4F) rfc)
                  !(ResourceSet (IpResource 'Ipv6F) rfc)
    deriving (Show, Eq, Ord, Typeable, Generic)                    

-- TODO Use library type?
data Hash = Hash B.ByteString deriving (Show, Eq, Ord, Typeable, Generic, NFData)


newtype URI  = URI T.Text deriving (Show, Eq, Ord, Typeable, Generic, NFData)
newtype KI   = KI  B.ByteString deriving (Show, Eq, Ord, Typeable, Generic, NFData)
newtype SKI  = SKI KI deriving (Show, Eq, Ord, Typeable, Generic, NFData)
newtype AKI  = AKI KI deriving (Show, Eq, Ord, Typeable, Generic, NFData)

newtype SessionId = SessionId B.ByteString deriving (Show, Eq, Ord, Typeable, Generic, NFData)
newtype Serial = Serial Integer deriving (Show, Eq, Ord, Typeable, Generic, NFData)
newtype Version = Version Integer deriving (Show, Eq, Ord, Typeable, Generic, NFData)


-- | Objects

newtype CMS a = CMS (SignedObject a) deriving (Show, Eq, Typeable, Generic)

newtype CerObject = CerObject ResourceCert deriving (Show, Eq, Ord, Typeable, Generic)
newtype CrlObject = CrlObject SignCRL deriving (Show, Eq, Typeable, Generic)

type MftObject = CMS Manifest
type RoaObject = CMS [Roa]
type GbrObject = CMS Gbr
    
data CrlMeta = CrlMeta {
    locations :: NonEmpty URI, 
    hash      :: Hash, 
    aki       :: AKI, 
    crlNumber :: Integer
} deriving (Show, Eq, Ord, Typeable)

data RpkiMeta = RpkiMeta {
    locations :: (NonEmpty URI), 
    hash      :: Hash, 
    aki       :: (Maybe AKI), 
    ski       :: SKI, 
    serial    :: Serial
} deriving (Show, Eq, Ord, Typeable)

data RO = CerRO CerObject 
        | MftRO MftObject
        | RoaRO RoaObject
        | GbrRO GbrObject
    deriving (Show, Eq, Typeable, Generic)

data RpkiObject = RpkiObject RpkiMeta RO 
                | RpkiCrl CrlMeta CrlObject
    deriving (Show, Eq, Typeable, Generic)

data ResourceCertificate (rfc :: ValidationRFC) = ResourceCertificate {
    certX509    :: (X509.SignedExact X509.Certificate), 
    ipResources :: (Maybe (IpResourceSet rfc)), 
    asResources :: (Maybe (ResourceSet AsResource rfc))
} deriving (Show, Eq, Typeable)

-- TODO Implement it properly
instance Ord (ResourceCertificate (rfc :: ValidationRFC)) where
    compare = comparing ipResources <> comparing asResources

newtype ResourceCert = ResourceCert (AnRFC ResourceCertificate)
    deriving (Show, Eq, Ord, Typeable)

data Roa = Roa     
    ASN 
    APrefix    
    {-# UNPACK #-} !Int
    deriving (Show, Eq, Ord, Typeable)

data Manifest = Manifest {
    mftNumber   :: Int, 
    fileHashAlg :: X509.HashALG, 
    thisTime    :: DateTime, 
    nextTime    :: DateTime, 
    mftEntries  :: [(T.Text, Hash)]
} deriving (Show, Eq, Typeable)

data SignCRL = SignCRL {
  crl                :: X509.CRL,
  signatureAlgorithm :: SignatureAlgorithmIdentifier,
  signatureValue     :: SignatureValue,
  encodedValue       :: B.ByteString
} deriving (Show, Eq, Typeable, Generic)

data Gbr = Gbr deriving (Show, Eq, Ord, Typeable)

data RpkiObj = RpkiObj !ObjId !RpkiMeta
    deriving (Show, Eq, Ord, Typeable)

-- Id of the object in the object store
newtype ObjId = ObjId B.ByteString deriving (Show, Eq, Ord, Typeable)


-- Subject Public Key Info
newtype SPKI = SPKI B.ByteString
    deriving (Show, Eq, Ord, Typeable, Generic)

newtype TaName = TaName T.Text

data TA = TA {
    taName        :: !TaName
  , taCertificate :: !ResourceCert
  , taUri         :: !URI
  , taSpki        :: !SPKI
}

data RepoType = Rsync | Rrdp

data Repository (t :: RepoType) where
    RsyncRepo :: URI -> Repository 'Rsync
    RrdpRepo  :: URI -> SessionId -> Serial -> Repository 'Rrdp

deriving instance Show (Repository t)
deriving instance Eq (Repository t)
deriving instance Ord (Repository t)
deriving instance Typeable (Repository t)

    
newtype Message = Message TS.ShortText 
    deriving (Show, Eq, Ord, Typeable, Generic)

data Invalid = Error | Warning
    deriving (Show, Eq, Ord, Typeable, Generic)

        
-- Validation errors

data VError = InvalidCert !T.Text |
              ParentDoesntHaveResources |
              NoAKIinManifest |
              ROACannotBeAParent |
              NoAKI | 
              RrdpProblem RrdpError
    deriving (Show, Eq, Ord, Typeable, Generic)
    
data RrdpError = BrokenSerial !B.ByteString |
                 NoSessionId |
                 NoSerial | 
                 NoSnapshotHash | 
                 NoSnapshotURI | 
                 NoDeltaSerial | 
                 NoDeltaURI | 
                 NoDeltaHash |
                 BadHash !B.ByteString |
                 NoVersion | 
                 BadVersion !B.ByteString | 
                 NoPublishURI |
                 BadBase64 !B.ByteString |
                 BadPublish !B.ByteString |
                 NoHashInWithdraw |
                 ContentInWithdraw !B.ByteString |
                 LocalSerialBiggerThanRemote Serial Serial |
                 NonConsecutiveDeltaSerials [(Serial, Serial)] |
                 CantDownloadNotification String |
                 CantDownloadSnapshot String |
                 CantDownloadDelta String |
                 SnapshotHashMismatch Hash Hash |
                 DeltaHashMismatch Hash Hash Serial
    deriving (Show, Eq, Ord, Typeable, Generic, NFData)

data CryptoValidation = CryptoOk |
                        NoDigest |
                        InvalidSignature