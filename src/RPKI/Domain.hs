{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module RPKI.Domain where

import qualified Data.Set as S
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Short as TS

import Data.Kind (Type)
import Data.Data (Typeable)

import Data.List.NonEmpty

import Data.Hourglass

import qualified Data.X509 as X509
import qualified Data.ASN1.OID as O

import RPKI.Types
import RPKI.IP    

newtype ASN = ASN Int
    deriving (Show, Eq, Ord, Typeable)

data AsResource =  AS !ASN
                 | ASRange  
                    {-# UNPACK #-} !ASN 
                    {-# UNPACK #-} !ASN
    deriving (Show, Eq, Ord, Typeable)

data ValidationRFC = Strict | Reconsidered
    deriving (Show, Eq, Ord, Typeable)

newtype WithRFC (rfc :: ValidationRFC) (r :: ValidationRFC -> Type) = WithRFC (r rfc)

data AnRFC (r :: ValidationRFC -> Type) = 
      LooseRFC (WithRFC 'Reconsidered r)
    | StrictRFC (WithRFC 'Strict r)

withRFC :: forall r a . AnRFC r -> (forall rfc . WithRFC (rfc :: ValidationRFC) r -> a) -> a
withRFC (LooseRFC r) f = f r
withRFC (StrictRFC r) f = f r

-- Deriving machinery
deriving instance Show (r rfc) => Show (WithRFC (rfc :: ValidationRFC) (r :: ValidationRFC -> Type))
deriving instance Eq (r rfc) => Eq (WithRFC (rfc :: ValidationRFC) (r :: ValidationRFC -> Type))
deriving instance Ord (r rfc) => Ord (WithRFC (rfc :: ValidationRFC) (r :: ValidationRFC -> Type))
deriving instance Typeable (r rfc) => Typeable (WithRFC (rfc :: ValidationRFC) (r :: ValidationRFC -> Type))
    
deriving instance Show (r 'Strict) => 
                  Show (r 'Reconsidered) =>
                  Show (AnRFC (r :: ValidationRFC -> Type))
deriving instance Eq (r 'Strict) => 
                  Eq (r 'Reconsidered) => 
                  Eq (AnRFC (r :: ValidationRFC -> Type))
deriving instance Ord (r 'Strict) => 
                  Ord (r 'Reconsidered) => 
                  Ord (AnRFC (r :: ValidationRFC -> Type))
deriving instance Typeable (r 'Strict) => 
                  Typeable (r 'Reconsidered) => 
                  Typeable (AnRFC (r :: ValidationRFC -> Type))


newtype ResourceCert = ResourceCert (AnRFC ResourceCertificate)
    deriving (Show, Eq, Ord, Typeable)

newtype IpResources = IpResources (AnRFC IpResourceSet)    
    deriving (Show, Eq, Ord, Typeable)

newtype RSet r = RSet (AnRFC (ResourceSet r))
    deriving (Show, Eq, Ord, Typeable)

data ResourceSet r (rfc :: ValidationRFC) = RS (S.Set r) | Inherit

deriving instance Show r => Show (ResourceSet r rfc)
deriving instance Eq r => Eq (ResourceSet r rfc)
deriving instance Ord r => Ord (ResourceSet r rfc)
deriving instance Typeable r => Typeable (ResourceSet r rfc)


data IpResourceSet (rfc :: ValidationRFC) = 
    IpResourceSet !(ResourceSet (IpResource 'Ipv4F) rfc)
                  !(ResourceSet (IpResource 'Ipv6F) rfc)
    deriving (Show, Eq, Ord, Typeable)                    


data HashAlg = SHA256 | SHA512 
    deriving (Show, Eq, Ord, Typeable)     

data Hash = Hash HashAlg B.ByteString deriving (Show, Eq, Ord, Typeable)

newtype URI  = URI T.Text deriving (Show, Eq, Ord, Typeable)
newtype KI   = KI  B.ByteString deriving (Show, Eq, Ord, Typeable)
newtype SKI  = SKI KI deriving (Show, Eq, Ord, Typeable)
newtype AKI  = AKI KI deriving (Show, Eq, Ord, Typeable)

newtype Serial = Serial Integer deriving (Show, Eq, Ord, Typeable)


-- | Objects

data ObjectType = CER | MFT | CRL | ROA | GBR
  deriving (Show, Eq, Typeable)

class (Show a, Eq a, Typeable a) => RpkiObject a where
    meta :: a -> RpkiMeta 

data CerObject = CerObject RpkiMeta ResourceCert 
    deriving (Show, Eq, Ord, Typeable)
data MftObject ref = MftObject RpkiMeta (Manifest ref) 
    deriving (Show, Eq, Ord, Typeable)
data CrlObject = CrlObject RpkiMeta Crl deriving (Show, Eq, Ord, Typeable)
data RoaObject = RoaObject RpkiMeta Roa deriving (Show, Eq, Ord, Typeable)
data GbrObject = GbrObject RpkiMeta Gbr deriving (Show, Eq, Ord, Typeable)

instance RpkiObject CerObject where
    meta (CerObject m _) = m

instance (Show r, Eq r, Typeable r) => RpkiObject (MftObject r) where
    meta (MftObject m _) = m    

instance RpkiObject CrlObject where
    meta (CrlObject m _) = m    
    
instance RpkiObject RoaObject where
    meta (RoaObject m _) = m    
    
instance RpkiObject GbrObject where
    meta (GbrObject m _) = m    
    

data RpkiMeta = RpkiMeta {
    locations :: NonEmpty URI
  , hash      :: !Hash
  , aki       :: !(Maybe AKI)
  , ski       :: !SKI
  , serial    :: !Serial
} deriving (Show, Eq, Ord, Typeable)

newtype Blob = Blob B.ByteString 
  deriving (Show, Eq, Ord, Typeable)

newtype RefHash = RefHash Hash
    deriving (Show, Eq, Ord, Typeable)

data RefResolved = RefMft (MftObject ARef)
                 | RefCrl CrlObject
                 | RefCer CerObject
                 | RefRoa RoaObject
                 | RefGbr GbrObject
                 deriving (Show, Eq, Ord, Typeable)


data ARef = RH RefHash | RR RefResolved
    deriving (Show, Eq, Ord, Typeable)


data ResourceCertificate (rfc :: ValidationRFC) = ResourceCertificate {
    certX509    :: !X509.Certificate 
  , ipResources :: !(Maybe (IpResourceSet rfc))
  , asResources :: !(Maybe (ResourceSet AsResource rfc))
} deriving (Show, Eq, Typeable)

instance Ord (ResourceCertificate (rfc :: ValidationRFC)) where
    compare c1 c2 = LT

newtype EECert = EECert X509.Certificate deriving (Show, Eq, Typeable)    

data Roa = Roa     
    !ASN 
    !APrefix    
    {-# UNPACK #-} !Int
    deriving (Show, Eq, Ord, Typeable)

newtype CRLRef = CRLRef (Serial, DateTime) deriving (Show, Eq, Ord, Typeable)


data Manifest r = Manifest {
    mftNumber   :: !Int  
  , fileHashAlg :: !HashAlg
  , thisTime    :: !DateTime
  , nextTime    :: !DateTime 
  , mftEntries  :: ![(T.Text, r)]
} deriving (Show, Eq, Ord, Typeable)

data Crl = Crl {
    entries :: [RpkiObj]
} deriving (Show, Eq, Ord, Typeable)

data Gbr = Gbr deriving (Show, Eq, Ord, Typeable)


data RpkiObj = RpkiObj !ObjId !RpkiMeta
    deriving (Show, Eq, Ord, Typeable)

-- Id of the object in the object store
newtype ObjId = ObjId B.ByteString deriving (Show, Eq, Ord, Typeable)


-- Subject Public Key Info
newtype SPKI = SPKI B.ByteString

newtype TaName = TaName T.Text

data TA = TA {
    taName        :: !TaName
  , taCertificate :: !ResourceCert
  , taUri         :: !URI
  , taSpki        :: !SPKI
}

data Repository = Repository {
    repoRsyncUrl :: !URI
  , repoRrdpUrl  :: !URI
}


-- data CA = CA {
--     caName :: T.Text
--   , caCertificate :: ResourceCert   
-- }

newtype Message = Message TS.ShortText deriving (Show, Eq, Ord, Typeable)

data Invalid = Error | Warning


-- TODO Make it better
hashAlg :: O.OID -> HashAlg
hashAlg _ = SHA256
        

-- Validation errors

data VError = InvalidCert T.Text |
              ParentDoesntHaveResources |
              NoAKIinManifest  
    deriving (Show, Eq, Ord, Typeable)
    