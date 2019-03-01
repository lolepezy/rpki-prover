{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

module RPKI.Domain where

import qualified Data.Set as S
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Short as TS

import Data.Data (Typeable)

import Data.Word

import Data.Hourglass

import qualified Data.X509 as X509
import qualified Data.ASN1.OID as O

import RPKI.IP    

newtype ASN = ASN Int
    deriving (Show, Eq, Ord, Typeable)

data IpResource (f :: AddrFamily) where
    IpP :: !(IpPrefix f) -> IpResource f
    IpR :: !(IpRange f ) -> IpResource f

deriving instance Show (IpResource f)
deriving instance Eq (IpResource f)
deriving instance Ord (IpResource f)
deriving instance Typeable (IpResource f)
    

data AsResource =  AS !ASN
                 | ASRange  
                    {-# UNPACK #-} !ASN 
                    {-# UNPACK #-} !ASN
    deriving (Show, Eq, Ord, Typeable)                    

data ValidationRFC = Strict | Reconsidered

data ResourceSet r (rfc :: ValidationRFC) = RS (S.Set r) | Inherit

deriving instance Show r => Show (ResourceSet r rfc)
deriving instance Eq r => Eq (ResourceSet r rfc)
deriving instance Ord r => Ord (ResourceSet r rfc)
deriving instance Typeable r => Typeable (ResourceSet r rfc)


data IpResourceSet (rfc :: ValidationRFC) = 
    IpResourceSet !(ResourceSet (IpResource 'Ipv4F) rfc)
                  !(ResourceSet (IpResource 'Ipv6F) rfc)
    deriving (Show, Eq, Ord, Typeable)                    
    
-- | Objects

data HashAlg = SHA256 | SHA512 
    deriving (Show, Eq, Ord, Typeable)     

data Hash = Hash HashAlg B.ByteString deriving (Show, Eq, Ord, Typeable)

newtype URI  = URI T.Text deriving (Show, Eq, Ord, Typeable)
newtype KI   = KI  B.ByteString deriving (Show, Eq, Ord, Typeable)
newtype SKI  = SKI KI deriving (Show, Eq, Ord, Typeable)
newtype AKI  = AKI KI deriving (Show, Eq, Ord, Typeable)

newtype Serial = Serial Integer deriving (Show, Eq, Ord, Typeable)

data RpkiMeta = RpkiMeta {
    locations :: ![URI]
  , hash      :: !Hash
  , aki       :: !AKI
  , ski       :: !SKI
  , serial    :: !Serial
} deriving (Show, Eq, Ord, Typeable)

data Cert (rfc :: ValidationRFC) = Cert {
    certX509    :: !X509.Certificate 
  , ipResources :: !(IpResourceSet rfc)
  , asResources :: !(ResourceSet AsResource rfc)
} deriving (Show, Eq, Typeable)

newtype EECert = EECert X509.Certificate deriving (Show, Eq, Typeable)

data APrefix = AV4 !(IpPrefix 'Ipv4F) | AV6 !(IpPrefix 'Ipv6F)
    deriving (Show, Eq, Ord, Typeable)

data ROA = ROA     
    {-# UNPACK #-} !ASN 
    {-# UNPACK #-} !APrefix    
    {-# UNPACK #-} !Int
    deriving (Show, Eq, Ord, Typeable)


newtype MFTRef = MFTRef (Either Hash ObjId) deriving (Show, Eq, Ord, Typeable)

newtype CRLRef = CRLRef (Serial, DateTime) deriving (Show, Eq, Ord, Typeable)


data MFT = MFT {
    mftnumber   :: !Int  
  , fileHashAlg :: !HashAlg
  , thisTime    :: !DateTime
  , nextTime    :: !DateTime 
  , mftEntries  :: ![(T.Text, MFTRef)]
} deriving (Show, Eq, Typeable)

data CRL = CRL {
    crlEntries :: [RpkiObj]
} deriving (Show, Eq, Typeable)

data RpkiStorable = Cu !(Cert 'Strict) 
                  | CuV2 !(Cert 'Reconsidered) 
                  | Mu !MFT
                  | Cru !CRL 
                  | Ru !ROA
    deriving (Show, Eq, Typeable)

data RpkiObj = RpkiObj !ObjId !RpkiMeta
    deriving (Show, Eq, Ord, Typeable)

-- Id of the object in the object store
newtype ObjId = ObjId B.ByteString deriving (Show, Eq, Ord, Typeable)


-- Subject Public Key Info
newtype SPKI = SPKI B.ByteString

data TA = TA {
    taName        :: !T.Text
  , taCertificate :: !(Either (Cert 'Strict) (Cert 'Reconsidered))
  , taUri         :: !URI
  , taSpki        :: !SPKI
}

data Repository = Repository {
    repoRsyncUrl :: !URI
  , repoRrdpUrl  :: !URI
}

newtype Message = Message TS.ShortText deriving (Show, Eq, Ord, Typeable)

data Invalid = Error | Warning


-- TODO Make it better
hashAlg :: O.OID -> HashAlg
hashAlg _ = SHA256
        
