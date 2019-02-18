{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}

module RPKI.Domain where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Short as TS

import Data.Data (Typeable)

import Data.Word

import Data.Hourglass

import HaskellWorks.Data.Network.Ip.Validity
import HaskellWorks.Data.Network.Ip.Range
import HaskellWorks.Data.Network.Ip.Word128
import HaskellWorks.Data.Network.Ip.Ipv4 as V4
import HaskellWorks.Data.Network.Ip.Ipv6 as V6

import qualified Data.X509 as X509

data AddrFamily = Ipv4F | Ipv6F
    deriving (Show, Eq, Ord, Typeable)

newtype Ipv4Prefix = Ipv4Prefix (V4.IpBlock Canonical) 
    deriving (Show, Eq, Ord, Typeable)
newtype Ipv6Prefix = Ipv6Prefix (V6.IpBlock Canonical) 
    deriving (Show, Eq, Ord, Typeable)

newtype Ipv4Range = Ipv4Range (Range V4.IpAddress) 
    deriving (Show, Eq, Ord, Typeable)
newtype Ipv6Range = Ipv6Range (Range V6.IpAddress) 
    deriving (Show, Eq, Ord, Typeable)

data IpPrefix (f :: AddrFamily) where
    Ipv4P :: !Ipv4Prefix -> IpPrefix 'Ipv4F
    Ipv6P :: !Ipv6Prefix -> IpPrefix 'Ipv6F

deriving instance Show (IpPrefix f)
deriving instance Eq (IpPrefix f)
deriving instance Ord (IpPrefix f)
deriving instance Typeable (IpPrefix f)

data IpRange (f :: AddrFamily) where
    Ipv4R :: !Ipv4Range -> IpRange 'Ipv4F
    Ipv6R :: !Ipv6Range -> IpRange 'Ipv6F

deriving instance Show (IpRange f)
deriving instance Eq (IpRange f)
deriving instance Ord (IpRange f)
deriving instance Typeable (IpRange f)
    

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

newtype MFTEntry = MFTEntry B.ByteString
    deriving (Show, Eq, Ord, Typeable)

newtype URI  = URI T.Text deriving (Show, Eq, Ord, Typeable)
newtype Hash = Hash B.ByteString deriving (Show, Eq, Ord, Typeable)
newtype KI   = KI  B.ByteString deriving (Show, Eq, Ord, Typeable)
newtype SKI  = SKI KI deriving (Show, Eq, Ord, Typeable)
newtype AKI  = AKI KI deriving (Show, Eq, Ord, Typeable)

newtype Serial  = Serial Integer deriving (Show, Eq, Ord, Typeable)

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

-- TODO Probably make ROA polymorphic by the address family
data ROA = ROA {-# UNPACK #-} !APrefix 
               {-# UNPACK #-} !ASN 
               {-# UNPACK #-} !Int
    deriving (Show, Eq, Ord, Typeable)


newtype MFTRef = MFTRef (Either Hash ObjId) deriving (Show, Eq, Ord, Typeable)

newtype CRLRef = CRLRef (Serial, DateTime) deriving (Show, Eq, Ord, Typeable)

data MFT = MFT {
    mftMeta    :: !RpkiMeta
  , mftEntries :: ![(T.Text, MFTRef)]
} deriving (Show, Eq, Typeable)

data CRL = CRL {
    crlMeta    :: !RpkiMeta
  , crlEntries :: [RpkiObj]
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

mkIpv4Block :: Word32 -> Word8 -> Ipv4Prefix
mkIpv4Block w32 nonZeroBits = Ipv4Prefix (V4.IpBlock (V4.IpAddress w32) (V4.IpNetMask nonZeroBits))

mkIpv4 :: Word32 -> Word32 -> Either Ipv4Range Ipv4Prefix
mkIpv4 w1 w2 = 
    let r = Range (V4.IpAddress w1) (V4.IpAddress w2) 
    in case V4.rangeToBlocks r of
        [b]     -> Right $ Ipv4Prefix b
        b1 : bs -> Left $ Ipv4Range r        

mkIpv6Block :: (Word32, Word32, Word32, Word32) -> Word8 -> Ipv6Prefix
mkIpv6Block w128 nonZeroBits = Ipv6Prefix (V6.IpBlock (V6.IpAddress w128) (V6.IpNetMask nonZeroBits))

mkIpv6 :: Word128 -> Word128 -> Either Ipv6Range Ipv6Prefix
mkIpv6 w1 w2 = 
    let r = Range (V6.IpAddress w1) (V6.IpAddress w2) 
    in case V6.rangeToBlocks r of
        [b]     -> Right $ Ipv6Prefix b
        b1 : bs -> Left $ Ipv6Range r

