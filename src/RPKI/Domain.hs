{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module RPKI.Domain where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Short as TS

import Data.Data (Typeable)

import Data.Word

import HaskellWorks.Data.Network.Ip.Validity
import HaskellWorks.Data.Network.Ip.Range
import HaskellWorks.Data.Network.Ip.Word128
import HaskellWorks.Data.Network.Ip.Ipv4 as V4
import HaskellWorks.Data.Network.Ip.Ipv6 as V6

newtype Ipv4Prefix = Ipv4Prefix (V4.IpBlock Canonical) 
    deriving (Show, Eq, Ord, Typeable)
newtype Ipv6Prefix = Ipv6Prefix (V6.IpBlock Canonical) 
    deriving (Show, Eq, Ord, Typeable)

newtype Ipv4Range = Ipv4Range (Range V4.IpAddress) 
    deriving (Show, Eq, Ord, Typeable)
newtype Ipv6Range = Ipv6Range (Range V6.IpAddress) 
    deriving (Show, Eq, Ord, Typeable)

data IpPrefix = Ipv4P !Ipv4Prefix | Ipv6P !Ipv6Prefix
    deriving (Show, Eq, Ord, Typeable)

data IpRange = Ipv4R !Ipv4Range | Ipv6R !Ipv6Range
    deriving (Show, Eq, Ord, Typeable)

newtype ASN = ASN Int
    deriving (Show, Eq, Ord, Typeable)

data Resource = IpP !IpPrefix 
              | IpR !IpRange 
              | AS  !ASN
              | ASRange !ASN !ASN
    deriving (Show, Eq, Ord, Typeable)

newtype MFTEntry = MFTEntry B.ByteString
    deriving (Show, Eq, Ord, Typeable)

newtype URI  = URI T.Text deriving (Show, Eq, Ord, Typeable)
newtype Hash = Hash B.ByteString deriving (Show, Eq, Ord, Typeable)
newtype KI   = KI  B.ByteString deriving (Show, Eq, Ord, Typeable)
newtype SKI  = SKI KI deriving (Show, Eq, Ord, Typeable)
newtype AKI  = AKI KI deriving (Show, Eq, Ord, Typeable)

newtype Serial  = Serial Integer deriving (Show, Eq, Ord, Typeable)

-- don't know yet
data RealCert = RealCert B.ByteString 
    deriving (Show, Eq, Ord, Typeable)
data RealRoa = RealRoa deriving (Show, Eq, Ord, Typeable)
data RealCrl = RealCrl deriving (Show, Eq, Ord, Typeable)
data RealMft = RealMft deriving (Show, Eq, Ord, Typeable)

data SignedObj = SignedObj {
    locations :: ![URI]
  , hash      :: !Hash
  , aki       :: !AKI
  , serial    :: !Serial
} deriving (Show, Eq, Ord, Typeable)

data Cert = Cert !RealCert !SKI !(S.Set Resource)
    deriving (Show, Eq, Ord, Typeable)
data ROA = ROA !RealRoa !IpPrefix !ASN
    deriving (Show, Eq, Ord, Typeable)
data CRL = CRL !RealCrl
    deriving (Show, Eq, Ord, Typeable)
data MFT = MFT !RealMft
    deriving (Show, Eq, Ord, Typeable)

data RpkiUnit = Cu !Cert | Mu !MFT | Cru !CRL | Ru !ROA
    deriving (Show, Eq, Ord, Typeable)

data RpkiObj = RpkiObj !SignedObj !RpkiUnit
    deriving (Show, Eq, Ord, Typeable)

-- Subject Public Key Info
newtype SPKI = SPKI B.ByteString

data TA = TA {
    name        :: !T.Text
  , certificate :: !Cert
  , uri         :: !URI
  , spki        :: !SPKI
}

data Repository = Repository {
    rsyncUrl :: !URI
  , rrdpUrl  :: !URI
}

newtype Message = Message TS.ShortText deriving (Show, Eq, Ord, Typeable)

data Invalid = Error | Warning

mftEntries :: MFT -> M.Map String MFTEntry
-- TODO Implement
mftEntries _ = M.empty


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

