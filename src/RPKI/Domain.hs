{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module RPKI.Domain where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Short as TS

import Data.Data (Typeable)

import HaskellWorks.Data.Network.Ip.Range 
import HaskellWorks.Data.Network.Ip.Ipv4 as V4
import HaskellWorks.Data.Network.Ip.Ipv6 as V6


data Ipv4Prefix = Ipv4P (Range V4.IpAddress)
    deriving (Show, Eq, Ord, Typeable)
data Ipv6Prefix = Ipv6P (Range V6.IpAddress)
    deriving (Show, Eq, Ord, Typeable)

data IpPrefix = Ipv4 !Ipv4Prefix | Ipv6 !Ipv6Prefix
    deriving (Show, Eq, Ord, Typeable)

newtype ASN = ASN Int
    deriving (Show, Eq, Ord, Typeable)

data Resource = IpR !IpPrefix | AS !ASN
    deriving (Show, Eq, Ord, Typeable)

data MFTEntry = MFTEntry B.ByteString
    deriving (Show, Eq, Ord, Typeable)

newtype URI  = URI T.Text deriving (Show, Eq, Ord, Typeable)
newtype Hash = Hash B.ByteString deriving (Show, Eq, Ord, Typeable)
newtype KI   = KI  B.ByteString deriving (Show, Eq, Ord, Typeable)
newtype SKI  = SKI KI deriving (Show, Eq, Ord, Typeable)
newtype AKI  = AKI KI deriving (Show, Eq, Ord, Typeable)

newtype Serial  = Serial Integer deriving (Show, Eq, Ord, Typeable)

-- don't know yet
data RealCert = RealCert deriving (Show, Eq, Ord, Typeable)
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
