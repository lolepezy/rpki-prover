{-# LANGUAGE DeriveDataTypeable #-}

module RPKI.Types where

import qualified Data.Set as S
import qualified Data.ByteString as B
import qualified Data.Text as T

import Data.Data (Data, Typeable)

data Ipv4Prefix = Ipv4Prefix
    deriving (Show, Eq, Ord, Data, Typeable)
data Ipv6Prefix = Ipv6Prefix
    deriving (Show, Eq, Ord, Data, Typeable)

data IpPrefix = Ipv4 Ipv4Prefix | Ipv6 Ipv6Prefix
    deriving (Show, Eq, Ord, Data, Typeable)

newtype ASN = ASN Int
    deriving (Show, Eq, Ord, Data, Typeable)

data Resource = IpR IpPrefix | AS ASN
    deriving (Show, Eq, Ord, Data, Typeable)


newtype URI  = URI T.Text deriving (Show, Eq, Ord, Data, Typeable)
newtype Hash = Hash B.ByteString deriving (Show, Eq, Ord, Data, Typeable)
newtype SKI  = SKI  B.ByteString deriving (Show, Eq, Ord, Data, Typeable)
newtype AKI  = AKI  B.ByteString deriving (Show, Eq, Ord, Data, Typeable)

newtype Serial  = Serial Integer deriving (Show, Eq, Ord, Data, Typeable)

-- don't know yet
data RealCert = RealCert deriving (Show, Eq, Ord, Data, Typeable)
data RealRoa = RealRoa deriving (Show, Eq, Ord, Data, Typeable)
data RealCrl = RealCrl deriving (Show, Eq, Ord, Data, Typeable)
data RealMft = RealMft deriving (Show, Eq, Ord, Data, Typeable)

data SignedObj = SignedObj {
    locations :: [URI]
  , hash      :: Hash 
  , aki       :: AKI 
  , serial    :: Serial 
} deriving (Show, Eq, Ord, Data, Typeable)

data Cert = Cert SignedObj RealCert (S.Set Resource)
    deriving (Show, Eq, Ord, Data, Typeable)
data ROA = Roa SignedObj RealRoa IpPrefix ASN
    deriving (Show, Eq, Ord, Data, Typeable)
data CRL = CRL SignedObj RealCrl
    deriving (Show, Eq, Ord, Data, Typeable)
data MFT = MFT SignedObj RealMft
    deriving (Show, Eq, Ord, Data, Typeable)

data RpkiUnit = Cu Cert | Mu MFT | Cru CRL | Ru ROA
    deriving (Show, Eq, Ord, Data, Typeable)