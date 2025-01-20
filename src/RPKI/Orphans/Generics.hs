{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module RPKI.Orphans.Generics where

import Data.ASN1.Types

import GHC.Generics

import Data.Hourglass

import Data.Set.NonEmpty
import Data.Set.NonEmpty.Internal
import Data.Map.Monoidal.Strict
import Data.ASN1.BitArray
import Crypto.PubKey.RSA.Types (PublicKey(..))
import Crypto.PubKey.DSA (PublicKey(..), Params(..))

import Crypto.PubKey.ECC.Types
    
import qualified Data.X509                as X509

deriving instance Generic DateTime
deriving instance Generic Date
deriving instance Generic TimeOfDay
deriving instance Generic TimezoneOffset
deriving instance Generic Month
deriving instance Generic Hours
deriving instance Generic Minutes
deriving instance Generic Seconds
deriving instance Generic NanoSeconds

deriving instance Generic ASN1

deriving instance Generic Crypto.PubKey.RSA.Types.PublicKey
deriving instance Generic Crypto.PubKey.DSA.PublicKey
deriving instance Generic Crypto.PubKey.DSA.Params

deriving instance Generic ASN1CharacterString
deriving instance Generic ASN1StringEncoding
deriving instance Generic ASN1TimeType
deriving instance Generic ASN1Class
deriving instance Generic ASN1ConstructionType
deriving instance Generic Crypto.PubKey.ECC.Types.CurveName

deriving instance Generic BitArray
deriving instance Generic (NESet a)
deriving instance Generic (MonoidalMap a b)

-- deriving instance Generic X509.Certificate
-- deriving instance Generic X509.SignatureALG
-- deriving instance Generic X509.HashALG
-- deriving instance Generic X509.SerializedPoint
-- deriving instance Generic X509.DistinguishedName
-- deriving instance Generic X509.PubKeyEC
-- deriving instance Generic X509.PubKeyALG
-- deriving instance Generic X509.PubKey
-- deriving instance Generic X509.ExtensionRaw
-- deriving instance Generic X509.Extensions
-- deriving instance Generic X509.RevokedCertificate
-- deriving instance Generic X509.CRL

-- deriving instance Generic X509.SignedCertificate
-- deriving instance Generic X509.SignedCRL

