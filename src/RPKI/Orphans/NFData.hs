{-# OPTIONS_GHC -fno-warn-orphans -Wno-type-defaults #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

module RPKI.Orphans.NFData where

import Control.DeepSeq

import Data.ASN1.Types
import Data.X509 as X509
import Data.ASN1.BitArray

import Crypto.PubKey.ECC.Types

import RPKI.Orphans.Generics

-- deriving anyclass instance NFData HashALG

-- deriving anyclass instance NFData BitArray
-- deriving anyclass instance NFData ASN1StringEncoding
-- deriving anyclass instance NFData ASN1CharacterString
-- deriving anyclass instance NFData ASN1TimeType
-- deriving anyclass instance NFData ASN1Class
-- deriving anyclass instance NFData ASN1ConstructionType
-- deriving anyclass instance NFData SerializedPoint
-- deriving anyclass instance NFData Crypto.PubKey.ECC.Types.CurveName
-- deriving anyclass instance NFData ASN1
-- deriving anyclass instance NFData DistinguishedName

-- deriving anyclass instance NFData PubKeyEC
-- deriving anyclass instance NFData PubKeyALG
-- deriving anyclass instance NFData PubKey
-- deriving anyclass instance NFData ExtensionRaw
-- deriving anyclass instance NFData Extensions
-- deriving anyclass instance NFData SignatureALG

-- deriving anyclass instance NFData X509.Certificate
-- deriving anyclass instance NFData X509.RevokedCertificate
-- deriving anyclass instance NFData X509.CRL

-- deriving anyclass instance NFData a => NFData (X509.Signed a)
-- deriving anyclass instance NFData a => NFData (X509.SignedExact a)

