{-# OPTIONS_GHC -fno-warn-orphans -Wno-type-defaults #-}
{-# LANGUAGE DeriveAnyClass      #-}

module RPKI.Orphans.NFData where

import Control.DeepSeq

import Data.ASN1.Types
import Data.X509 as X509
import Data.ASN1.BitArray

import Crypto.PubKey.ECC.Types

import RPKI.Orphans.Generics

deriving instance NFData HashALG

deriving instance NFData BitArray
deriving instance NFData ASN1StringEncoding
deriving instance NFData ASN1CharacterString
deriving instance NFData ASN1TimeType
deriving instance NFData ASN1Class
deriving instance NFData ASN1ConstructionType
deriving instance NFData SerializedPoint
deriving instance NFData Crypto.PubKey.ECC.Types.CurveName
deriving instance NFData ASN1
deriving instance NFData DistinguishedName

deriving instance NFData PubKeyEC
deriving instance NFData PubKeyALG
deriving instance NFData PubKey
deriving instance NFData ExtensionRaw
deriving instance NFData Extensions
deriving instance NFData SignatureALG

deriving instance NFData X509.Certificate
deriving instance NFData X509.RevokedCertificate
deriving instance NFData X509.CRL

deriving instance NFData a => NFData (X509.Signed a)
deriving instance NFData a => NFData (X509.SignedExact a)

