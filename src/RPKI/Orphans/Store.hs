{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass    #-}

module RPKI.Orphans.Store where

import qualified Data.ByteString as BS

import Data.ByteArray (convert)
import Data.Store

import Data.ASN1.Types

import Data.X509 as X509

import Data.Hourglass
import Data.ASN1.BitArray

import Data.Tuple.Strict
import Data.Set.NonEmpty

import Crypto.Error
import Crypto.PubKey.RSA.Types (PublicKey(..))
import Crypto.PubKey.DSA (PublicKey(..), Params(..))

import qualified Crypto.PubKey.Curve25519 as C25519
import qualified Crypto.PubKey.Ed25519 as E25519
import qualified Crypto.PubKey.Curve448 as C448
import qualified Crypto.PubKey.Ed448 as E448

import Crypto.PubKey.ECC.Types

import RPKI.Orphans.Generics

instance Store X509.Certificate
instance Store X509.CRL
instance Store X509.RevokedCertificate
instance Store a => Store (X509.SignedExact a)    
instance Store a => Store (X509.Signed a) 
    
instance Store SignatureALG

instance Store DateTime
instance Store Date
instance Store TimeOfDay
instance Store Month
instance Store Hours
instance Store Minutes
instance Store Seconds
instance Store NanoSeconds
instance Store TimezoneOffset

instance Store ASN1
instance Store DistinguishedName
instance Store PubKey
instance Store PubKeyEC
instance Store PubKeyALG
instance Store Extensions
instance Store ExtensionRaw
instance Store HashALG

instance Store Crypto.PubKey.RSA.Types.PublicKey
instance Store Crypto.PubKey.DSA.PublicKey
instance Store Crypto.PubKey.DSA.Params

instance Store C25519.PublicKey where    
    encode = encodeBytes . convert
    decode = decodePK C25519.publicKey

instance Store E25519.PublicKey where
    encode = encodeBytes . convert
    decode = decodePK E25519.publicKey

instance Store C448.PublicKey where
    encode = encodeBytes . convert
    decode = decodePK C448.publicKey

instance Store E448.PublicKey where
    encode = encodeBytes . convert
    decode = decodePK E448.publicKey

instance Store BitArray
instance Store ASN1CharacterString
instance Store ASN1StringEncoding
instance Store ASN1TimeType
instance Store ASN1Class
instance Store ASN1ConstructionType
instance Store SerializedPoint
instance Store Crypto.PubKey.ECC.Types.CurveName

decodePK :: (BS.ByteString -> CryptoFailable b) -> Decoder s b
decodePK f = f <$> decodeBytes >>= \case 
    CryptoPassed p -> pure p
    CryptoFailed e -> fail $ show e


deriving instance (Store a, Store b) => Store (T2 a b)
deriving instance (Store a, Store b, Store c) => Store (T3 a b c)

deriving instance (Ord a, Store a) => Store (NESet a)