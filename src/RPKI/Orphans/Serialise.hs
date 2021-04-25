{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass    #-}

module RPKI.Orphans.Serialise where

import qualified Data.ByteString as BS

import Codec.Serialise
import Codec.CBOR.Encoding
import Codec.CBOR.Decoding

import Data.ByteArray (convert)

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

instance Serialise X509.Certificate
instance Serialise X509.CRL
instance Serialise X509.RevokedCertificate
instance Serialise a => Serialise (X509.SignedExact a)    
instance Serialise a => Serialise (X509.Signed a) 
    
instance Serialise SignatureALG

instance Serialise DateTime
instance Serialise Date
instance Serialise TimeOfDay
instance Serialise Month
instance Serialise Hours
instance Serialise Minutes
instance Serialise Seconds
instance Serialise NanoSeconds
instance Serialise TimezoneOffset

instance Serialise ASN1
instance Serialise DistinguishedName
instance Serialise PubKey
instance Serialise PubKeyEC
instance Serialise PubKeyALG
instance Serialise Extensions
instance Serialise ExtensionRaw
instance Serialise HashALG

instance Serialise Crypto.PubKey.RSA.Types.PublicKey
instance Serialise Crypto.PubKey.DSA.PublicKey
instance Serialise Crypto.PubKey.DSA.Params

instance Serialise C25519.PublicKey where    
    encode = encodeBytes . convert
    decode = decodePK C25519.publicKey

instance Serialise E25519.PublicKey where
    encode = encodeBytes . convert
    decode = decodePK E25519.publicKey

instance Serialise C448.PublicKey where
    encode = encodeBytes . convert
    decode = decodePK C448.publicKey

instance Serialise E448.PublicKey where
    encode = encodeBytes . convert
    decode = decodePK E448.publicKey

instance Serialise BitArray
instance Serialise ASN1CharacterString
instance Serialise ASN1StringEncoding
instance Serialise ASN1TimeType
instance Serialise ASN1Class
instance Serialise ASN1ConstructionType
instance Serialise SerializedPoint
instance Serialise Crypto.PubKey.ECC.Types.CurveName

decodePK :: (BS.ByteString -> CryptoFailable b) -> Decoder s b
decodePK f = f <$> decodeBytes >>= \case 
    CryptoPassed p -> pure p
    CryptoFailed e -> fail $ show e


deriving instance (Serialise a, Serialise b) => Serialise (T2 a b)
deriving instance (Serialise a, Serialise b, Serialise c) => Serialise (T3 a b c)

deriving instance (Ord a, Serialise a) => Serialise (NESet a)