{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module RPKI.Serialise.StoreOrphans where

import Data.Store

import qualified Data.ByteString as B
import Data.ByteArray (convert)

import Data.ASN1.Types

import Data.X509 as X509

import Data.Hourglass
import Data.ASN1.BitArray
import Crypto.Error
import Crypto.PubKey.RSA.Types (PublicKey(..))
import Crypto.PubKey.DSA (PublicKey(..), Params(..))
import qualified Crypto.PubKey.Curve25519 as C25519
import qualified Crypto.PubKey.Ed25519 as E25519
import qualified Crypto.PubKey.Curve448 as C448
import qualified Crypto.PubKey.Ed448 as E448

import Crypto.PubKey.ECC.Types
import RPKI.Serialise.Orphans

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
    size = ConstSize 32
    poke = pokePK
    peek = peekPK C25519.publicKey        
    
instance Store E25519.PublicKey where
    size = ConstSize 32
    poke = pokePK
    peek = peekPK E25519.publicKey

instance Store C448.PublicKey where
    size = ConstSize 56
    poke = pokePK
    peek = peekPK C448.publicKey    

instance Store E448.PublicKey where
    size = ConstSize 56
    poke = pokePK
    peek = peekPK E448.publicKey    

instance Store BitArray
instance Store ASN1CharacterString
instance Store ASN1StringEncoding
instance Store ASN1TimeType
instance Store ASN1Class
instance Store ASN1ConstructionType
instance Store SerializedPoint
instance Store Crypto.PubKey.ECC.Types.CurveName

peekPK f = f <$> (peek @B.ByteString) >>= \case 
    CryptoPassed p -> pure p
    CryptoFailed e -> fail $ show e

pokePK pk = poke bs
    where 
        bs :: B.ByteString = convert pk