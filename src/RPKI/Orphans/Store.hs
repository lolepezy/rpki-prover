{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RPKI.Orphans.Store where

import qualified Data.ByteString as BS

import Data.Store
-- import Data.Store.TH
import TH.Derive

import Data.ByteArray (convert)

import Data.ASN1.Types

import Data.These

import Data.X509 as X509

import Data.Hourglass
import Data.ASN1.BitArray

import Data.Tuple.Strict
import Data.Set.NonEmpty
import Data.Map.Monoidal.Strict

import Crypto.Error
import Crypto.PubKey.RSA.Types (PublicKey(..))
import Crypto.PubKey.DSA (PublicKey(..), Params(..))

import qualified Crypto.PubKey.Curve25519 as C25519
import qualified Crypto.PubKey.Ed25519 as E25519
import qualified Crypto.PubKey.Curve448 as C448
import qualified Crypto.PubKey.Ed448 as E448

import Crypto.PubKey.ECC.Types


peekPK :: (BS.ByteString -> CryptoFailable b) -> Peek b
peekPK f = do 
    bs <- peek
    case f bs of     
        CryptoPassed p -> pure p
        CryptoFailed e -> fail $ show e

$($(derive [d|instance Deriving (Store HashALG)|]))
$($(derive [d|instance Deriving (Store Month)|]))
$($(derive [d|instance Deriving (Store Hours)|]))
$($(derive [d|instance Deriving (Store Minutes)|]))
$($(derive [d|instance Deriving (Store Seconds)|]))
$($(derive [d|instance Deriving (Store NanoSeconds)|]))
$($(derive [d|instance Deriving (Store TimezoneOffset)|]))
$($(derive [d|instance Deriving (Store TimeOfDay)|]))
$($(derive [d|instance Deriving (Store Date)|]))
$($(derive [d|instance Deriving (Store DateTime)|]))

$($(derive [d|instance Deriving (Store Crypto.PubKey.DSA.Params)|]))
$($(derive [d|instance Deriving (Store Crypto.PubKey.RSA.Types.PublicKey)|]))
$($(derive [d|instance Deriving (Store Crypto.PubKey.DSA.PublicKey)|]))


instance Store C25519.PublicKey where    
    size = ConstSize 32
    poke pk = let bs = convert pk :: BS.ByteString in poke bs    
    peek = peekPK C25519.publicKey

instance Store E25519.PublicKey where
    size = ConstSize $ E25519.publicKeySize
    poke pk = let bs = convert pk :: BS.ByteString in poke bs    
    peek = peekPK E25519.publicKey

instance Store C448.PublicKey where
    -- https://github.com/haskell-crypto/cryptonite/blob/master/Crypto/PubKey/Curve448.hs#L105
    size = ConstSize 56
    poke pk = let bs = convert pk :: BS.ByteString in poke bs    
    peek = peekPK C448.publicKey

instance Store E448.PublicKey where
    size = ConstSize $ E448.publicKeySize
    poke pk = let bs = convert pk :: BS.ByteString in poke bs    
    peek = peekPK E448.publicKey

$($(derive [d|instance Deriving (Store BitArray)|]))
$($(derive [d|instance Deriving (Store ASN1StringEncoding)|]))
$($(derive [d|instance Deriving (Store ASN1CharacterString)|]))
$($(derive [d|instance Deriving (Store ASN1TimeType)|]))
$($(derive [d|instance Deriving (Store ASN1Class)|]))
$($(derive [d|instance Deriving (Store ASN1ConstructionType)|]))
$($(derive [d|instance Deriving (Store SerializedPoint)|]))
$($(derive [d|instance Deriving (Store Crypto.PubKey.ECC.Types.CurveName)|]))
$($(derive [d|instance Deriving (Store ASN1)|]))
$($(derive [d|instance Deriving (Store DistinguishedName)|]))

$($(derive [d|instance Deriving (Store PubKeyEC)|]))
$($(derive [d|instance Deriving (Store PubKeyALG)|]))
$($(derive [d|instance Deriving (Store PubKey)|]))
$($(derive [d|instance Deriving (Store ExtensionRaw)|]))
$($(derive [d|instance Deriving (Store Extensions)|]))
$($(derive [d|instance Deriving (Store SignatureALG)|]))

$($(derive [d|instance Deriving (Store X509.Certificate)|]))
$($(derive [d|instance Deriving (Store X509.RevokedCertificate)|]))
$($(derive [d|instance Deriving (Store X509.CRL)|]))
$($(derive [d|instance Store a => Deriving (Store (X509.Signed a))|]))
$($(derive [d|instance Store a => Deriving (Store (X509.SignedExact a))|]))


$($(derive [d|instance (Store a, Store b) => Deriving (Store (T2 a b))|]))
$($(derive [d|instance (Store a, Store b, Store c) => Deriving (Store (T3 a b c))|]))

$($(derive [d|instance (Ord a, Store a) => Deriving (Store (NESet a))|]))

$($(derive [d|instance (Ord a, Store a, Store b) => Deriving (Store (MonoidalMap a b))|]))

$($(derive [d|instance (Store a, Store b) => Deriving (Store (These a b))|]))


