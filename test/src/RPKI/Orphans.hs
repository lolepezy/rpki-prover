{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module RPKI.Orphans where

import Control.Monad

import qualified Data.ByteString                       as B
import qualified Data.List                       as L
import qualified Data.ByteString.Base64                as B64

import qualified Data.List.NonEmpty as NE

import           Test.QuickCheck.Arbitrary.Generic
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Instances.ByteString
import           Test.QuickCheck.Instances.Vector
import           Test.QuickCheck.Instances.Text
import           Test.QuickCheck.Monadic

import           Data.ASN1.BitArray
import           Data.ASN1.Types
import           Data.X509                             as X509
import           Data.Bits
import           Data.Word

import           HaskellWorks.Data.Network.Ip.Ipv4     as V4
import           HaskellWorks.Data.Network.Ip.Ipv6     as V6
import           HaskellWorks.Data.Network.Ip.Range
import           HaskellWorks.Data.Network.Ip.Validity

import           Common.SmallSet
import           RPKI.Domain
import           RPKI.Resources.Types
import           RPKI.Resources.Resources
import           RPKI.RRDP.Types

import           Time.Types

import           Crypto.Error
import           Data.Hourglass

import qualified Crypto.PubKey.Curve25519              as X25519
import qualified Crypto.PubKey.Curve448                as X448
import qualified Crypto.PubKey.DSA                     as DSA
import qualified Crypto.PubKey.Ed25519                 as Ed25519
import qualified Crypto.PubKey.Ed448                   as Ed448
import qualified Crypto.PubKey.RSA                     as RSA
import           Crypto.PubKey.ECC.Types

import           RPKI.Util                             (convert)


instance Arbitrary URI where
  arbitrary = URI <$> do
    ext  <- elements [ ".cer", ".mft", ".roa", ".crl" ]
    name <- listOf1 $ elements ['a'..'z']
    pure $ convert $ "rsync://" <> name <> ext
  shrink = genericShrink

instance Arbitrary Hash where
  arbitrary = Hash . B.pack <$> replicateM 32 arbitrary

instance Arbitrary Serial where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary SessionId where
  arbitrary = SessionId . convert <$>
    (listOf1 $ elements $ ['a'..'z'] ++ ['0'..'9'])

instance Arbitrary Version where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary DeltaPublish where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary DeltaWithdraw where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary DeltaItem where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary DecodedBase64 where
  arbitrary = DecodedBase64 <$> arbitrary

instance Arbitrary EncodedBase64 where
    arbitrary = do 
        DecodedBase64 bs <- arbitrary
        pure $ EncodedBase64 $ B64.encode bs
    shrink = genericShrink   
  
instance Arbitrary SnapshotInfo where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary SnapshotPublish where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary DeltaInfo where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Snapshot where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Delta where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Notification where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- 
  

instance Arbitrary a => Arbitrary (X509.SignedExact a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary a => Arbitrary (X509.Signed a) where
  arbitrary = genericArbitrary
  shrink = genericShrink
    
instance Arbitrary SignatureALG where    
    arbitrary = pure $ SignatureALG HashSHA256 PubKeyALG_RSA

instance Arbitrary a => Arbitrary (NE.NonEmpty a) where
  arbitrary = genericArbitrary
  shrink = genericShrink
  
instance Arbitrary AKI where
  arbitrary = AKI . KI <$> B.pack <$> replicateM 20 arbitrary

instance Arbitrary SKI where
  arbitrary = SKI . KI <$> B.pack <$> replicateM 20 arbitrary

instance Arbitrary KI where
  arbitrary = KI <$> arbitrary  

instance Arbitrary Manifest where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Roa where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Gbr where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary ASN where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary a => Arbitrary (CMS a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary SignCRL where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary ResourceCertificate where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary RpkiObject where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary IdentityMeta where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance (Arbitrary m, Arbitrary a) => Arbitrary (With m a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance (Arbitrary s, Arbitrary r) => Arbitrary (WithRFC_ s r) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary (WithRFC 'Strict_ ResourceCert) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary (ResourceCert 'Strict_) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary (ResourceCert 'Reconsidered_) where
  arbitrary = genericArbitrary
  shrink = genericShrink
instance Arbitrary (WithRFC 'Reconsidered_ ResourceCert) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary AsResources where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary IpResources where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary AllResources where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary AsResource where
  arbitrary = oneof [as, asRange]
    where
      as = AS . ASN <$> arbitrary
      asRange = do
        s <- arbitrary
        e <- suchThat arbitrary (\w -> (fromIntegral w) > s)
        pure $ ASRange (ASN s) (ASN e)
  shrink = genericShrink

instance Arbitrary a => Arbitrary (RSet a) where
  arbitrary = genericArbitrary
  shrink = genericShrink
  
instance Arbitrary IpResourceSet where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary a => Arbitrary (SmallSet a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary a => Arbitrary (IntervalSet a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary ContentType where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary a => Arbitrary (EncapsulatedContentInfo a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary a => Arbitrary (SignedObject a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary a => Arbitrary (SignedData a) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary CMSVersion where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary DigestAlgorithmIdentifiers where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary SignatureAlgorithmIdentifier where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary SignatureValue where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary SignerIdentifier where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary SignedAttributes where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Attribute where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary CertificateWithSignature where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary SignerInfos where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- IPs

instance Arbitrary IpPrefix where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Ipv4Prefix where
  arbitrary = do
    w1 :: Word8 <- arbitrary `suchThat` (>0)
    w2 :: Word8 <- arbitrary `suchThat` (>0)
    w3 :: Word8 <- arbitrary `suchThat` (>0)
    w4 :: Word8 <- arbitrary `suchThat` (>0)
    let w = fourW8sToW32 [w1, w2, w3, w4]
    m :: Word8  <- choose (8, 32)
    let x = w `shift` (32 - fromIntegral m)
    pure $ mkIpv4Block x m
  shrink = genericShrink

instance Arbitrary Ipv6Prefix where
  arbitrary = do
    w1 :: Word32 <- arbitrary `suchThat` (>0)
    w2 :: Word32 <- arbitrary `suchThat` (>0)
    w3 :: Word32 <- arbitrary `suchThat` (>0)
    w4 :: Word32 <- arbitrary `suchThat` (>0)    
    m :: Word8  <- choose (46, 128)
    let x = (w1, w2, w3, w4) `shift` (128 - fromIntegral m)
    pure $ mkIpv6Block x m
  shrink = genericShrink

instance Arbitrary (V4.IpBlock Canonical) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary (V6.IpBlock Canonical) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary (Range V4.IpAddress) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary (Range V6.IpAddress) where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary V4.IpAddress where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary V6.IpAddress where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary V4.IpNetMask where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary V6.IpNetMask where
  arbitrary = genericArbitrary
  shrink = genericShrink

--- Crypto stuff

instance Arbitrary BitArray where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary ASN1TimeType where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary ASN1Class where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary ASN1ConstructionType where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary SerializedPoint where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Crypto.PubKey.ECC.Types.CurveName where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Date where
  arbitrary = genericArbitrary
  shrink = genericShrink
  
instance Arbitrary TimeOfDay where
  arbitrary = genericArbitrary
  shrink = genericShrink
  
instance Arbitrary Month where
  arbitrary = genericArbitrary
  shrink = genericShrink
  
instance Arbitrary Hours where
  arbitrary = genericArbitrary
  shrink = genericShrink
  
instance Arbitrary Minutes where
  arbitrary = genericArbitrary
  shrink = genericShrink
  
instance Arbitrary Seconds where
  arbitrary = genericArbitrary
  shrink = genericShrink
  
instance Arbitrary NanoSeconds where
  arbitrary = genericArbitrary
  shrink = genericShrink
  
instance Arbitrary TimezoneOffset where
  arbitrary = genericArbitrary
  shrink = genericShrink
  
instance Arbitrary ASN1 where
  arbitrary = genericArbitrary
  shrink = genericShrink
    
instance Arbitrary PubKey where
  arbitrary = PubKeyRSA <$> arbitrary

instance Arbitrary PubKeyEC where
  arbitrary = genericArbitrary
  shrink = genericShrink
  
instance Arbitrary PubKeyALG where
  arbitrary = genericArbitrary
  shrink = genericShrink
  
instance Arbitrary ExtensionRaw where
  arbitrary = genericArbitrary
  shrink = genericShrink
  
instance Arbitrary HashALG where
  arbitrary = genericArbitrary
  shrink = genericShrink
  
  
instance Arbitrary RSA.PublicKey where
    arbitrary = do
        bytes <- elements [64,128,256]
        e     <- elements [0x3,0x10001]
        n     <- choose (2^(8*(bytes-1)),2^(8*bytes))
        return $ RSA.PublicKey { RSA.public_size = bytes
                               , RSA.public_n    = n
                               , RSA.public_e    = e
                               }

instance Arbitrary DSA.Params where
    arbitrary = DSA.Params <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary DSA.PublicKey where
    arbitrary = DSA.PublicKey <$> arbitrary <*> arbitrary

instance Arbitrary X25519.PublicKey where
    arbitrary = X25519.toPublic <$> arbitrary

instance Arbitrary X448.PublicKey where
    arbitrary = X448.toPublic <$> arbitrary

instance Arbitrary Ed25519.PublicKey where
    arbitrary = Ed25519.toPublic <$> arbitrary

instance Arbitrary Ed448.PublicKey where
    arbitrary = Ed448.toPublic <$> arbitrary

instance Arbitrary DSA.PrivateKey where
    arbitrary = DSA.PrivateKey <$> arbitrary <*> arbitrary

instance Arbitrary X25519.SecretKey where
    arbitrary = throwCryptoError . X25519.secretKey <$> arbitraryBS 32 32

instance Arbitrary X448.SecretKey where
    arbitrary = throwCryptoError . X448.secretKey <$> arbitraryBS 56 56

instance Arbitrary Ed25519.SecretKey where
    arbitrary = throwCryptoError . Ed25519.secretKey <$> arbitraryBS 32 32

instance Arbitrary Ed448.SecretKey where
    arbitrary = throwCryptoError . Ed448.secretKey <$> arbitraryBS 57 57  


instance Arbitrary ASN1StringEncoding where
    arbitrary = elements [IA5,UTF8]

instance Arbitrary ASN1CharacterString where
    arbitrary = ASN1CharacterString <$> arbitrary <*> arbitraryBS 2 36

instance Arbitrary DistinguishedName where
    arbitrary = DistinguishedName <$> (choose (1,5) >>= \l -> replicateM l arbitraryDE)
      where arbitraryDE = (,) <$> arbitrary <*> arbitrary

instance Arbitrary DateTime where
    arbitrary = timeConvert <$> (arbitrary :: Gen Elapsed)
instance Arbitrary Elapsed where
    arbitrary = Elapsed . Seconds <$> (choose (1, 100000000))

instance Arbitrary Extensions where
    arbitrary = Extensions <$> oneof
        [ pure Nothing
        , Just <$> (listOf1 $ oneof
            [ extensionEncode <$> arbitrary <*> (arbitrary :: Gen ExtKeyUsage)
            ]
            )
        ]

instance Arbitrary ExtKeyUsageFlag where
    arbitrary = elements $ enumFrom KeyUsage_digitalSignature
instance Arbitrary ExtKeyUsage where
    arbitrary = ExtKeyUsage . L.sort . L.nub <$> listOf1 arbitrary

instance Arbitrary ExtKeyUsagePurpose where
    arbitrary = elements [ KeyUsagePurpose_ServerAuth
                          , KeyUsagePurpose_ClientAuth
                          , KeyUsagePurpose_CodeSigning
                          , KeyUsagePurpose_EmailProtection
                          , KeyUsagePurpose_TimeStamping
                          , KeyUsagePurpose_OCSPSigning ]
instance Arbitrary ExtExtendedKeyUsage where
    arbitrary = ExtExtendedKeyUsage . L.nub <$> listOf1 arbitrary

instance Arbitrary Certificate where
    arbitrary = Certificate <$> pure 2
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary

instance Arbitrary RevokedCertificate where
    arbitrary = RevokedCertificate <$> arbitrary
                                    <*> arbitrary
                                    <*> pure (Extensions Nothing)

instance Arbitrary CRL where
    arbitrary = CRL <$> pure 1
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary

arbitraryBS :: Int -> Int -> Gen B.ByteString
arbitraryBS r1 r2 = choose (r1,r2) >>= \l -> (B.pack <$> replicateM l arbitrary)