{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=18 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module RPKI.Orphans where

import           Control.Monad

import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Base64               as B64
import qualified Data.List                            as List
import qualified Data.Text                            as Text

import           Data.List.NonEmpty                   (NonEmpty(..))
import qualified Data.Set.NonEmpty                    as NESet

import           Test.QuickCheck hiding ((.&.))
import           Test.QuickCheck.Arbitrary.Generic
import           Test.QuickCheck.Instances.ByteString
import           Test.QuickCheck.Instances.Text
import           Test.QuickCheck.Instances.Vector

import           Data.ASN1.BitArray
import           Data.ASN1.Types
import           Data.Bits
import           Data.Word
import           Data.X509                            as X509
import           Data.Tuple.Strict

import           RPKI.Orphans.Generics
import           RPKI.Domain
import           RPKI.Time
import           RPKI.Repository
import           RPKI.Resources.Resources
import           RPKI.Resources.Types
import           RPKI.RRDP.Types
import           RPKI.Reporting

import           Time.Types

import           Crypto.Error
import           Data.Hourglass

import qualified Crypto.PubKey.Curve25519             as X25519
import qualified Crypto.PubKey.Curve448               as X448
import qualified Crypto.PubKey.DSA                    as DSA
import           Crypto.PubKey.ECC.Types
import qualified Crypto.PubKey.Ed25519                as Ed25519
import qualified Crypto.PubKey.Ed448                  as Ed448
import qualified Crypto.PubKey.RSA                    as RSA
import           Data.Map        (Map)
import qualified Data.Map.Strict as Map

import System.Posix.Types

import           Data.Map.Monoidal.Strict
import           RPKI.Logging
import           RPKI.RTR.Types
import           RPKI.RTR.Protocol
import           RPKI.Util       (convert, mkHash)


instance Arbitrary URI where
    arbitrary = urlByProtocol =<< elements ["rsync", "https"]          
        
urlByProtocol :: [Char] -> Gen URI
urlByProtocol protocol = URI <$> do
        ext  <- elements [ ".cer", ".mft", ".roa", ".crl" ]
        name <- listOf1 $ elements ['a'..'z']        
        pure $ convert $ protocol <> "://" <> name <> ext    

instance Arbitrary RrdpURL where
    arbitrary = RrdpURL <$> urlByProtocol "https"
    shrink = genericShrink

instance Arbitrary RsyncURL where
    arbitrary = do
        ext  <- elements [ ".cer", ".mft", ".roa", ".crl" ]
        hostName <- arbitrary 
        name <- listOf1 $ elements ['a'..'z']        
        name1 <- listOf1 $ elements ['a'..'z']        
        name2 <- listOf1 $ elements ['a'..'z']                
        pure $ RsyncURL hostName                
                (Prelude.map (RsyncPathChunk . convert) [name1, name2, name <> ext])
    shrink = genericShrink

instance Arbitrary RsyncHost where
    arbitrary = RsyncHost <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary RsyncHostName where
    arbitrary = fmap (RsyncHostName . convert) $ listOf1 $ elements $ ['a'..'z'] <> ['.']
    shrink = genericShrink

instance Arbitrary RsyncPort where
    arbitrary = do 
        i <- arbitrary
        pure $ RsyncPort $ 1024 + abs i `mod` 64000
    shrink = genericShrink

instance Arbitrary RsyncPathChunk where
    arbitrary = fmap (RsyncPathChunk . convert) $ listOf1 $ elements ['a'..'z']
    shrink = genericShrink

instance Arbitrary RpkiURL where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Hash where
    arbitrary = mkHash . BS.pack <$> replicateM 32 arbitrary

instance Arbitrary Serial where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SessionId where
    arbitrary = SessionId . convert <$> 
        listOf1 (elements $ ['a'..'z'] ++ ['0'..'9'])

instance Arbitrary RrdpSerial where
    arbitrary = genericArbitrary
    shrink = genericShrink

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
        pure $ EncodedBase64 $ B64.encodeBase64' bs

instance Arbitrary SPKI where
    arbitrary = genericArbitrary
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

instance Arbitrary LogLevel where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary CPid where
    arbitrary = CPid <$> arbitrary
    shrink (CPid i) = Prelude.map CPid $ shrink i

instance Arbitrary LogMessage where
    arbitrary = genericArbitrary
    shrink = genericShrink

#if MIN_VERSION_generic_arbitrary(1,0,0)
instance (Arg (X509.SignedExact a) (X509.Signed a), 
          Arg (X509.Signed a) a, 
          Arbitrary a) => Arbitrary (X509.SignedExact a) where
#else
instance Arbitrary a => Arbitrary (X509.SignedExact a) where    
#endif
    arbitrary = genericArbitrary
    shrink = genericShrink

#if MIN_VERSION_generic_arbitrary(1,0,0)
instance (Arg (X509.Signed a) a, Arbitrary a) => Arbitrary (X509.Signed a) where
#else
instance Arbitrary a => Arbitrary (X509.Signed a) where    
#endif    
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SignatureALG where    
    arbitrary = pure $ SignatureALG HashSHA256 PubKeyALG_RSA

#if MIN_VERSION_generic_arbitrary(1,0,0)
instance (Arg (NESet.NESet a) a, Arbitrary a, Ord a) => Arbitrary (NESet.NESet a) where
#else
instance (Arbitrary a, Ord a) => Arbitrary (NESet.NESet a) where
#endif    
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = do 
        x :: a    <- arbitrary 
        xs :: [a] <- arbitrary
        pure $ x :| xs
    shrink = genericShrink

instance Arbitrary AKI where
    arbitrary = AKI <$> arbitrary

instance Arbitrary SKI where
    arbitrary = SKI <$> arbitrary

instance Arbitrary KI where
    arbitrary = mkKI . BS.pack <$> replicateM 20 arbitrary  

instance Arbitrary Instant where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Manifest where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Vrp where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary AscOrderedVrp where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary BGPSecPayload where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PrefixLength where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Gbr where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Rsc where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Aspa where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ASN where
    arbitrary = genericArbitrary
    shrink = genericShrink

#if MIN_VERSION_generic_arbitrary(1,0,0)
instance (Arg (CMS a) (SignedObject a), 
          Arg (SignedObject a) a,           
          Arg (EncapsulatedContentInfo a) a, 
          Arbitrary a) => Arbitrary (CMS a) where
#else
instance (Arbitrary a) => Arbitrary (CMS a) where    
#endif            
    arbitrary = genericArbitrary
    shrink = genericShrink

#if MIN_VERSION_generic_arbitrary(1,0,0)
instance (Arg (CMSBasedObject a) a, 
          Arg (SignedObject a) a, 
          Arg (EncapsulatedContentInfo a) a, 
          Arbitrary a) => Arbitrary (CMSBasedObject a) where
#else
instance (Arbitrary a) => Arbitrary (CMSBasedObject a) where    
#endif            
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

#if MIN_VERSION_generic_arbitrary(1,0,0)
instance (Arg (Located a) a, Arbitrary a) => Arbitrary (Located a) where
#else
instance Arbitrary a => Arbitrary (Located a) where
#endif      
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Locations where
    arbitrary = Locations . NESet.fromList <$> arbitrary        
    shrink = genericShrink

instance Arbitrary CaCerObject where
    arbitrary = genericArbitrary    
    shrink = genericShrink

instance Arbitrary EECerObject where
    arbitrary = genericArbitrary    
    shrink = genericShrink

instance Arbitrary BgpCerObject where
    arbitrary = genericArbitrary    
    shrink = genericShrink

instance Arbitrary CrlObject where
    arbitrary = genericArbitrary    
    shrink = genericShrink

instance Arbitrary RawResourceCertificate where
    arbitrary = genericArbitrary
    shrink = genericShrink

#if MIN_VERSION_generic_arbitrary(1,0,0)
instance (Arg (SomeRFC r) r, 
          Arg (PolyRFC r 'StrictRFC) r,
          Arg (PolyRFC r 'ReconsideredRFC) r,
          Arbitrary r) => Arbitrary (SomeRFC r) where
#else
instance Arbitrary r => Arbitrary (SomeRFC r) where
#endif      
    arbitrary = genericArbitrary
    shrink = genericShrink

#if MIN_VERSION_generic_arbitrary(1,0,0)
instance (Arg (PolyRFC r 'StrictRFC) r, Arbitrary r) => 
        Arbitrary (PolyRFC r 'StrictRFC) where
#else
instance Arbitrary r => Arbitrary (PolyRFC r 'StrictRFC) where
#endif      
    arbitrary = genericArbitrary
    shrink = genericShrink

#if MIN_VERSION_generic_arbitrary(1,0,0)
instance (Arg (PolyRFC r 'ReconsideredRFC) r, Arbitrary r) => 
        Arbitrary (PolyRFC r 'ReconsideredRFC) where
#else
instance Arbitrary r => Arbitrary (PolyRFC r 'ReconsideredRFC) where
#endif
    arbitrary = genericArbitrary
    shrink = genericShrink

#if MIN_VERSION_generic_arbitrary(1,0,0)
instance (Arg (TypedCert r 'CACert) r, Arbitrary r) => Arbitrary (TypedCert r 'CACert) where
#else
instance Arbitrary r => Arbitrary (TypedCert r 'CACert) where
#endif
    arbitrary = genericArbitrary
    shrink = genericShrink

#if MIN_VERSION_generic_arbitrary(1,0,0)
instance (Arg (TypedCert r 'EECert) r, Arbitrary r) => Arbitrary (TypedCert r 'EECert) where
#else
instance Arbitrary r => Arbitrary (TypedCert r 'EECert) where
#endif
    arbitrary = genericArbitrary
    shrink = genericShrink

#if MIN_VERSION_generic_arbitrary(1,0,0)
instance (Arg (TypedCert r 'BGPCert) r, Arbitrary r) => Arbitrary (TypedCert r 'BGPCert) where
#else
instance Arbitrary r => Arbitrary (TypedCert r 'BGPCert) where
#endif
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
            e <- suchThat arbitrary (>s)
            pure $ ASRange (ASN s) (ASN e)

#if MIN_VERSION_generic_arbitrary(1,0,0)
instance (Arg (RSet a) a, Arbitrary a) => Arbitrary (RSet a) where
#else
instance Arbitrary a => Arbitrary (RSet a) where
#endif
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary IpResourceSet where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (IntervalSet a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ContentType where
    arbitrary = genericArbitrary
    shrink = genericShrink

#if MIN_VERSION_generic_arbitrary(1,0,0)
instance (Arg (EncapsulatedContentInfo a) a, Arbitrary a) => Arbitrary (EncapsulatedContentInfo a) where
#else
instance Arbitrary a => Arbitrary (EncapsulatedContentInfo a) where
#endif
    arbitrary = genericArbitrary
    shrink = genericShrink

#if MIN_VERSION_generic_arbitrary(1,0,0)
instance (Arg (SignedObject a) (SignedData a),
          Arg (EncapsulatedContentInfo a) a, 
          Arbitrary a) => Arbitrary (SignedObject a) where
#else
instance Arbitrary a => Arbitrary (SignedObject a) where
#endif
    arbitrary = genericArbitrary
    shrink = genericShrink

#if MIN_VERSION_generic_arbitrary(1,0,0)
instance (Arg (SignedData a) (EncapsulatedContentInfo a), 
          Arg (EncapsulatedContentInfo a) a, 
          Arbitrary a) => Arbitrary (SignedData a) where
#else
instance Arbitrary a => Arbitrary (SignedData a) where
#endif
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary CMSVersion where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary DigestAlgorithmIdentifier where
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

instance Arbitrary Repository where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary RsyncPublicationPoint where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary RrdpIntegrity where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary RrdpMeta where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary RrdpRepository where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary RsyncRepository where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ETag where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary FetchType where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary FetchStatus where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary RsyncTree where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary RsyncNodeNormal where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary RepositoryMeta where
    arbitrary = genericArbitrary
    shrink = genericShrink
    
instance Arbitrary Trace where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PublicationPoints where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary RrdpMap where
    arbitrary = do 
        rrdps :: [RrdpRepository] <- arbitrary
        pure $ RrdpMap $ Map.fromList [ (uri, r) | r@RrdpRepository{..} <- rrdps ]

-- errors and warnings

instance Arbitrary ArtificialKey where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ObjectKey where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ObjectIdentity where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary MftPair where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (ParseError Text.Text) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary VWarning where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ValidationError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary StorageError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary RsyncError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary RrdpError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TALError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary InitError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary InternalError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SlurmError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary AppError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary VIssue where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary VScope where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary MetricScope where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Focus where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Validations where
    arbitrary = generateMap Validations     

instance Arbitrary ValidationState where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary RawMetric where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary RsyncMetric where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary RrdpMetric where
    arbitrary = genericArbitrary
    shrink = genericShrink
    
instance Arbitrary VrpCounts where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ValidationMetric where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TaName where
    arbitrary = TaName <$> arbitrary
    shrink = genericShrink

instance Arbitrary RpkiObjectType where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (MetricMap a) where
    arbitrary = generateMap $ MetricMap . MonoidalMap    

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (MonoidalMap k v) where
    arbitrary = genericArbitrary
    shrink = genericShrink

generateMap :: (Arbitrary k, Arbitrary a, Ord k) => (Map k a -> b) -> Gen b
generateMap constructor = do 
    size_ <- choose (0, 10)
    validations <- replicateM size_ arbitrary
    pure $ constructor $ Map.fromList validations

instance Arbitrary TimeMs where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary CPUTime where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary RrdpSource where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary FetchFreshness where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Count where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary HttpStatus where
    arbitrary = genericArbitrary
    shrink = genericShrink

#if MIN_VERSION_generic_arbitrary(1,0,0)
instance (Arg (T2 a b) a, Arg (T2 a b) b, 
         Arbitrary a, Arbitrary b) => Arbitrary (T2 a b) where
#else
instance (Arbitrary a, Arbitrary b) => Arbitrary (T2 a b) where
#endif
    arbitrary = genericArbitrary
    shrink = genericShrink

#if MIN_VERSION_generic_arbitrary(1,0,0)
instance (Arg (T3 a b c) a, Arg (T3 a b c) b, Arg (T3 a b c) c, 
         Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (T3 a b c) where
#else
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (T3 a b c) where
#endif
    arbitrary = genericArbitrary
    shrink = genericShrink

-- IPs

instance Arbitrary IpPrefix where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Ipv4Prefix where
    arbitrary = do
        w1 :: Word8 <- arbitrary `suchThat` (> 0)
        w2 :: Word8 <- arbitrary
        w3 :: Word8 <- arbitrary
        w4 :: Word8 <- arbitrary
        m :: Word8  <- choose (8, 32)        
        let mask = (0xFFFFFFFF :: Word32) `shift` (32 - fromIntegral m)
        let x = fourW8sToW32 [w1, w2, w3, w4] .&. mask 
        pure $! mkIpv4Block x m

instance Arbitrary Ipv6Prefix where
    arbitrary = do
        w1 :: Word32 <- arbitrary `suchThat` (> 0)
        w2 :: Word32 <- arbitrary
        w3 :: Word32 <- arbitrary
        w4 :: Word32 <- arbitrary
        m :: Word8  <- choose (46, 128)
        let x = (w1, w2, w3, w4) `shift` (128 - fromIntegral m)
        pure $! mkIpv6Block x m

instance Arbitrary PrefixesAndAsns where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary AddrFamily where
    arbitrary = genericArbitrary
    shrink = genericShrink


-- RTR

instance (Arbitrary a, Ord a) => Arbitrary (Diff a) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary RtrSessionId where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SerialNumber where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ErrorCode where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ProtocolVersion where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Flags where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Intervals where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Pdu where
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
    arbitrary = Elapsed . Seconds <$> choose (1, 100000000)

instance Arbitrary Extensions where
    arbitrary = Extensions <$> oneof
        [ pure Nothing
        , Just <$> listOf1 (oneof
            [ extensionEncode <$> arbitrary <*> (arbitrary :: Gen ExtKeyUsage)
            ])
        ]

instance Arbitrary ExtKeyUsageFlag where
    arbitrary = elements $ enumFrom KeyUsage_digitalSignature
instance Arbitrary ExtKeyUsage where
    arbitrary = ExtKeyUsage . List.sort . List.nub <$> listOf1 arbitrary

instance Arbitrary ExtKeyUsagePurpose where
    arbitrary = elements [ KeyUsagePurpose_ServerAuth
                          , KeyUsagePurpose_ClientAuth
                          , KeyUsagePurpose_CodeSigning
                          , KeyUsagePurpose_EmailProtection
                          , KeyUsagePurpose_TimeStamping
                          , KeyUsagePurpose_OCSPSigning ]
instance Arbitrary ExtExtendedKeyUsage where
    arbitrary = ExtExtendedKeyUsage . List.nub <$> listOf1 arbitrary

instance Arbitrary Certificate where
    arbitrary = Certificate 2 <$>arbitrary
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

instance Arbitrary X509.CRL where
    arbitrary = X509.CRL 1 <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary

arbitraryBS :: Int -> Int -> Gen BS.ByteString
arbitraryBS r1 r2 = choose (r1,r2) >>= \l -> BS.pack <$> replicateM l arbitrary