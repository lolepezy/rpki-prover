{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RPKI.Parse.Internal.Common where

import Data.Bifunctor
import Control.Applicative
import Control.Monad

import Data.Bits

import qualified Data.ByteString as BS  
import qualified Data.Text as Text  
import Data.Text.Encoding (decodeUtf8)

import qualified Data.List as List

import Data.Word
import Data.Char (chr)
import Data.Maybe

import Data.ASN1.OID
import Data.ASN1.Types
import Data.ASN1.Parse
import Data.ASN1.BitArray
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding

import Data.X509 as X509

import RPKI.Resources.Types
import RPKI.Domain
import RPKI.Reporting (ParseError(..))

import RPKI.Resources.Resources   as R

type ParseResult a = Either (ParseError Text.Text) a

oid_pkix, oid_pe :: OID
id_pe_ipAddrBlocks, id_pe_autonomousSysIds :: OID
id_pe_ipAddrBlocks_v2, id_pe_autonomousSysIds_v2 :: OID
id_pe_sia, id_ad_rpki_notify, id_ad_rpki_repository :: OID
id_ad_rpkiManifest :: OID
id_kp_bgpsecRouter :: OID

oid_pkix                  = [1, 3, 6, 1, 5, 5, 7]
oid_pe                    = oid_pkix <> [ 1 ]
id_pe_sia                 = oid_pe <> [ 11 ]
id_pe_ipAddrBlocks        = oid_pe <> [ 7 ]
id_pe_autonomousSysIds    = oid_pe <> [ 8 ]
id_pe_ipAddrBlocks_v2     = oid_pe <> [ 28 ]
id_pe_autonomousSysIds_v2 = oid_pe <> [ 29 ]

id_ad_rpki_notify         = oid_pkix <> [ 48, 13 ]  
id_ad_rpki_repository     = oid_pkix <> [ 48, 5 ]  
id_ad_rpkiManifest        = oid_pkix <> [ 48, 10]

id_kp_bgpsecRouter        = oid_pkix <> [3, 30]

id_cp_ipAddr_asNumber, id_cps_qualifier :: OID
id_cp_ipAddr_asNumber = oid_pkix <> [ 14, 2 ]
id_cps_qualifier      = oid_pkix <> [ 2, 1 ]

id_subjectKeyId, id_authorityKeyId, id_crlNumber :: OID
id_subjectKeyId   = [2, 5, 29, 14]
id_authorityKeyId = [2, 5, 29, 35]
id_crlNumber      = [2, 5, 29, 20]

id_pkcs9, id_contentType, id_messageDigest, id_signingTime, id_binarySigningTime :: OID
id_sha256, id_sha512, id_ct_signedChecklist :: OID

id_pkcs9              = [1, 2, 840, 113549, 1, 9]
id_contentType        = id_pkcs9 <> [3]
id_messageDigest      = id_pkcs9 <> [4]
id_signingTime        = id_pkcs9 <> [5]
id_binarySigningTime  = id_pkcs9 <> [16, 2, 46]
id_ct_signedChecklist = id_pkcs9 <> [16, 1, 48]
id_ct_aspa            = id_pkcs9 <> [16, 1, 49]
                       
                        
id_sha256            = [2, 16, 840, 1, 101, 3, 4, 2, 1]
id_sha512            = [2, 16, 840, 1, 101, 3, 4, 2, 3]

id_ce_CRLDistributionPoints, id_ce_certificatePolicies, id_ce_basicConstraints ::OID 
id_ce_keyUsage, id_ce_extKeyUsage :: OID 
id_ce_keyUsage              = [2, 5, 29, 15]
id_ce_extKeyUsage           = [2, 5, 29, 37]
id_ce_basicConstraints      = [2, 5, 29, 19]
id_ce_CRLDistributionPoints = [2, 5, 29, 31]
id_ce_certificatePolicies   = [2, 5, 29, 32]


allowedCriticalOIDs :: [OID]
allowedCriticalOIDs = [ 
        id_ce_basicConstraints, 
        id_ce_certificatePolicies, 
        id_ce_keyUsage, 
        id_pe_ipAddrBlocks, 
        id_pe_autonomousSysIds 
    ]

fmtErr :: String -> ParseError Text.Text
fmtErr = ParseError . Text.pack

mapParseErr :: Either String a -> ParseResult a       
mapParseErr = first fmtErr

parseError :: String -> ASN1 -> ParseASN1 a
parseError m a = throwParseError $ case m of 
    [] -> show a
    m' -> m' <> "(" <> show a <> ")"

getNull_ :: ParseASN1 a -> ParseASN1 a
getNull_ f = getNull f ""

getNull :: ParseASN1 a -> String -> ParseASN1 a
getNull f m = getNext >>= \case 
    Null -> f
    a    -> parseError m a

getInteger :: (Integer -> ParseASN1 a) -> String -> ParseASN1 a
getInteger f m = getNext >>= \case 
    IntVal i -> f i
    b        -> throwParseError $ m <> "(" <> show b <> ")"

getOID :: (OID -> ParseASN1 a) -> String -> ParseASN1 a
getOID f m = getNext >>= \case 
    OID oid -> f oid
    a       -> parseError m a

getIA5String :: (String -> ParseASN1 a) -> String -> ParseASN1 a
getIA5String f m = getNext >>= \case 
    ASN1String (ASN1CharacterString IA5 bs) -> f $ map (chr . fromEnum) $ BS.unpack bs
    a                                       -> parseError m a

getBitString :: (BS.ByteString -> ParseASN1 a) -> String -> ParseASN1 a
getBitString f m = getNext >>= \case 
    BitString (BitArray _ bs) -> f bs
    a                         -> parseError m a

getAddressFamily :: String -> ParseASN1 (Either BS.ByteString AddrFamily)
getAddressFamily message = getNext >>= \case 
    (OctetString familyType) -> 
        pure $ extractAddressaFamily familyType
    a -> parseError message a      

getAddressFamilyMaybe :: ParseASN1 (Maybe AddrFamily)
getAddressFamilyMaybe = getNext >>= \case 
    (OctetString familyType) -> 
        pure $ either (const Nothing) Just $ extractAddressaFamily familyType                
    _ -> pure Nothing

extractAddressaFamily :: BS.ByteString -> Either BS.ByteString AddrFamily
extractAddressaFamily familyBS = 
    case BS.take 2 familyBS of 
        "\NUL\SOH" -> Right Ipv4F
        "\NUL\STX" -> Right Ipv6F
        af         -> Left af


getDigest :: ParseASN1 (Maybe OID)
getDigest = 
    getNext >>= \case
        OID oid -> pure $ Just oid
        Null    -> pure Nothing
        s       -> throwParseError $ "DigestAlgorithms is wrong " <> show s

-- Certificate utilities
extVal :: [ExtensionRaw] -> OID -> Maybe BS.ByteString
extVal exts oid = listToMaybe [c | ExtensionRaw oid' _ c <- exts, oid' == oid ]

getExts :: Certificate -> [ExtensionRaw]
getExts (certExtensions -> Extensions extensions) = fromMaybe [] extensions   

getExtsSign :: CertificateWithSignature -> [ExtensionRaw]
getExtsSign = getExts . cwsX509certificate

parseKI :: BS.ByteString -> ParseResult KI
parseKI bs = 
    case decodeASN1' BER bs of
        Left e -> Left $ fmtErr $ "Error decoding key identifier: " <> show e
        Right [OctetString bytes] -> makeKI bytes
        Right [Start Sequence, Other Context 0 bytes, End Sequence] -> makeKI bytes    
        Right s -> Left $ fmtErr $ "Unknown key identifier " <> show s
  where
    makeKI bytes = 
        let len = BS.length bytes
        in if len == 20
            then Right $ mkKI bytes
            else Left $ fmtErr $ "KI has wrong length, must be 160 bits, but it is " <> show len

oid2Hash :: OID -> ParseASN1 HashALG
oid2Hash = \case
    oid | oid == id_sha256 -> pure HashSHA256
        | otherwise        -> throwParseError $ "Unknown hashing algorithm OID: " <> show oid

parseSignature :: ParseASN1 SignatureValue
parseSignature = getNext >>= \case 
    OctetString sig            -> pure $ SignatureValue $ toShortBS sig
    BitString (BitArray _ sig) -> pure $ SignatureValue $ toShortBS sig
    s                          -> throwParseError $ "Unknown signature value : " <> show s

unifyCert :: SignedExact X509.Certificate -> CertificateWithSignature
unifyCert signedExact = CertificateWithSignature {
        cwsX509certificate = signedObject signed,
        cwsSignatureAlgorithm = SignatureAlgorithmIdentifier $ signedAlg signed,
        cwsSignature = SignatureValue $ toShortBS $ signedSignature signed,
        cwsEncoded = toShortBS $ getSignedData signedExact      
    }
    where 
        signed = getSigned signedExact


getSiaValue :: Certificate -> OID -> Maybe BS.ByteString
getSiaValue c oid = do
    sia  <- extVal (getExts c) id_pe_sia
    asns <- toMaybe $ decodeASN1' BER sia
    join $ toMaybe $ flip runParseASN1 asns $ 
            listToMaybe . catMaybes <$> 
                onNextContainer Sequence (getMany extractByOid)
    where
        extractByOid = getNextContainerMaybe Sequence >>= \case
            Nothing -> pure Nothing
            Just [OID oid', Other Context 6 value] 
                | oid' == oid -> pure $ Just value
                | otherwise   -> pure Nothing
            _ -> pure Nothing        

getRrdpNotifyUri :: Certificate -> Maybe URI
getRrdpNotifyUri c = URI . decodeUtf8 <$> getSiaValue c id_ad_rpki_notify

getRepositoryUri :: Certificate -> Maybe URI
getRepositoryUri c = URI . decodeUtf8 <$> getSiaValue c id_ad_rpki_repository

getManifestUri :: Certificate -> Maybe URI
getManifestUri c = URI . decodeUtf8 <$> getSiaValue c id_ad_rpkiManifest

getCrlDistributionPoint :: Certificate -> Maybe URI
getCrlDistributionPoint c = do
    crlDP <- extVal (getExts c) id_ce_CRLDistributionPoints
    asns  <- toMaybe $ decodeASN1' BER crlDP
    join $ toMaybe $ flip runParseASN1 asns $ 
        onNextContainer Sequence $ 
            onNextContainer Sequence $ 
                onNextContainer (Container Context 0) $ 
                    getNextContainer (Container Context 0) >>= \case 
                            [Other Context 6 value] -> pure $ Just $ URI $ decodeUtf8 value
                            _                       -> pure Nothing

toMaybe :: Either b a -> Maybe a
toMaybe = either (const Nothing) Just


{-
  Parse IP address extension.

  https://tools.ietf.org/html/rfc3779#section-2.2.3

   IPAddrBlocks        ::= SEQUENCE OF IPAddressFamily

   IPAddressFamily     ::= SEQUENCE {    -- AFI & optional SAFI --
      addressFamily        OCTET STRING (SIZE (2..3)),
      ipAddressChoice      IPAddressChoice }

   IPAddressChoice     ::= CHOICE {
      inherit              NULL, -- inherit from issuer --
      addressesOrRanges    SEQUENCE OF IPAddressOrRange }

   IPAddressOrRange    ::= CHOICE {
      addressPrefix        IPAddress,
      addressRange         IPAddressRange }

   IPAddressRange      ::= SEQUENCE {
      min                  IPAddress,
      max                  IPAddress }

   IPAddress           ::= BIT STRING
-}
parseIpExt' :: ParseASN1 IpResources
parseIpExt' = do
    afs <- getMany addrFamily    
    pure $ IpResources $ IpResourceSet
      (rs [ af | Left  af <- afs ]) 
      (rs [ af | Right af <- afs ])
  where
    rs []       = R.emptyRS
    rs (af : _) = af
    addrFamily = onNextContainer Sequence $
        getAddressFamily "Expected an address family here" >>= \case
            Right Ipv4F -> Left  <$> ipResourceSet ipv4Address
            Right Ipv6F -> Right <$> ipResourceSet ipv6Address
            Left af     -> throwParseError $ "Unsupported address family " <> show af
      where
        ipResourceSet address =
            getNull_ (pure Inherit) <|>
            onNextContainer Sequence (R.toRS . mconcat <$> getMany address)


ipv4Address :: ParseASN1 [Ipv4Prefix]
ipv4Address = ipvVxAddress R.fourW8sToW32 32  makeOneIP R.ipv4RangeToPrefixes

ipv6Address :: ParseASN1 [Ipv6Prefix]
ipv6Address = ipvVxAddress R.someW8ToW128 128 makeOneIP R.ipv6RangeToPrefixes

makeOneIP :: (Prefix a, Integral b) => BS.ByteString -> b -> [a]
makeOneIP bs nz = [make bs (fromIntegral nz)]

ipvVxAddress :: ([Word8] -> t)
            -> Int
            -> (BS.ByteString -> Word64 -> b)
            -> (t -> t -> b)
            -> ParseASN1 b
ipvVxAddress wToAddr fullLength makePrefix rangeToPrefixes =     
    getNextContainerMaybe Sequence >>= \case
        Nothing -> getNext >>= \case
            (BitString (BitArray nzBits bs)) -> 
                pure $ makePrefix bs nzBits
            s -> 
                throwParseError ("Unexpected prefix representation: " <> show s)

        Just [BitString (BitArray nzBits bs)] ->                
                pure $ makePrefix bs nzBits

        Just [
            BitString (BitArray _       bs1),
            BitString (BitArray nzBits2 bs2)
            ] ->
                let w1 = wToAddr $ BS.unpack bs1
                    w2 = wToAddr $ setLowerBitsToOne (BS.unpack bs2)
                        (fromIntegral nzBits2) fullLength
                in pure $ rangeToPrefixes w1 w2

        s -> throwParseError $ "Unexpected address representation: " <> show s

--
-- Set all the bits to `1` starting from `setBitsNum`
-- `allBitsNum` is the total number of bits.
--
setLowerBitsToOne :: (Bits a, Num a) => [a] -> Int -> Int -> [a]
setLowerBitsToOne ws setBitsNum allBitsNum =
    R.rightPad (allBitsNum `div` 8) 0xFF $
        List.zipWith setBits ws (map (*8) [0..])
    where
        setBits w8 i | i < setBitsNum && setBitsNum < i + 8 = w8 .|. extra (i + 8 - setBitsNum)
                        | i < setBitsNum = w8
                        | otherwise = 0xFF
        extra lastBitsNum =
            List.foldl' (\w i -> w .|. (1 `shiftL` i)) 0 [0..lastBitsNum - 1]


parseIpExt :: [ASN1] -> ParseResult IpResources
parseIpExt asns = mapParseErr $ runParseASN1 
        (onNextContainer Sequence parseIpExt') asns


{-
  https://tools.ietf.org/html/rfc3779#section-3.2.3

  id-pe-autonomousSysIds  OBJECT IDENTIFIER ::= { id-pe 8 }

   ASIdentifiers       ::= SEQUENCE {
       asnum               [0] EXPLICIT ASIdentifierChoice OPTIONAL,
       rdi                 [1] EXPLICIT ASIdentifierChoice OPTIONAL}

   ASIdentifierChoice  ::= CHOICE {
      inherit              NULL, -- inherit from issuer --
      asIdsOrRanges        SEQUENCE OF ASIdOrRange }

   ASIdOrRange         ::= CHOICE {
       id                  ASId,
       range               ASRange }
   ASRange             ::= SEQUENCE {
       min                 ASId,
       max                 ASId }

   ASId                ::= INTEGER
-}
parseAsnExt :: [ASN1] -> ParseResult AsResources
parseAsnExt asnBlocks = mapParseErr $ runParseASN1 
        (onNextContainer Sequence parseAsnExt') asnBlocks 
  where
    parseAsnExt' = do     
      -- we only want the first element of the sequence
      AsResources <$> onNextContainer (Container Context 0)
          (getNull_ (pure Inherit) <|>
           R.toRS <$> onNextContainer Sequence (getMany asOrRange))

asOrRange :: ParseASN1 AsResource
asOrRange = 
    getNextContainerMaybe Sequence >>= \case
        Nothing -> getNext >>= \case
            IntVal asn -> pure $ AS $ as' asn
            something  -> throwParseError $ "Unknown ASN specification " <> show something
        Just [IntVal b, IntVal e] -> pure $ ASRange (as' b) (as' e)
        Just something -> throwParseError $ "Unknown ASN specification " <> show something
  where
    as' = ASN . fromInteger
