{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RPKI.Parse.Internal.Common where

import Data.Bifunctor
import Control.Monad

import qualified Data.ByteString as BS  
import qualified Data.Text as Text  
import Data.Text.Encoding (decodeUtf8)

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

id_pkcs9, id_contentType, id_messageDigest, id_signingTime, id_binarySigningTime, id_sha256 :: OID
id_pkcs9              = [1, 2, 840, 113549, 1, 9]
id_contentType        = id_pkcs9 <> [3]
id_messageDigest      = id_pkcs9 <> [4]
id_signingTime        = id_pkcs9 <> [5]
id_binarySigningTime  = id_pkcs9 <> [16, 2, 46]

id_sha256            = [2, 16, 840, 1, 101, 3, 4, 2, 1]

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
getAddressFamily m = getNext >>= \case 
    (OctetString familyType) -> 
        case BS.take 2 familyType of 
            "\NUL\SOH" -> pure $ Right Ipv4F
            "\NUL\STX" -> pure $ Right Ipv6F
            af         -> pure $ Left af 
    a              -> parseError m a      

-- Certificate utilities
extVal :: [ExtensionRaw] -> OID -> Maybe BS.ByteString
extVal exts oid = listToMaybe [c | ExtensionRaw oid' _ c <- exts, oid' == oid ]

getExts :: Certificate -> [ExtensionRaw]
getExts (certExtensions -> Extensions extensions) = fromMaybe [] extensions   

getExtsSign :: CertificateWithSignature -> [ExtensionRaw]
getExtsSign = getExts . cwsX509certificate

parseKI :: BS.ByteString -> ParseResult KI
parseKI bs = case decodeASN1' BER bs of
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

keyCertSignBit :: BitArray -> Bool
keyCertSignBit ba = bitArrayGetBit ba 5

cRLSignBit :: BitArray -> Bool
cRLSignBit ba = bitArrayGetBit ba 6