{-# LANGUAGE OverloadedStrings #-}

module RPKI.Parse.Internal.Common where

import Data.Bifunctor
import Control.Applicative
import Control.Monad

import Data.Bits

import qualified Data.ByteString as BS  
import           Data.Text   (Text)
import qualified Data.Text as Text  
import Data.Text.Encoding (decodeUtf8')

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
import RPKI.AppMonad
import RPKI.Reporting
import RPKI.Domain
import RPKI.Util (fmtGen)

import RPKI.Resources.Resources   as R

oid_pkix, oid_pe :: OID
id_pe_ipAddrBlocks, id_pe_autonomousSysIds :: OID
id_pe_sia, id_pe_aia, id_ad_rpki_notify, id_ad_rpki_repository, id_ad_caIssuers :: OID
id_ad_rpkiManifest, id_ad_signedObject, id_kp_bgpsecRouter :: OID

oid_pkix                  = [1, 3, 6, 1, 5, 5, 7]
oid_pe                    = oid_pkix <> [ 1 ]
id_pe_sia                 = oid_pe <> [ 11 ]
id_pe_aia                 = oid_pe <> [ 1 ]
id_pe_ipAddrBlocks        = oid_pe <> [ 7 ]
id_pe_autonomousSysIds    = oid_pe <> [ 8 ]

id_ad_rpki_notify         = oid_pkix <> [ 48, 13 ]  
id_ad_rpki_repository     = oid_pkix <> [ 48, 5 ]  
id_ad_rpkiManifest        = oid_pkix <> [ 48, 10]
id_ad_caIssuers           = oid_pkix <> [ 48, 2]
id_ad_signedObject        = oid_pkix <> [ 48, 11]

id_kp_bgpsecRouter        = oid_pkix <> [3, 30]

id_cp_ipAddr_asNumber, id_cps_qualifier :: OID
id_cp_ipAddr_asNumber = oid_pkix <> [ 14, 2 ]
id_cps_qualifier      = oid_pkix <> [ 2, 1 ]

id_subjectKeyId, id_authorityKeyId, id_crlNumber :: OID
id_subjectKeyId   = [2, 5, 29, 14]
id_authorityKeyId = [2, 5, 29, 35]
id_crlNumber      = [2, 5, 29, 20]

id_pkcs9, id_contentType, id_messageDigest, id_signingTime, id_binarySigningTime :: OID
id_sha256, id_sha512, id_ct_signedChecklist, id_ct_aspa, id_ct_rpkiSignedPrefixList :: OID
id_signedData, id_ct_rpkiManifest, id_ct_routeOriginAuthz, id_ct_rpkiGhostbusters :: OID

id_pkcs9                   = [1, 2, 840, 113549, 1, 9]
id_contentType             = id_pkcs9 <> [3]
id_messageDigest           = id_pkcs9 <> [4]
id_signingTime             = id_pkcs9 <> [5]
id_binarySigningTime       = id_pkcs9 <> [16, 2, 46]

-- https://www.rfc-editor.org/rfc/rfc5652#section-5.1
id_signedData              = [1, 2, 840, 113549, 1, 7, 2]

-- eContentType OIDs, one per signed object type
-- https://www.rfc-editor.org/rfc/rfc6488#section-2.1.3.1
id_ct_rpkiManifest         = id_pkcs9 <> [16, 1, 26]   -- RFC 9286
id_ct_routeOriginAuthz     = id_pkcs9 <> [16, 1, 24]   -- RFC 9582
id_ct_rpkiGhostbusters     = id_pkcs9 <> [16, 1, 35]   -- RFC 6493
id_ct_signedChecklist      = id_pkcs9 <> [16, 1, 48]   -- RFC 9323
id_ct_aspa                 = id_pkcs9 <> [16, 1, 49]
id_ct_rpkiSignedPrefixList = id_pkcs9 <> [16, 1, 51]
                       
                        
id_sha256            = [2, 16, 840, 1, 101, 3, 4, 2, 1]
id_sha512            = [2, 16, 840, 1, 101, 3, 4, 2, 3]

id_ce_CRLDistributionPoints, id_ce_certificatePolicies, id_ce_basicConstraints :: OID 
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

parseErr :: Text -> AppError
parseErr = ParseE . ParseError

mapParseErr :: Either String a -> PureValidatorT a       
mapParseErr = fromEither . first (ParseE . ParseError . Text.pack)

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
    b        -> throwParseError $ m <> " (" <> show b <> ")"

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


-- https://www.rfc-editor.org/rfc/rfc6487#section-4.8.10
-- SAFI must not be used in the RPKI profile, so the addressFamily field 
-- is exactly two octets, no more.
extractAddressaFamily :: BS.ByteString -> Either BS.ByteString AddrFamily
extractAddressaFamily familyBS = 
    case BS.unpack familyBS of 
        [0, 1] -> Right Ipv4F
        [0, 2] -> Right Ipv6F
        _      -> Left familyBS

getDigest :: ParseASN1 (Maybe OID)
getDigest = 
    getNext >>= \case
        OID oid -> pure $ Just oid
        Null    -> pure Nothing
        s       -> throwParseError $ "DigestAlgorithms is wrong " <> show s

-- Certificate utilities
-- Keep full extension metadata (including critical bit) for profile checks.
-- https://www.rfc-editor.org/rfc/rfc6487#section-4.8
extRawVal :: [ExtensionRaw] -> OID -> Maybe ExtensionRaw
extRawVal exts oid = listToMaybe [e | e@(ExtensionRaw oid' _ _) <- exts, oid' == oid]

extVal :: [ExtensionRaw] -> OID -> Maybe BS.ByteString
extVal exts oid = extRawContent <$> extRawVal exts oid

getExts :: Certificate -> [ExtensionRaw]
getExts (certExtensions -> Extensions extensions) = fromMaybe [] extensions   

getExtsSign :: CertificateWithSignature -> [ExtensionRaw]
getExtsSign = getExts . cwsX509certificate

parseKI :: BS.ByteString -> PureValidatorT KI
parseKI bs = 
    case decodeASN1' DER bs of
        Left e -> pureError $ parseErr $ "Error decoding key identifier: " <> Text.pack (show e)
        Right [OctetString bytes] -> makeKI bytes
        Right [Start Sequence, Other Context 0 bytes, End Sequence] -> makeKI bytes    
        Right s -> pureError $ parseErr $ "Unknown key identifier " <> Text.pack (show s)
  where
    makeKI bytes = 
        let len = BS.length bytes
        in if len == 20
            then pure $ mkKI bytes
            else pureError $ parseErr $ "KI has wrong length, must be 160 bits, but it is " <> Text.pack (show len)

-- https://www.rfc-editor.org/rfc/rfc7935.html#section-2
asSha256Only :: OID -> ParseASN1 HashALG
asSha256Only = \case
    oid | oid == id_sha256 -> pure HashSHA256
        | otherwise        -> throwParseError $ "Only SHA-256 hashing algorithm is supported, unknown: " <> show oid

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


extractSiaValue :: BS.ByteString -> OID -> Maybe BS.ByteString
extractSiaValue sia oid = do 
    asns <- toMaybe $ decodeASN1' DER sia
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

getSiaExt :: Certificate -> Maybe BS.ByteString
getSiaExt c = extVal (getExts c) id_pe_sia


getRrdpNotifyUriExt :: [ExtensionRaw] -> Maybe URI
getRrdpNotifyUriExt exts = toMaybe . extractURI =<< (extVal exts id_pe_sia >>= (`extractSiaValue` id_ad_rpki_notify))

getRepositoryUriExt :: [ExtensionRaw] -> Maybe URI
getRepositoryUriExt exts = toMaybe . extractURI =<< (extVal exts id_pe_sia >>= (`extractSiaValue` id_ad_rpki_repository))

getManifestUriExt :: [ExtensionRaw] -> Maybe URI
getManifestUriExt exts = toMaybe . extractURI =<< (extVal exts id_pe_sia >>= (`extractSiaValue` id_ad_rpkiManifest))

extractURI :: BS.ByteString -> Either Text URI
extractURI u =  fmap URI $ first fmtGen $ decodeUtf8' u


getCrlDistributionPointExt :: [ExtensionRaw] -> Maybe URI
getCrlDistributionPointExt exts = extVal exts id_ce_CRLDistributionPoints >>= extractCrlDistributionPoint

extractCrlDistributionPoint :: BS.ByteString -> Maybe URI
extractCrlDistributionPoint crlDP = do    
    asns  <- toMaybe $ decodeASN1' DER crlDP
    join $ toMaybe $ flip runParseASN1 asns $ 
        onNextContainer Sequence $ 
            onNextContainer Sequence $ 
                onNextContainer (Container Context 0) $ 
                    getNextContainer (Container Context 0) >>= \case 
                            [Other Context 6 value] -> 
                                pure $ toMaybe $ extractURI value
                            _   -> 
                                pure Nothing


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
    afs  <- getMany addrFamily    
    ipv4 <- oneAddressFamily "IPv4" [ af | Left  af <- afs ]
    ipv6 <- oneAddressFamily "IPv6" [ af | Right af <- afs ]
    pure $ IpResources $ IpResourceSet ipv4 ipv6
  where
    -- https://www.rfc-editor.org/rfc/rfc3779#section-2.2.3.3
    -- Every address family may appear at most once. Silently using the first 
    -- block and dropping the rest would make the resulting resource set 
    -- implementation-dependent.
    oneAddressFamily _    []   = pure R.emptyRS
    oneAddressFamily _    [af] = pure af
    oneAddressFamily name _    = throwParseError $ 
        "More than one " <> name <> " block in the IP resource extension"

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
makeOneIP bs nz = [makePrefix bs (fromIntegral nz)]

ipvVxAddress :: Ord t 
            => ([Word8] -> t)
            -> Int
            -> (BS.ByteString -> Word64 -> b)
            -> (t -> t -> b)
            -> ParseASN1 b
ipvVxAddress wToAddr fullLength makePrefix_ rangeToPrefixes =     
    getNextContainerMaybe Sequence >>= \case
        Nothing -> getNext >>= \case
            (BitString (BitArray nzBits bs)) -> 
                onePrefix bs nzBits
            s -> 
                throwParseError ("Unexpected prefix representation: " <> show s)

        Just [BitString (BitArray nzBits bs)] ->                
                onePrefix bs nzBits

        Just [
            BitString (BitArray nzBits1 bs1),
            BitString (BitArray nzBits2 bs2)
            ] -> do
                checkPrefixEncoding nzBits1 bs1
                checkPrefixEncoding nzBits2 bs2
                let w1 = wToAddr $ BS.unpack bs1
                    w2 = wToAddr $ setLowerBitsToOne (BS.unpack bs2)
                        (fromIntegral nzBits2) fullLength
                -- https://www.rfc-editor.org/rfc/rfc3779#section-2.2.3.9
                -- min must not be bigger than max. An inverted range is not just 
                -- meaningless, it silently decodes to a completely different 
                -- (and arbitrary) set of prefixes.
                when (w1 > w2) $ 
                    throwParseError "Address range has min bigger than max"
                pure $ rangeToPrefixes w1 w2

        s -> throwParseError $ "Unexpected address representation: " <> show s
  where
    onePrefix bs nzBits = do 
        checkPrefixEncoding nzBits bs
        pure $! makePrefix_ bs nzBits

    {- The number of significant bits ends up in the prefix length, which is a 
       Word8. Without this check a certificate declaring, say, 40 significant 
       bits for an IPv4 prefix produces a prefix with mask 40, for which 
       hw-ip computes a `lastIpAddress` that precedes its `firstIpAddress`, 
       and every containment/intersection test on it misbehaves.
    -}
    checkPrefixEncoding nzBits bs = do 
        when (nzBits > fromIntegral fullLength) $ 
            throwParseError $ "Prefix length " <> show nzBits 
                <> " is bigger than the address size " <> show fullLength
        when (BS.length bs > (fullLength + 7) `div` 8) $ 
            throwParseError $ "Prefix is encoded in " <> show (BS.length bs) 
                <> " octets, too many for " <> show fullLength <> "-bit addresses"

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


parseIpExt :: [ASN1] -> PureValidatorT IpResources
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
parseAsnExt :: [ASN1] -> PureValidatorT AsResources
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
            IntVal asn -> AS <$> as' asn
            something  -> throwParseError $ "Unknown ASN specification " <> show something
        Just [IntVal b, IntVal e] -> do 
            b' <- as' b
            e' <- as' e
            -- https://www.rfc-editor.org/rfc/rfc3779#section-3.2.3.8, min <= max
            when (b' > e') $ 
                throwParseError $ "AS range " <> show b <> "-" <> show e <> " has min bigger than max"
            pure $ ASRange b' e'
        Just something -> throwParseError $ "Unknown ASN specification " <> show something
  where
    as' = either throwParseError pure . mkAsn
