{-# LANGUAGE OverloadedStrings   #-}

module RPKI.Parse.Internal.Cert where

import           Control.Applicative

import qualified Data.ByteString          as B
import qualified Data.ByteString.Base64   as B64

import qualified Data.List                as L
import           Data.List.NonEmpty       (NonEmpty ((:|)))
import qualified Data.Set                 as S

import           Data.Bifunctor
import           Data.Bits

import           Data.ASN1.BinaryEncoding
import           Data.ASN1.BitArray
import           Data.ASN1.Encoding
import           Data.ASN1.Parse
import           Data.ASN1.Types

import           Data.X509

import           RPKI.Domain
import           RPKI.IP
import qualified RPKI.IP                  as IP
import qualified RPKI.Util                as U

import           RPKI.Parse.Internal.Common

{- |
  Parse RPKI certificate object with the IP and ASN resource extensions.
-}
parseResourceCertificate :: B.ByteString ->
                            ParseResult (URI -> CerObject)
parseResourceCertificate bs = do
  certificate <- mapParseErr $ decodeSignedObject bs  
  let signedCertificate = unifyCert certificate
  (rc, ski_, aki_) <- parseResourceCert signedCertificate  
  let meta location = RpkiMeta {
          aki  = aki_,
          hash = U.sha256s bs,
          locations = location :| []
      } 
  pure $ \location -> (FullMeta (meta location) ski_, rc)


toResourceCert :: CertificateWithSignature -> ParseResult ResourceCertificate
toResourceCert sc = do 
  (rc, _, _) <- parseResourceCert sc
  pure rc

parseResourceCert :: CertificateWithSignature -> ParseResult (ResourceCertificate, SKI, Maybe AKI)
parseResourceCert certificate = do  
  let exts = getExtsSign certificate
  case extVal exts id_subjectKeyId of
    Just s  -> do
      rc <- parseResources certificate    
      ki <- parseKI s
      aki' <- case extVal exts id_authorityKeyId of
            Nothing -> pure Nothing
            Just a  -> Just . AKI <$> parseKI a
      pure (rc, SKI ki, aki')
    Nothing -> Left $ fmtErr "No SKI extension"


parseResources :: CertificateWithSignature -> ParseResult ResourceCertificate
parseResources x509cert = do    
      let ext' = extVal $ getExtsSign x509cert
      case (ext' id_pe_ipAddrBlocks,
            ext' id_pe_ipAddrBlocks_v2,
            ext' id_pe_autonomousSysIds,
            ext' id_pe_autonomousSysIds_v2) of
        (Just _, Just _, _, _)   -> broken "Both IP extensions"
        (_, _, Just _, Just _)   -> broken "Both ASN extensions"
        (Just _, _, _, Just _)   -> broken "There is both IP V1 and ASN V2 extensions"
        (_, Just _, Just _, _)   -> broken "There is both IP V2 and ASN V1 extensions"
        (ips, Nothing, asns, Nothing) -> strictCert <$> cert' x509cert ips asns
        (Nothing, ips, Nothing, asns) -> reconcideredCert <$> cert' x509cert ips asns
    where
      broken = Left . fmtErr
      cert' x509c ips asns = ResourceCert x509c <$>
            traverse (parseR parseIpExt) ips <*>
            traverse (parseR parseAsnExt) asns

      parseR :: ([ASN1] -> ParseResult a) -> B.ByteString -> ParseResult a
      parseR f ext = f =<< first fmt decoded
        where decoded = decodeASN1' BER ext
              fmt err = fmtErr $ "Couldn't parse IP address extension:" ++ show err

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
parseIpExt :: [ASN1] -> ParseResult (IpResourceSet rfc)
parseIpExt addrBlocks = mapParseErr $
  flip runParseASN1 addrBlocks $ do
    afs <- onNextContainer Sequence (getMany addrFamily)    
    pure $ IpResourceSet $ rs $ [ af | Left  af <- afs ] <> [ af | Right af <- afs ]
    where
      rs []       = RS S.empty
      rs (af : _) = af
      addrFamily = onNextContainer Sequence $
        getAddressFamily  "Expected an address family here" >>= \case
          Right IP.Ipv4F -> Left  <$> ipResourceSet ipv4Address
          Right IP.Ipv6F -> Right <$> ipResourceSet ipv6Address
          Left af        -> throwParseError $ "Unsupported address family " ++ show af
        where
          ipResourceSet address =
            getNull_ (pure Inherit) <|>
            onNextContainer Sequence (RS . S.fromList <$> getMany address)

      ipv4Address = ipvVxAddress
          IP.fourW8sToW32 32
          (\bs nz -> IpP $ IP.Ipv4P $ IP.mkV4Prefix bs (fromIntegral nz))
          (\w1 w2 -> case IP.mkIpv4 w1 w2 of
                      Left  r -> IpR $ IP.Ipv4R r
                      Right p -> IpP $ IP.Ipv4P p)

      ipv6Address = ipvVxAddress
          IP.someW8ToW128 128
          (\bs nz -> IpP $ IP.Ipv6P $ IP.mkV6Prefix bs (fromIntegral nz))
          (\w1 w2 -> case IP.mkIpv6 w1 w2 of
                      Left  r -> IpR $ IP.Ipv6R r
                      Right p -> IpP $ IP.Ipv6P p)

      ipvVxAddress wToAddr fullLength makePrefix makeRange =
        getNextContainerMaybe Sequence >>= \case
          Nothing -> getNext >>= \case
            (BitString (BitArray nzBits bs)) ->
              pure $ makePrefix bs nzBits
            s -> throwParseError ("Unexpected prefix representation: " ++ show s)
          Just [
              BitString (BitArray _            bs1),
              BitString (BitArray nzBits2 bs2)
            ] ->
              let w1 = wToAddr $ B.unpack bs1
                  w2 = wToAddr $ setLowerBitsToOne (B.unpack bs2)
                        (fromIntegral nzBits2) fullLength
                in pure $ makeRange w1 w2

          s -> throwParseError ("Unexpected address representation: " ++ show s)


      setLowerBitsToOne ws setBitsNum allBitsNum =
        IP.rightPad (allBitsNum `div` 8) 0xFF $
          L.zipWith (curry setBits) ws (map (*8) [0..])
        where
          setBits (w8, i) | i < setBitsNum && setBitsNum < i + 8 = w8 .|. extra (i + 8 - setBitsNum)
                          | i < setBitsNum = w8
                          | otherwise = 0xFF
          extra lastBitsNum =
            L.foldl' (\w i -> w .|. (1 `shiftL` i)) 0 [0..lastBitsNum-1]


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
parseAsnExt :: [ASN1] -> ParseResult (ResourceSet AsResource rfc)
parseAsnExt asnBlocks = mapParseErr $ flip runParseASN1 asnBlocks $
    onNextContainer Sequence $
      -- we only want the first element of the sequence
      onNextContainer (Container Context 0) $
        getNull_ (pure Inherit) <|>
        RS . S.fromList <$> onNextContainer Sequence (getMany asOrRange)
  where
    asOrRange = getNextContainerMaybe Sequence >>= \case
        Nothing -> getNext >>= \case
          IntVal asn -> pure $ AS $ as' asn
          something  -> throwParseError $ "Unknown ASN specification " <> show something
        Just [IntVal b, IntVal e] -> pure $ ASRange (as' b) (as' e)
        Just something -> throwParseError $ "Unknown ASN specification " <> show something
      where
        as' = ASN . fromInteger


-- | https://tools.ietf.org/html/rfc5280#page-16
subjectPublicKeyInfo :: Certificate -> EncodedBase64
subjectPublicKeyInfo cert = EncodedBase64 $ B64.encode $ 
  encodeASN1' DER $ (toASN1 $ certPubKey cert) []

getX509Cert :: SignedExact c -> c
getX509Cert = signedObject . getSigned