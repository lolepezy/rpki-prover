{-# LANGUAGE OverloadedStrings #-}

module RPKI.Parse.Internal.Cert where

import           Control.Applicative

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base64     as B64
import qualified Data.List                  as List

import           Data.Bifunctor
import           Data.Bits

import           Data.ASN1.BinaryEncoding
import           Data.ASN1.BitArray
import           Data.ASN1.Encoding
import           Data.ASN1.Parse
import           Data.ASN1.Types

import           Data.X509

import           RPKI.Domain
import           RPKI.Resources.Resources   as R
import           RPKI.Resources.Types
import qualified RPKI.Util                  as U

import           RPKI.Parse.Internal.Common


{- |
  Parse RPKI certificate object with the IP and ASN resource extensions.
-}
parseResourceCertificate :: BS.ByteString ->
                            ParseResult (RpkiURL -> CerObject)
parseResourceCertificate bs = do
    cert <- mapParseErr $ decodeSignedObject bs      
    (rc, ski_, aki_) <- toResourceCert $ unifyCert cert
    pure $ \location -> newCert location aki_ ski_ (U.sha256s bs) rc


toResourceCert :: CertificateWithSignature -> ParseResult (ResourceCertificate, SKI, Maybe AKI)
toResourceCert cert = do  
  let exts = getExtsSign cert
  case extVal exts id_subjectKeyId of
    Just s  -> do
      rc <- parseResources cert    
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
        (Nothing, ips, Nothing, asns) -> reconsideredCert <$> cert' x509cert ips asns
    where
      broken = Left . fmtErr
      cert' x509c ips asns = do 
        ips'  <- maybe (pure emptyIpResources) (parseR parseIpExt) ips
        asns' <- maybe (pure emptyAsResources) (parseR parseAsnExt) asns
        pure $ ResourceCert x509c $ allResources ips' asns'

      parseR :: ([ASN1] -> ParseResult a) -> BS.ByteString -> ParseResult a
      parseR f bs = f =<< first fmt decoded
        where decoded = decodeASN1' BER bs
              fmt err = fmtErr $ "Couldn't parse IP address extension: " <> show err

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
parseIpExt :: [ASN1] -> ParseResult IpResources
parseIpExt asns = mapParseErr $
  flip runParseASN1 asns $ do
    afs <- onNextContainer Sequence (getMany addrFamily)    
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

    ipv4Address = ipvVxAddress R.fourW8sToW32 32  makeOneIP R.ipv4RangeToPrefixes
    ipv6Address = ipvVxAddress R.someW8ToW128 128 makeOneIP R.ipv6RangeToPrefixes

    makeOneIP bs nz = [make bs (fromIntegral nz)]

    ipvVxAddress wToAddr fullLength makePrefix rangeToPrefixes =
        getNextContainerMaybe Sequence >>= \case
            Nothing -> getNext >>= \case
                (BitString (BitArray nzBits bs)) ->
                    pure $ makePrefix bs nzBits
                s -> throwParseError ("Unexpected prefix representation: " <> show s)
            Just [
                BitString (BitArray _       bs1),
                BitString (BitArray nzBits2 bs2)
                ] ->
                    let w1 = wToAddr $ BS.unpack bs1
                        w2 = wToAddr $ setLowerBitsToOne (BS.unpack bs2)
                            (fromIntegral nzBits2) fullLength
                    in pure $ rangeToPrefixes w1 w2

            s -> throwParseError ("Unexpected address representation: " <> show s)

    --
    -- Set all the bits to `1` starting from `setBitsNum`
    -- `allBitsNum` is the total number of bits.
    --
    setLowerBitsToOne ws setBitsNum allBitsNum =
        R.rightPad (allBitsNum `div` 8) 0xFF $
            List.zipWith setBits ws (map (*8) [0..])
        where
            setBits w8 i | i < setBitsNum && setBitsNum < i + 8 = w8 .|. extra (i + 8 - setBitsNum)
                         | i < setBitsNum = w8
                         | otherwise = 0xFF
            extra lastBitsNum =
                List.foldl' (\w i -> w .|. (1 `shiftL` i)) 0 [0..lastBitsNum-1]


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
parseAsnExt asnBlocks = mapParseErr $ flip runParseASN1 asnBlocks $
    onNextContainer Sequence $
      -- we only want the first element of the sequence
      AsResources <$> onNextContainer (Container Context 0)
          (getNull_ (pure Inherit) <|>
           R.toRS <$> onNextContainer Sequence (getMany asOrRange))
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
--
subjectPublicKeyInfo :: Certificate -> EncodedBase64
subjectPublicKeyInfo cert = EncodedBase64 $ B64.encodeBase64' $ 
  encodeASN1' DER $ (toASN1 $ certPubKey cert) []

getX509Cert :: SignedExact c -> c
getX509Cert = signedObject . getSigned