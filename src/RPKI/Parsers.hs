{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module RPKI.Parsers where

import qualified Data.ByteString as B  
import qualified Data.Text as T

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

import Data.Bifunctor
import Data.Maybe

import Data.ASN1.Types
import Data.ASN1.BitArray
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.ASN1.Parse

import Data.X509.Memory
import Data.X509

import RPKI.Domain

newtype ParseError = ParseError T.Text

type ParseResult a = Either ParseError a


parseMft :: B.ByteString -> ParseResult MFT
parseMft _ = Left (ParseError "Not implemented")

parseCrl :: B.ByteString -> ParseResult MFT
parseCrl _ = Left (ParseError "Not implemented")

parseCert :: B.ByteString -> ParseResult Cert
parseCert b = let 
      content :: Either String (SignedExact Certificate) = decodeSignedObject b
      certificate = first (ParseError . T.pack) content 
      resourceSet  = parseResourceExtension <$> certificate
    in case content of
      Left e   -> Left (ParseError (T.pack e))
      Right se -> let xt = certExtensions $ signedObject $ getSigned se
                    in Left (ParseError "")
    where
      parseResourceExtension = getExts . certExtensions . signedObject . getSigned      
      getExts (Extensions Nothing)     = Left (ParseError "No extensions in the certificate")
      getExts (Extensions (Just exts)) = 
        case (extVal exts oid_ip, extVal exts oid_asn) of
          (Nothing, Nothing) -> Left (ParseError "No IP or ASN extensions in the certificate")
          (ips, asns)        -> let
              ipResources  = maybe (Right S.empty) parseIPs ips
              asnResources = maybe (Right S.empty) parseASNs asns
              in do i <- first (ParseError . T.pack) ipResources 
                    a <- first (ParseError . T.pack) asnResources                
                    pure $ S.union i a

      parseResources :: B.ByteString -> ([ASN1] -> Either String a) -> Either String a
      parseResources b f = 
        f =<< first (\err -> "Couldn't parse IP:" ++ show err) decoded
        where decoded = decodeASN1' BER b       

      parseIPs :: B.ByteString -> Either String (S.Set Resource)
      parseIPs b = parseResources b $ \case
        [Start Sequence,
          Start Sequence,
          OctetString os,
          Start Sequence,
            BitString (BitArray n bs),
          End Sequence,
          End Sequence,
          End Sequence] -> Right S.empty
        _               -> Left "Wrong format of IP"

      parseASNs :: B.ByteString -> Either String (S.Set Resource)
      parseASNs _ = parseResources b $ \case
        [Start Sequence,
          Start (Container Context 0),
          Start Sequence,
            IntVal 28101,
          End Sequence,
          End (Container Context 0),
          End Sequence] -> Right S.empty
        _ -> Left "Wrong format of IP"


oid_pkix, oid_pe, oid_ip, oid_asn :: [Integer]
oid_pkix = [1, 3, 6, 1, 5, 5, 7]
oid_pe  = oid_pkix ++ [1]
oid_ip  = oid_pe ++ [7]
oid_asn = oid_pe ++ [8]
          
extVal :: [ExtensionRaw] -> OID -> Maybe B.ByteString
extVal exts oid = listToMaybe [c | ExtensionRaw oid' _ c <- exts, oid' == oid ]


data AddrFamily = Ipv4Family | Ipv6Family

{-
  https://tools.ietf.org/html/rfc3779

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
-- parseIpExt :: [ASN1] -> (S.Set Resource)
-- parseIpExt addrBlocks = let 
--       do
  -- let x = (flip runParseASN1) a $ let 
    -- z                = onNextContainer Sequence (getMany addrFamily)
    -- addrFamily       = onNextContainer Sequence $ do
    --   (OctetString familyType) <- getNext
    --   let addressParser = case familyType of 
    --               _ | familyType == "\NUL\SOH" -> ipv4Address
    --                 | familyType == "\NUL\STX" -> ipv6Address
    --   onNextContainer Sequence (getMany addressParser)
    -- ipv4Address = getNext >>= \(BitString (BitArray n bs)) -> return ("v4", bs)
    -- ipv6Address = getNext >>= \(BitString (BitArray n bs)) -> return ("v6", bs)

--     in 
--       S.empty
--   where 
--     getAddrBlocks = getAddrFamily >>= get
