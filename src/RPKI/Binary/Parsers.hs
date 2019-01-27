{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE KindSignatures #-}

module RPKI.Binary.Parsers where

import Control.Applicative

import qualified Data.ByteString as B  
import qualified Data.Text as T

import qualified Data.Set as S
import qualified Data.List as L

import Data.Bifunctor
import Data.Maybe
import Data.Word
import Data.Bits

import Data.ASN1.Types
import Data.ASN1.BitArray
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.ASN1.Parse

import Data.X509

import RPKI.Domain 
import RPKI.Binary.Const 
import RPKI.Binary.ASN1Util 

newtype ParseError s = ParseError s
  deriving (Eq, Show, Functor)

type ParseResult a = Either (ParseError T.Text) a

parseMft :: B.ByteString -> ParseResult MFT
parseMft _ = Left (ParseError "Not implemented")

parseCrl :: B.ByteString -> ParseResult MFT
parseCrl _ = Left (ParseError "Not implemented")

parseCert :: B.ByteString -> ParseResult (Either (Cert 'Strict) (Cert 'Reconsidered))
parseCert b = do
      let certificate :: Either String (SignedExact Certificate) = decodeSignedObject b
      let getExtensions = certExtensions . signedObject . getSigned
      Extensions extensions <- getExtensions <$> mapParseErr certificate
      let ext' = extVal $ maybe [] id extensions
      case ext' id_subjectKeyId of 
        Nothing  -> broken "No SKI"
        Just ski' -> do
          case (ext' id_pe_ipAddrBlocks, 
                ext' id_pe_ipAddrBlocks_v2, 
                ext' id_pe_autonomousSysIds, 
                ext' id_pe_autonomousSysIds_v2) of
            (Nothing, Nothing, _, _) -> broken "No IP extension"
            (Just _, Just _, _, _)   -> broken "Both IP extensions"
            (_, _, Nothing, Nothing) -> broken "No ASN extension"
            (_, _, Just _, Just _)   -> broken "Both ASN extensions"
            (Just _, _, _, Just _)   -> broken "There is both IP V1 and ASN V2 extensions"
            (_, Just _, Just _, _)   -> broken "There is both IP V2 and ASN V1 extensions"                
            (Just ips, Nothing, Just asns, Nothing) -> Left  <$> cert' ski' ips asns
            (Nothing, Just ips, Nothing, Just asns) -> Right <$> cert' ski' ips asns          
    where
      broken = Left . fmtErr
      cert' ski' ips asns = (Cert (RealCert b) (SKI (KI ski'))) <$> 
          (parseResources parseIpExt ips) <*> 
          (parseResources parseAsnExt asns)

      parseResources :: ([ASN1] -> ParseResult a) -> B.ByteString -> ParseResult a
      parseResources f ext = do
        f =<< first fmt decoded
        where decoded = decodeASN1' BER ext
              fmt err = fmtErr $ "Couldn't parse IP address extension:" ++ show err
          
extVal :: [ExtensionRaw] -> OID -> Maybe B.ByteString
extVal exts oid = listToMaybe [c | ExtensionRaw oid' _ c <- exts, oid' == oid ]


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
    (flip runParseASN1) addrBlocks $ do
    afs <- onNextContainer Sequence (getMany addrFamily)
    let ipv4family = head [ af | Left  af <- afs ]
    let ipv6family = head [ af | Right af <- afs ]
    pure $ IpResourceSet ipv4family ipv6family
    where      
      addrFamily = onNextContainer Sequence $ do
        (OctetString familyType) <- getNext
        case familyType of 
              "\NUL\SOH" -> Left  <$> onNextContainer Sequence (ipResourceSet ipv4Address)
              "\NUL\STX" -> Right <$> onNextContainer Sequence (ipResourceSet ipv6Address)       
              af         -> throwParseError $ "Unsupported address family " ++ show af
        where
          ipResourceSet address = 
            (getNull (pure Inherit)) <|> 
            ((RS . S.fromList) <$> (getMany address))
      
      ipv4Address = ipvVxAddress fourW8sToW32 mkIpv4 Ipv4R Ipv4P mkIpv4Block 32
      ipv6Address = ipvVxAddress someW8ToW128 mkIpv6 Ipv6R Ipv6P mkIpv6Block 128

      ipvVxAddress wToAddr mkIpVx range prefix mkBlock fullLength =        
        getNextContainerMaybe Sequence >>= \case
          Nothing -> getNext >>= \case
            (BitString (BitArray nonZeroBits bs)) -> do
              let w = wToAddr (B.unpack bs)
              pure $ IpP $ prefix $ mkBlock w (fromIntegral nonZeroBits)

            s -> throwParseError ("Unexpected prefix representation: " ++ show s)  
          Just [
              (BitString (BitArray nonZeroBits1 bs1)), 
              (BitString (BitArray nonZeroBits2 bs2))
            ] -> do
              let w1 = wToAddr $ B.unpack bs1
              let w2 = wToAddr $ setLowerBitsToOne (B.unpack bs2)
                        (fromIntegral nonZeroBits2) fullLength 
              pure $ case mkIpVx w1 w2 of
                Left  r -> IpR $ range r
                Right p -> IpP $ prefix p          

          s -> throwParseError ("Unexpected address representation: " ++ show s)

      setLowerBitsToOne ws setBitsNum allBitsNum =
        rightPad (allBitsNum `div` 8) 0xFF $ 
          map setBits $ L.zip ws (map (*8) [0..])
        where
          setBits (w8, i) | i < setBitsNum && setBitsNum < i + 8 = w8 .|. (extra (i + 8 - setBitsNum))
                          | i < setBitsNum = w8
                          | otherwise = 0xFF  
          extra lastBitsNum = 
            L.foldl' (\w i -> w .|. (1 `shiftL` i)) 0 [0..lastBitsNum-1]
                    
      fourW8sToW32 ws = fst $ L.foldl' foldW8toW32 (0 :: Word32, 24) ws
        where
          foldW8toW32 (w32, shift') w8 = (
              w32 + (fromIntegral w8 :: Word32) `shiftL` shift', 
              shift' - 8)       
 
      someW8ToW128 ws = (
            fourW8sToW32 (take 4 unpacked),
            fourW8sToW32 (take 4 (drop 4 unpacked)),
            fourW8sToW32 (take 4 (drop 8 unpacked)),
            fourW8sToW32 (take 4 (drop 12 unpacked))
          ) 
        where unpacked = rightPad 16 0 ws
                         
      rightPad :: Int -> a -> [a] -> [a]
      rightPad n a as = go 0 as
        where
          go acc [] | acc < n  = a : go (acc + 1) []
                    | otherwise = []  
          go acc (x : xs) = x : go (acc + 1) xs    

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
parseAsnExt asnBlocks = mapParseErr $ (flip runParseASN1) asnBlocks $
    onNextContainer Sequence $ 
      -- we only want the first element of the sequence
      onNextContainer (Container Context 0) $
        (getNull (pure Inherit)) <|> 
        (RS . S.fromList) <$> onNextContainer Sequence (getMany asOrRange)
  where
    asOrRange = getNextContainerMaybe Sequence >>= \case       
        Nothing -> getNext >>= \case
          IntVal asn -> pure $ AS $ as' asn        
          something  -> throwParseError $ "Unknown ASN specification " ++ show something
        Just [IntVal b, IntVal e] -> pure $ ASRange (as' b) (as' e)
        Just something -> throwParseError $ "Unknown ASN specification " ++ show something
      where 
        as' = ASN . fromInteger    


fmtErr :: String -> ParseError T.Text
fmtErr = ParseError . T.pack

mapParseErr :: Either String a -> ParseResult a       
mapParseErr = first fmtErr