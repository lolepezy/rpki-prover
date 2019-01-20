{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module RPKI.Parsers where

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

newtype ParseError s = ParseError s
  deriving (Eq, Show, Functor)

type ParseResult a = Either (ParseError T.Text) a


parseMft :: B.ByteString -> ParseResult MFT
parseMft _ = Left (ParseError "Not implemented")

parseCrl :: B.ByteString -> ParseResult MFT
parseCrl _ = Left (ParseError "Not implemented")

parseCert :: B.ByteString -> ParseResult Cert
parseCert b = do
      let certificate :: Either String (SignedExact Certificate) = decodeSignedObject b
      resourceSet <- parseResourceExtensions =<< first (ParseError . T.pack) certificate
      pure $ Cert (RealCert b) (SKI (KI B.empty)) resourceSet
    where
      parseResourceExtensions :: SignedExact Certificate -> ParseResult (S.Set Resource)
      parseResourceExtensions = getExts . certExtensions . signedObject . getSigned            
      getExts (Extensions exts) = let 
        existingExts = maybe [] id exts 
          in case (extVal existingExts oid_ip, extVal existingExts oid_asn) of
            (Nothing,  Nothing)   -> Left (ParseError (T.pack "No IP or ASN extensions in the certificate"))
            (Just ips, Nothing)   -> parseResources ips parseIpExt
            (Nothing,  Just asns) -> parseResources asns parseAsnExt
            (Just ips, Just asns) -> S.union <$> parseResources ips parseIpExt
                                             <*> parseResources asns parseAsnExt

      parseResources :: B.ByteString -> ([ASN1] -> ParseResult a) -> ParseResult a
      parseResources b f = do
        f =<< first fmt decoded
        where decoded = decodeASN1' BER b
              fmt err = ParseError $ T.pack "Couldn't parse IP address extension:" `T.append` (T.pack (show err))


oid_pkix, oid_pe, oid_ip, oid_asn :: [Integer]
oid_pkix = [1, 3, 6, 1, 5, 5, 7]
oid_pe  = oid_pkix ++ [1]
oid_ip  = oid_pe ++ [7]
oid_asn = oid_pe ++ [8]
          
extVal :: [ExtensionRaw] -> OID -> Maybe B.ByteString
extVal exts oid = listToMaybe [c | ExtensionRaw oid' _ c <- exts, oid' == oid ]


{-
  Parse IP address extension.

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
parseIpExt :: [ASN1] -> ParseResult (S.Set Resource)
parseIpExt addrBlocks = first (ParseError . T.pack) $
    (flip runParseASN1) addrBlocks $ do
    addressFamilies <- onNextContainer Sequence (getMany addrFamily)
    pure $ S.unions (S.fromList <$> addressFamilies)
    where 
      addrFamily = onNextContainer Sequence $ do
        (OctetString familyType) <- getNext
        let addressParser = case familyType of 
              "\NUL\SOH" -> ipv4Address
              "\NUL\STX" -> ipv6Address
              af         -> throwParseError $ "Unsupported address family " ++ show af
        onNextContainer Sequence (getMany addressParser)       
      ipv4Address = ipvVxAddress fourW8sToW32 mkIpv4 Ipv4R Ipv4P mkIpv4Block 32
      ipv6Address = ipvVxAddress someW8ToW128 mkIpv6 Ipv6R Ipv6P mkIpv6Block 128

      ipvVxAddress wToAddr mkIpVx range prefix mkBlock fullLength =
        -- try range first 
        getNextContainerMaybe Sequence >>= \case
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
          -- now try prefix      
          Nothing -> getNext >>= \case
            (BitString (BitArray nonZeroBits bs)) -> do
              let w = wToAddr (B.unpack bs)
              pure $ IpP $ prefix $ mkBlock w (fromIntegral nonZeroBits)

            s -> throwParseError ("weird " ++ show s)        

      setLowerBitsToOne ws setBitsNum allBitsNum =
        rightPad (allBitsNum `div` 8) (0xFF :: Word8) $ 
          map setBits $ L.zip ws (map (*8) [0..])
        where
          setBits (w8, i) | i < setBitsNum && setBitsNum < i + 8 = w8 .|. (extra (i + 8 - setBitsNum))
                          | i < setBitsNum = w8
                          | otherwise = 0xFF :: Word8  
          extra lastBitsNum = 
            L.foldl' (\w i -> w .|. (1 `shiftL` i)) 0 [0..lastBitsNum-1]
                    
      fourW8sToW32 ws = fst $ L.foldl' foldW8toW32 (0 :: Word32, 24) ws
        where
          foldW8toW32 (w32, shift) w8 = (
              w32 + (fromIntegral w8 :: Word32) `shiftL` shift, 
              shift - 8)        
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
          go acc (x : as) = x : go (acc + 1) as    


parseAsnExt :: [ASN1] -> ParseResult (S.Set Resource)
parseAsnExt asnBlocks = first (ParseError . T.pack) $
  (flip runParseASN1) asnBlocks $ do
  -- asIds <- onNextContainer Sequence (asIdentifiers)
  -- pure $ S.unions (S.fromList <$> asIds)
    throwParseError $ "bla"
    -- pure $ Left $ ParseError $ T.pack ""
  -- where     
  --   addrFamily = onNextContainer Sequence $ do
  --     (OctetString familyType) <- getNext
  --     let addressParser = case familyType of 
  --           "\NUL\SOH" -> ipv4Address
  --           "\NUL\STX" -> ipv6Address
  --           af         -> throwParseError $ "Unsupported address family " ++ show af
  --     onNextContainer Sequence (getMany addressParser)          
  --   ipv4Address = do
  --     (BitString (BitArray nonZeroBits bs)) <- getNext
  --     let w32 = fourW8sToW32 (B.unpack bs)
  --     pure $ IpR $ Ipv4 $ mkIpv4Block w32 (fromIntegral nonZeroBits)