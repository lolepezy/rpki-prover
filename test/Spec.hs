{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

import Data.Bifunctor
import Data.Maybe
import Data.Bits
import Data.Word
import Data.ByteString.Conversion

import Data.ASN1.Types
import Data.ASN1.BitArray
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding

import Data.ASN1.Types
import Data.ASN1.BitArray
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.ASN1.Parse

import Data.X509.Memory
import qualified Data.X509 as X509

import RPKI.Domain
import RPKI.Parsers
import RPKI.Store

testCertParsing = do
  let path = "/Users/mpuzanov/ripe/rpki-validator//rpki-validator-cli/target/result.txt/rta/unvalidated/rpki.ripe.net/repository/DEFAULT/d6p7QQYKDfmHXTjSKqqbcfNrmAM.cer"

  cert <- B.readFile path
  let (Right content :: Either String (X509.SignedExact X509.Certificate)) = X509.decodeSignedObject cert
  let (X509.Extensions (Just xt)) = X509.certExtensions $ X509.signedObject $ X509.getSigned content

  let addrExt = [1, 3, 6, 1, 5, 5, 7, 1, 7]
  let asnExt = [1, 3, 6, 1, 5, 5, 7, 1, 8]

  let addr = [ X509.tryExtRawASN1 e | e@(X509.ExtensionRaw oid _ c) <- xt, oid == addrExt ]
  let asn = [ X509.tryExtRawASN1 e | e@(X509.ExtensionRaw oid _ c) <- xt, oid == asnExt ]  

  let [Right a] = addr

  -- let x = (flip runParseASN1) a $ getNextContainerMaybe Sequence

  let x = (flip runParseASN1) a $ let 
              z                = onNextContainer Sequence (getMany addrFamily)
              addrFamily       = onNextContainer Sequence $ do
                (OctetString familyType) <- getNext
                let addressParser = case familyType of 
                            _ | familyType == "\NUL\SOH" -> ipv4Address
                              | familyType == "\NUL\STX" -> ipv6Address
                onNextContainer Sequence (getMany addressParser)
              ipv4Address = do
                (BitString (BitArray n bs)) <- getNext
                let w32 = four28sToW32 (B.unpack bs)                
                pure ("v4", B.unpack bs, w32)
              ipv6Address = do
                (BitString (BitArray n bs)) <- getNext                
                let upacked = rightPad 16 0 (B.unpack bs)
                    w128 = (four28sToW32 (drop 12 upacked),
                            four28sToW32 (drop 8 upacked),
                            four28sToW32 (drop 4 upacked),
                            four28sToW32 upacked)                
                pure ("v6", B.unpack bs, 0)
              four28sToW32 s = let 
                foldW8toW32 (w32, shift) w8 =  (w32 + (fromIntegral w8 :: Word32) `shiftL` shift, shift - 8)
                (w32, _) = L.foldl' foldW8toW32 (0 :: Word32, 24) s
                  in w32                                  
              rightPad :: Int -> a -> [a] -> [a]
              rightPad n a as = go 0 as
                where
                  go acc [] | acc <= n  = a : go (acc + 1) []
                            | otherwise = []  
                  go acc (x : as) = go (acc + 1) as
                  
            in z   
    

  putStrLn $ "x = " ++ show x
  putStrLn ""

  putStrLn $ "IP = " ++ show a
  -- putStrLn $ "ASN = " ++ show asn

testCrlParsing = do
  let path = "/Users/mpuzanov/ripe/tmp/rpki/validator-3/1/rpki-validator-3.0-DEV20190107133347/rsync/rpki.afrinic.net/873/repository/ripe/f3rBgIl5g-Kek3wKGHgDwHJ1VUU.crl"

  cert <- B.readFile path
  let (content :: Either String (X509.SignedExact X509.CRL)) = X509.decodeSignedObject cert
  putStrLn $ "content = " ++ show content  
  


main :: IO ()
main = do 
  testCertParsing
  
