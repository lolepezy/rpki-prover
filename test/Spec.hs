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
import RPKI.Binary.Parsers
import RPKI.Store

testCertParsing = do
  let path = "/Users/mpuzanov/dev/haskell/rpki-prover/test/big_cert.cer"
  -- let path = "/Users/mpuzanov/dev/haskell/rpki-prover/test/smaller.cer"

  cert <- B.readFile path
  let (Right content :: Either String (X509.SignedExact X509.Certificate)) = X509.decodeSignedObject cert
  let (X509.Extensions (Just xt)) = X509.certExtensions $ X509.signedObject $ X509.getSigned content

  let addrExt = [1, 3, 6, 1, 5, 5, 7, 1, 7]
  let asnExt = [1, 3, 6, 1, 5, 5, 7, 1, 8]

  let addr = [ X509.tryExtRawASN1 e | e@(X509.ExtensionRaw oid _ c) <- xt, oid == addrExt ]
  let asn = [ X509.tryExtRawASN1 e | e@(X509.ExtensionRaw oid _ c) <- xt, oid == asnExt ]  

  let [Right ip] = addr
  let [Right as] = asn

  let x = parseIpExt ip
  let z = parseAsnExt as
  let xcert = parseCert cert

  -- putStrLn $ "a = " ++ show a
  -- putStrLn $ "xcert = " ++ show xcert
  putStrLn ""
  putStrLn ""
  putStrLn ""

  -- putStrLn $ "IP = " ++ show x
  putStrLn ""
  putStrLn ""
  putStrLn ""
  putStrLn ""
  putStrLn $ "ASN = " ++ show z

testCrlParsing = do
  let path = "/Users/mpuzanov/ripe/tmp/rpki/validator-3/1/rpki-validator-3.0-DEV20190107133347/rsync/rpki.afrinic.net/873/repository/ripe/f3rBgIl5g-Kek3wKGHgDwHJ1VUU.crl"

  cert <- B.readFile path
  let (content :: Either String (X509.SignedExact X509.CRL)) = X509.decodeSignedObject cert
  putStrLn $ "content = " ++ show content  
  


main :: IO ()
main = do 
  testCertParsing
  
