{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

import qualified Data.ByteString as B

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

import Data.Bifunctor
import Data.Maybe

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
              addrFamily       = onNextContainer Sequence (familyType >>= addresses)
              familyType       = getNext >>= \(OctetString bs) -> pure Ipv4Family
              addresses = \case 
                Ipv4Family -> onNextContainer Sequence (getMany ipv4Addres)
                Ipv6Family -> onNextContainer Sequence (getMany ipv6Addres)
              ipv4Addres = getNext >>= \(BitString (BitArray n bs)) -> return ("v4", bs)
              ipv6Addres = getNext >>= \(BitString (BitArray n bs)) -> return ("v6", bs)
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
  
