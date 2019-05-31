{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B

import Control.Monad
import Control.Concurrent.Async

import Data.ASN1.Types
import Data.ASN1.BitArray
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.ASN1.Parse

import Data.X509.Memory
import qualified Data.X509 as X509

import RPKI.Domain
import RPKI.Parse.Cert
import RPKI.Parse.MFT
import RPKI.Parse.ROA
import RPKI.Store

import System.Environment
import System.Directory
import System.FilePath.Find

import RrdpSpec

testCertParsing = do
  -- let path = "/Users/mpuzanov/dev/haskell/rpki-prover/test/big_cert.cer"
  let path = "/Users/mpuzanov/ripe/tmp/rpki-validator-app-2.25/data/rsync/repository.lacnic.net/rpki/lacnic/ff14e9055d5afaa37fbe20f4a26bd13c8f18d79a.cer"
  -- let path = "/Users/mpuzanov/ripe/tmp/rsync/rpki.apnic.net/873/member_repository/A917150D/2B63EBE6326811E4B59A4118C4F9AE02/L3M4gVlKnWpaICg9mMRk2VX_A3k.mft"
  -- let path = "/Users/mpuzanov/ripe/tmp/rsync/rpki.afrinic.net/873/repository/afrinic/imnjE5vX78CApC4WBIM5j6bHW7Y.cer"

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
  let xcert = parseResourceCertificate (URI "blabla.cer") cert

  -- putStrLn $ "xt = " ++ show xt
  -- putStrLn $ "xcert = " ++ show xcert
  putStrLn $ "x = " ++ show x
  -- putStrLn $ "mft = " ++ show mft
  putStrLn ""
  putStrLn ""

  -- putStrLn $ "IP = " ++ show x
  putStrLn ""
  putStrLn ""
  putStrLn ""
  putStrLn ""
  -- putStrLn $ "ASN = " ++ show z

testSignedObjectParsing = do
  -- let path = "/Users/mpuzanov/ripe/tmp/rpki-validator-app-2.25/data/rsync/rpki.apnic.net/member_repository/A91362A0/8032CBF0E3A111E6BF3B0068C4F9AE02/gyAF3EXYwTKohRVcQx0zMZrPbY0.crl"
  let mftFile = "/Users/mpuzanov/ripe/tmp/rpki-validator-app-2.25/data/rsync/repository.lacnic.net/rpki/lacnic/fe88c371-48d2-4980-b341-79bd4bd55a11/8a2ba27a87a82b04425116ea7339ee577b624203.mft"
  let roaFile = "/Users/mpuzanov/ripe/tmp/rpki-validator-app-2.25/data/rsync/rpki.afrinic.net/repository/member_repository/F361842C/06D149FC58F011E7A967243DF8AEA228/E91A0C4C6E5F11E8AE1A3342F8AEA228.roa"

  let repository = "/Users/mpuzanov/ripe/tmp/rpki-validator-app-2.25/data/rsync"

  roa <- B.readFile roaFile 
  -- let d = decodeASN1' BER mft
  let r = parseRoa roa
  putStrLn $ "roa = " ++ show r
  

testAllManifests = testAllObjects parseMft "mft"  
testAllRoas      = testAllObjects parseRoa "roa" 
testAllCerts     = testAllObjects (parseResourceCertificate (URI "something.cer")) "cer" 

testAllObjects parseIt t = do  
  let repository = "/Users/mpuzanov/ripe/tmp/rpki-validator-app-2.25/data/rsync"

  objs   <- find always (fileName ~~? ("*." ++ t)) repository
  asyncs <- forM objs $ \obj -> async $ do    
              p <- parseIt <$> B.readFile obj
              putStrLn $ "path = " ++ show obj
              case p of
                Left e  -> putStrLn $ obj ++ ", error = " ++ show e
                Right _ -> pure ()
              pure (obj, p)
  parsed <- mapM wait asyncs              

  let errors = [ (e, p) | (p, Left e) <- parsed ]  

  putStrLn $ "errors = " ++ show (length errors)
  




main :: IO ()
main = do 
  -- testCertParsing
  -- testSignedObjectParsing
  -- testAllCerts
  -- testAllRoas
  -- testAllManifests
  testParseSnapshot
