{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Concurrent.Async
import Control.Monad
import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.MultiMap as MultiMap
import Data.MultiMap (MultiMap)

import Data.X509
import Data.X509.Validation

import Data.ASN1.Types
import Data.ASN1.BinaryEncoding
import Data.ASN1.Encoding

import RPKI.Domain
import RPKI.SignTypes
import RPKI.Validate
import RPKI.Parse.Common
import RPKI.Parse.Cert
import RPKI.Parse.MFT
import RPKI.Parse.ROA

import RPKI.Util

import System.FilePath.Find

runValidate :: IO ()
runValidate = do  
  parsedObjects <- readRepo              

  let errors = [ e | Left e <- parsedObjects ]  
  let objects = [ o | Right os <- parsedObjects, o <- os ]

  putStrLn $ "errors = " <> show (length errors)
  -- putStrLn $ "error 0 = " <> show (head errors)
  putStrLn $ "objects = " <> show (length objects)  
  let Right signatureValidations = validateKeys objects
  putStrLn $ "valid signature = " <> show (length [s | s@SignaturePass <- signatureValidations])
  putStrLn $ "invalid signature = " <> show (length [s | s@(SignatureFailed SignatureInvalid) <- signatureValidations])  
  putStrLn $ "invalid pubkey mismatch = " <> show (length [s | s@(SignatureFailed SignaturePubkeyMismatch) <- signatureValidations])  
  putStrLn $ "invalid not implemented = " <> show (length [s | s@(SignatureFailed SignatureUnimplemented) <- signatureValidations])  
  

readRepo :: IO [Either (ParseError T.Text) [RpkiObject]]
readRepo = do
  let repository = "/Users/mpuzanov/ripe/tmp/rpki/repo/ripe.net/"  
  let expr = (fileName ~~? "*.cer") ||? 
             (fileName ~~? "*.roa") ||?
             (fileName ~~? "*.mft") 
             -- add (fileName ~~? "*.crl")
  fileNames <- find always expr repository 
  -- SLY.toList $ SLY.fromFoldable fileNames 
  --            |& SLY.mapM (\f -> B.readFile f >>= readObject f)             

  parallel 50 (\f -> B.readFile f >>= readObject f) fileNames 
  -- mapM (\f -> B.readFile f >>= readObject f) fileNames


testSignature :: IO ()
testSignature = do
  parsed <- readRepo
  let objects = [ o | Right os <- parsed, o <- os ]

  let roaFile = "/Users/mpuzanov/ripe/tmp//rpki/repo/ripe.net/repository/DEFAULT/5b/6ab497-16d9-4c09-94d9-1be4c416a09c/1/XY9rz4RATnJEwJxBpE7ExbD-Ubk.roa"  
  bs  <- B.readFile roaFile
  roa <- readObject roaFile bs

  putStrLn $ "roa blob = " ++ show bs

  let Right [RpkiObject (RpkiMeta { aki = Just (AKI ki)}) (RoaRO cms@(CMS so))] = roa
  let CertificateWithSignature 
        _ 
        (SignatureAlgorithmIdentifier signAlgorithm) 
        (SignatureValue sign) encoded = scCertificate $ soContent so

  putStrLn $ "encoded = " ++ show encoded

  let skiMap = bySKIMap objects
  let [x@(RpkiObject _ (CerRO cer@(CerObject (ResourceCert parentCert))))] = MultiMap.lookup (SKI ki) skiMap

  let s = validateSignature (RoaRO cms) cer

  putStrLn $ "x = " <> show x
  putStrLn $ "s = " <> show s

  let bss = [ B.take len $ B.drop b bs | b <- [1..B.length bs - 1], len <- [1..B.length bs - b]]

  let pubKey = certPubKey $ signedObject $ getSigned $ withRFC parentCert certX509              
  let checks = map (\b -> (b, verifySignature signAlgorithm pubKey b sign)) bss 

  putStrLn $ "valid signature = " <> show [ b | (b, SignaturePass) <- checks]

  putStrLn $ "validateCMSSignature = " ++ (show $ validateCMSSignature cms)

  
testCrl :: IO ()
testCrl = do
  bs  <- B.readFile "/Users/mpuzanov/ripe/tmp/rpki/repo/repository/ripe-ncc-ta.crl"
  let d = decodeSignedCRL bs
  putStrLn $ "d = " ++ show d  
  case decodeASN1' BER bs of
    Left e -> putStrLn $ show e
    Right asns -> do
      let crl :: Either String (CRL, [ASN1]) = fromASN1 asns  
      putStrLn $ "asns = " ++ show asns  
  

readObject :: String -> B.ByteString -> IO (Either (ParseError T.Text) [RpkiObject])
readObject fileName content = do    
  let ext = L.drop (L.length fileName - 3) fileName
  let uri = URI $ T.pack fileName
  pure $! case ext of
        "cer" -> do
            f <- parseResourceCertificate content
            let (meta, o) = f uri
            pure $! [RpkiObject meta $ CerRO o]

        "mft" -> do
            f <- parseMft content             
            let (meta, o) = f uri
            pure $! [RpkiObject meta $ MftRO o]

        "roa" -> do
            f <- parseRoa content
            let (meta, o) = f uri
            pure $! [RpkiObject meta $ RoaRO o]

        _     -> pure $! [] 

validateKeys :: [RpkiObject] -> Either String [SignatureVerification]
validateKeys objects = 
  case [ ro | ro@(RpkiObject (RpkiMeta { aki = Nothing }) _) <- objects ] of
    []                   -> Left $ "No TA certificate"    
    [RpkiObject meta ro] -> Right $ validateChildren meta ro
  where    
    validateChildren :: RpkiMeta -> RO -> [SignatureVerification]
    validateChildren meta (CerRO cert) = signatureChecks <> childSignatureChecks
      where
        childSignatureChecks = L.concat [ validateChildren m cer | RpkiObject m cer@(CerRO _) <- children meta ]      
        signatureChecks = map (\(RpkiObject _ c) -> validateSignature c cert) $ children meta
    validateChildren _ _ = []

    children meta = MultiMap.lookup (AKI ki) akiMap
      where
        SKI ki = ski meta

    akiMap = byAKIMap objects
    


bySKIMap :: [RpkiObject] -> MultiMap SKI RpkiObject
bySKIMap ros = MultiMap.fromList [ (ski, ro) | ro@(RpkiObject (RpkiMeta {..}) _) <- ros ]

byAKIMap :: [RpkiObject] -> MultiMap AKI RpkiObject
byAKIMap ros = MultiMap.fromList [ (a, ro) | ro@(RpkiObject (RpkiMeta { aki = Just a }) _) <- ros ]

  

main :: IO ()
main = do 
  -- testCrl
  runValidate  
  -- testSignature
