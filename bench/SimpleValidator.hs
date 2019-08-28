{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Parallel.Strategies

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.MultiMap as MultiMap
import Data.MultiMap (MultiMap)

import Data.X509
import Data.X509.Validation

import Data.ASN1.BinaryEncoding
import Data.ASN1.Encoding

import RPKI.Domain
import RPKI.SignTypes
import RPKI.Validate
import RPKI.Parse.Common
import RPKI.Parse.Cert
import RPKI.Parse.MFT
import RPKI.Parse.ROA
import RPKI.Parse.CRL

import System.FilePath.Find

import RPKI.Util (parallel)

runValidate :: IO ()
runValidate = do  
  parsedObjects <- readRepo              

  let errors = [ (f, e) | (f, Left e) <- parsedObjects ]  
  let objects = [ o | (_, Right os) <- parsedObjects, o <- os ]

  putStrLn $ "errors = " <> show (length errors)
  -- putStrLn $ "errors 0 = " <> show (head errors)
  putStrLn $ "objects = " <> show (length objects)  
  let Right signatureValidations = validateKeys objects
  putStrLn $ "valid signature = " <> show (length [s | s@SignaturePass <- signatureValidations])
  putStrLn $ "invalid signature = " <> show (length [s | s@(SignatureFailed SignatureInvalid) <- signatureValidations])  
  putStrLn $ "invalid pubkey mismatch = " <> show (length [s | s@(SignatureFailed SignaturePubkeyMismatch) <- signatureValidations])  
  putStrLn $ "invalid not implemented = " <> show (length [s | s@(SignatureFailed SignatureUnimplemented) <- signatureValidations])  
  

readRepo :: IO [(String, Either (ParseError T.Text) [RpkiObject])]
readRepo = do
  let repository = "/Users/mpuzanov/ripe/tmp/rpki/repo/"  
  let expr = (fileName ~~? "*.cer") ||? 
             (fileName ~~? "*.roa") ||?
             (fileName ~~? "*.mft") ||?            
             (fileName ~~? "*.crl") 
  fileNames <- find always expr repository 
  parallel 50 (\f -> B.readFile f >>= readObject f) fileNames 
  -- mapM (\f -> B.readFile f >>= readObject f) fileNames


testSignature :: IO ()
testSignature = do
  parsed <- readRepo
  let objects = [ o | (_, Right os) <- parsed, o <- os ]

  let roaFile = "/Users/mpuzanov/ripe/tmp//rpki/repo/ripe.net/repository/DEFAULT/5b/6ab497-16d9-4c09-94d9-1be4c416a09c/1/XY9rz4RATnJEwJxBpE7ExbD-Ubk.roa"  
  bs  <- B.readFile roaFile
  (_, roa) <- readObject roaFile bs

  putStrLn $ "roa blob = " ++ show bs

  let Right [rr@(RpkiObject (RpkiMeta { aki = Just (AKI ki)}) (RoaRO cms@(CMS so)))] = roa
  let CertificateWithSignature 
        _ 
        (SignatureAlgorithmIdentifier signAlgorithm) 
        (SignatureValue sign) encoded = scCertificate $ soContent so

  putStrLn $ "encoded = " ++ show encoded

  let skiMap = bySKIMap objects
  let [x@(RpkiObject _ (CerRO cer@(CerObject (ResourceCert parentCert))))] = MultiMap.lookup (SKI ki) skiMap

  let s = validateSignature rr cer

  putStrLn $ "x = " <> show x
  putStrLn $ "s = " <> show s

  let bss = [ B.take len $ B.drop b bs | b <- [1..B.length bs - 1], len <- [1..B.length bs - b]]

  let pubKey = certPubKey $ signedObject $ getSigned $ withRFC parentCert certX509              
  let checks = map (\b -> (b, verifySignature signAlgorithm pubKey b sign)) bss 

  putStrLn $ "valid signature = " <> show [ b | (b, SignaturePass) <- checks]

  putStrLn $ "validateCMSSignature = " ++ (show $ validateCMSSignature cms)

  
testCrl :: IO ()
testCrl = do
  
  bs  <- B.readFile "/Users/mpuzanov/ripe/tmp/rpki/repo/ripe.net/repository/ripe-ncc-ta.crl"
  -- bs  <- B.readFile "/Users/mpuzanov/ripe/tmp/rpki/repo/lacnic/repository/lacnic/0043f52c-eeee-4d62-97e6-44c83c8beb9f/3c4a28de1ccccd1e98eb95feb157d61c6659de78.crl"  
  -- putStrLn $ "asns = " <> show (decodeASN1' BER bs)
  
  parsed <- readRepo
  let objects = [ o | (_, Right os) <- parsed, o <- os ]  

  case parseCrl bs of
    Left e  -> putStrLn $ "x = " <> show e
    Right x -> do
      let (CrlMeta { aki = AKI ki }, CrlObject crl) = x $ URI "rsync://bla"
      putStrLn $ "aki = " <> show ki
      let SignCRL 
            _ 
            (SignatureAlgorithmIdentifier signAlgorithm) 
            (SignatureValue sign) 
            encoded  = crl

      let skiMap = bySKIMap objects
      let xx = MultiMap.lookup (SKI ki) skiMap
      putStrLn $ "xx = " <> show xx
      let [x@(RpkiObject _ (CerRO cer@(CerObject (ResourceCert parentCert))))] = xx

      let pubKey = certPubKey $ signedObject $ getSigned $ withRFC parentCert certX509              

      let bss = [ B.take len $ B.drop b bs | b <- [1..B.length bs - 1], len <- [1..B.length bs - b]]
      let checks = map (\b -> (b, verifySignature signAlgorithm pubKey b sign)) bss 
      putStrLn $ "valid signature = " <> show [ b | (b, SignaturePass) <- checks]

      putStrLn $ "verify = " <> (show $ verifySignature signAlgorithm pubKey encoded sign)
      


readObject :: String -> B.ByteString -> IO (String, Either (ParseError T.Text) [RpkiObject])
readObject fName content = do    
  let ext = L.drop (L.length fName - 3) fName
  let uri = URI $ T.pack fName
  let os = case ext of
        "cer" -> do
            f <- parseResourceCertificate content
            let (meta, o) = f uri
            pure [RpkiObject meta $ CerRO o]

        "mft" -> do
            f <- parseMft content             
            let (meta, o) = f uri
            pure [RpkiObject meta $ MftRO o]

        "roa" -> do
            f <- parseRoa content
            let (meta, o) = f uri
            pure [RpkiObject meta $ RoaRO o]

        "crl" -> do
            f <- parseCrl content
            let (meta, o) = f uri
            pure [RpkiCrl meta o]

        _     -> pure []

  pure (fName, os)

validateKeys :: [RpkiObject] -> Either String [SignatureVerification]
validateKeys objects = 
  case [ ro | ro@(RpkiObject (RpkiMeta { aki = Nothing }) _) <- objects ] of
    []      -> Left $ "No TA certificate"    
    taCerts -> Right $ concat [ validateChildren meta ro | RpkiObject meta ro <- taCerts ]
  where    
    validateChildren :: RpkiMeta -> RO -> [SignatureVerification]
    validateChildren meta (CerRO parentCert) = concat childrenVerification 
      where 
        childrenVerification = parMap strategy (validateChildParent parentCert) $ children meta
        strategy = parListChunk 1000 rseq
    validateChildren _ _ = []

    validateChildParent parentCert = \case 
      ro@(RpkiObject _ (MftRO cms@(CMS _))) -> eeSignAndCMSSign ro cms 
      ro@(RpkiObject _ (RoaRO cms@(CMS _))) -> eeSignAndCMSSign ro cms
      ro@(RpkiObject _ (GbrRO cms@(CMS _))) -> eeSignAndCMSSign ro cms
      ro@(RpkiObject m cer@(CerRO _))       -> 
        [validateSignature ro parentCert] <> validateChildren m cer      
      where
        eeSignAndCMSSign  :: RpkiObject -> CMS a -> [SignatureVerification]
        eeSignAndCMSSign ro cms = [ validateCMSSignature cms, validateSignature ro parentCert]            

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
