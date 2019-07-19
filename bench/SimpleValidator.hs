{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Concurrent.Async
import Control.Monad

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.MultiMap as MultiMap
import Data.MultiMap (MultiMap)

import Data.Bifunctor
import Data.Hex

import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding

import Data.X509
import Data.X509.Validation

import RPKI.Domain hiding (SHA256)
import RPKI.SignTypes
import RPKI.Validate
import RPKI.Parse.Common
import RPKI.Parse.Cert
import RPKI.Parse.MFT
import RPKI.Parse.ROA

import Streaming
import qualified Streaming.Prelude as S

-- import Streamly
-- import qualified Streamly.Prelude as SLY
-- import Data.Function ((&))

import System.FilePath.Find

-- import Crypto.Store.X509
-- import Crypto.Store.CMS
-- import Crypto.Store.ASN1.Parse

import Crypto.Hash
import Crypto.Random.Types
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import qualified Crypto.PubKey.RSA.Prim as RSAPrim
import Crypto.PubKey.RSA.Types 
import qualified Data.ByteArray as BA

-- import Crypto.Store.PEM


runValidate = do  
  parsedObjects <- readRepo              

  let errors = [ e | Left e <- parsedObjects ]  
  let objects = [ o | Right os <- parsedObjects, o <- os ]

  let akis = Set.fromList [ hex a | Right ros <- parsedObjects, 
                              RpkiObject (RpkiMeta { aki = Just (AKI (KI a)) }) _ <- ros ]

  let skis = Set.fromList [ hex a | Right ros <- parsedObjects, 
                              RpkiObject (RpkiMeta { ski = SKI (KI a) }) _ <- ros ]

  -- putStrLn $ "skis = " ++ show (length skis)
  -- forM_ skis $ \ski -> putStrLn $ show ski

  -- putStrLn $ "akis = " ++ show (length akis)
  -- forM_ akis $ \aki -> putStrLn $ show aki

  -- putStrLn $ "errors = " ++ show (length errors)
  putStrLn $ "objects = " ++ show (length objects)  
  let Right signatureValidations = validateKeys objects
  putStrLn $ "valid signature = " ++ show (length [s | s@SignaturePass <- signatureValidations])
  putStrLn $ "invalid signature = " ++ show (length [s | s@(SignatureFailed SignatureInvalid) <- signatureValidations])  
  putStrLn $ "invalid pubkey mismatch = " ++ show (length [s | s@(SignatureFailed SignaturePubkeyMismatch) <- signatureValidations])  
  putStrLn $ "invalid not implemented = " ++ show (length [s | s@(SignatureFailed SignatureUnimplemented) <- signatureValidations])  
  

readRepo :: IO [Either (ParseError T.Text) [RpkiObject]]
readRepo = do
  let repository = "/Users/mpuzanov/ripe/tmp/rpki/repo"  
  let expr = (fileName ~~? "*.cer") ||? 
             (fileName ~~? "*.roa") ||? 
             (fileName ~~? "*.mft") -- add (fileName ~~? "*.crl")
  fileNames <- find always expr repository 
  -- SLY.toList $ SLY.fromFoldable fileNames 
  --            |& SLY.mapM (\f -> B.readFile f >>= readObject f)             

  mapConcurrently (\f -> B.readFile f >>= readObject f) fileNames

unpad' :: BA.ByteArray bytearray => bytearray -> Either Error bytearray
unpad' packed
    | paddingSuccess = Right m
    | otherwise      = Left MessageNotRecognized
  where
        (zt, ps0m)   = BA.splitAt 2 packed
        (ps, zm)     = BA.span (/= 0) ps0m
        (z, m)       = BA.splitAt 1 zm
        paddingSuccess = ((zt `BA.constEq` (BA.pack [0,2] :: BA.Bytes)) || (zt `BA.constEq` (BA.pack [0,1] :: BA.Bytes))) &&
                           (z == BA.zero 1) && (BA.length ps >= 8)


testSignature = do
  let roaFile = "/Users/mpuzanov/ripe/tmp/rpki/repo/repository/DEFAULT/5b/6ab497-16d9-4c09-94d9-1be4c416a09c/1/XY9rz4RATnJEwJxBpE7ExbD-Ubk.roa"  
  bs  <- B.readFile roaFile
  roa <- readObject roaFile bs
  let len = B.length bs
  let Right [RpkiObject _ (RoaRO (CMS so))] = roa
  let CertificateWithSignature 
        eeCertificate 
        (SignatureAlgorithmIdentifier signAlgorithm) 
        _ = scCertificate $ soContent so

  let SignatureValue sign = signature $ scSignerInfos $ soContent so

  let pubKey@(PubKeyRSA pk) = certPubKey eeCertificate

  --  java                  haskell
  -- signer.resultDigest == signedAttributes.MessageDigest
  -- 

  let digestEncoded = "010\r\ACK\t`\134H\SOHe\ETX\EOT\STX\SOH\ENQ\NUL\EOT \202\&2\213\195\143N\v}\158\203d\SI\227\246\136\244\ACK\164Sg>\ACKR\128\213R\198\249\194\&24l"
  let digestOriginal = "\202\&2\213\195\143N\v}\158\203d\SI\227\246\136\244\ACK\164Sg>\ACKR\128\213R\198\249\194\&24l"

  let rr = RSA.verify (Nothing :: Maybe SHA256) pk digestEncoded sign


  ---- WORKS
  let rr1 = RSA.verify (Nothing :: Maybe SHA256) pk digestOriginal sign
  ---- WORKS!!!!!

  let v = verifySignature signAlgorithm pubKey digestEncoded sign
  let v1 = verifySignature signAlgorithm pubKey digestOriginal sign

  let sb :: B.ByteString = RSAPrim.ep pk digestEncoded
  let ss :: B.ByteString = RSAPrim.ep pk sign

  putStrLn $ "eeCertificate = " ++ show eeCertificate
  putStrLn $ "so = " ++ show so
  putStrLn $ "v = " ++ show v
  putStrLn $ "v1 = " ++ show v1
  putStrLn $ "pk = " ++ show pk
  putStrLn $ "sb = " ++ show (hex sb)
  putStrLn $ "ss = " ++ show (hex ss)
  putStrLn $ "unpad ss = " ++ show (hex <$> unpad' ss)
  putStrLn $ "sign = " ++ show (hex sign)  
  putStrLn $ "rr = " ++ show rr
  putStrLn $ "rr1 = " ++ show rr1
  

readObject :: String -> B.ByteString -> IO (Either (ParseError T.Text) [RpkiObject])
readObject fileName content = do    
  let ext = L.drop (L.length fileName - 3) fileName
  let uri = URI $ T.pack fileName
  pure $ case ext of
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
    validateChildren meta (CerRO cert) = signatureChecks ++ childSignatureChecks
      where
        childSignatureChecks = L.concat [ validateChildren m cer | RpkiObject m cer@(CerRO c) <- children meta ]      
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

-- parseCMS :: B.ByteString -> Maybe ContentInfo
-- parseCMS bs = pemToContentInfo pem
--   where pem = PEM { pemName = "CMS", pemHeader = [], pemContent = bs}
  

main :: IO ()
main = do 
  -- runValidate
  testSignature
