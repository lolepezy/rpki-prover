{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Control.Parallel.Strategies hiding ((.|))
import Control.Concurrent.Async
import Control.DeepSeq (($!!))
import Control.Exception
import qualified Crypto.Hash.SHA256      as S256

import qualified Codec.Serialise as Serialise

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.List as L
import qualified Data.Text.Encoding as TE
import qualified Data.MultiMap as MultiMap
import Data.MultiMap (MultiMap)

import Data.X509
import Data.X509.Validation

import RPKI.Domain
import RPKI.SignTypes
import RPKI.Validate
import RPKI.Parse.Parse

import System.FilePath.Find

import RPKI.Util (parallel, sha256s, sinkHash)

import Data.Time.Clock (getCurrentTime)

import Database.LMDB.Simple
import Database.LMDB.Simple.Extra

import RPKI.RRDP.Types
import RPKI.RRDP.Parse
import RPKI.RRDP.Update

import Streaming
import qualified Streaming.Prelude as S
import qualified Data.ByteString.Streaming as Q
import Data.ByteString.Streaming.HTTP
import Data.IORef

import Conduit
import qualified Data.Conduit.List      as CL
import Data.Conduit
import Network.HTTP.Simple (httpSink)

import qualified Text.XML.Stream.Parse as XC
import Data.XML.Types (Event)
import Text.XML
import qualified Text.XML.Expat.SAX as X
import qualified Data.ByteString.Base64               as B64

import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan

import System.IO.Posix.MMap.Lazy (unsafeMMapFile)
import System.IO (withFile, IOMode(..))
import System.Directory (removeFile)
import System.IO.Temp (withSystemTempFile)
import           Data.Hex                   (hex, unhex)

import qualified UnliftIO.Async as Unlift

mkLmdb :: IO (Environment ReadWrite)
mkLmdb = openReadWriteEnvironment "./data" limits 
  where 
    limits = defaultLimits {
      mapSize = 8*1000*1000*1000,
      maxDatabases = 120
    }

runValidate :: IO ()
runValidate = do
  say "starting"
  parsedObjects <- readRepo              

  let errors = [ (f, e) | (f, Left e) <- parsedObjects ]  
  
  say $ "errors = " <> show (length errors)
  let objects = [ o | (_, Right o) <- parsedObjects ]

  say $ "objects = " <> show (length objects)

  let Right signatureValidations = validateKeys objects
  say $ "valid signature = " <> show (length [s | s@SignaturePass <- signatureValidations])
  say $ "invalid signature = " <> show (length [s | s@(SignatureFailed SignatureInvalid) <- signatureValidations])  
  say $ "invalid pubkey mismatch = " <> show (length [s | s@(SignatureFailed SignaturePubkeyMismatch) <- signatureValidations])  
  say $ "invalid not implemented = " <> show (length [s | s@(SignatureFailed SignatureUnimplemented) <- signatureValidations])  

  say "done"
  
validateObjects :: [RpkiObject] -> IO ()
validateObjects objects = do
  let Right signatureValidations = validateKeys objects
  say $ "valid signature = " <> show (length [s | s@SignaturePass <- signatureValidations])
  say $ "invalid signature = " <> show (length [s | s@(SignatureFailed SignatureInvalid) <- signatureValidations])  
  say $ "invalid pubkey mismatch = " <> show (length [s | s@(SignatureFailed SignaturePubkeyMismatch) <- signatureValidations])  
  say $ "invalid not implemented = " <> show (length [s | s@(SignatureFailed SignatureUnimplemented) <- signatureValidations])  

type ReadStuff = (String, Either (ParseError T.Text) [RpkiObject])

readRepo :: IO [(String, ParseResult RpkiObject)]
readRepo = do
  fileNames <- getFileNames
  forM fileNames $ \f -> do
    bs <- B.readFile f
    pure (f, readObject f bs)

getFileNames :: IO [String]
getFileNames = do
  let repository = "/Users/mpuzanov/ripe/tmp/rpki/repo/"  
  let expr = (fileName ~~? "*.cer") ||? 
              (fileName ~~? "*.roa") ||?
              (fileName ~~? "*.mft") ||?            
              (fileName ~~? "*.crl") 
  find always expr repository 

saveSerialised :: Environment ReadWrite -> IO ()
saveSerialised lmdb = do
  say "start"
  fileNames <- getFileNames
  say "found"

  db :: Database Hash RpkiObject <- readWriteTransaction lmdb $ getDatabase (Just "objects")  

  (chanIn, chanOut) <- Chan.newChan 20
  void $ concurrently
      (write_ fileNames chanIn)
      (readWriteTransaction lmdb $ read_ db fileNames chanOut)  

  say $ "re-read and validate"
  objects <- readAll db
  validateObjects objects
  say $ "done"

  where 
    write_ fileNames chanIn =
      S.effects $
      S.mapM (Chan.writeChan chanIn) $
      S.mapM (\f -> async $ do
        bs <- B.readFile f
        pure (readObject f bs, bs)) $
      S.each fileNames

    read_ db fileNames chanOut = 
      S.effects $ 
      S.mapM (saveToLmdb db) $ 
      S.concat $ 
      S.map fst $
      S.mapM (liftIO . wait) $
      S.mapM (\_ -> liftIO $ Chan.readChan chanOut) $
      S.each fileNames

    saveToLmdb :: Database Hash RpkiObject -> RpkiObject -> Transaction ReadWrite ()
    saveToLmdb db = \case 
       ro@(RpkiObject RpkiMeta {..} _) -> do
        put db hash (Just ro)
       rc@(RpkiCrl CrlMeta {..} _) -> do
        put db hash (Just rc) 

    readAll :: Database Hash RpkiObject -> IO [RpkiObject]
    readAll db = readOnlyTransaction lmdb $ do
        ks <- keys db 
        S.toList_ $ 
          S.concat $ 
          S.mapM (get db) $ 
          S.each ks      
        

saveOriginal :: Environment ReadWrite -> IO ()
saveOriginal lmdb = do
  say "start"
  fileNames <- getFileNames
  say "found"

  db :: Database Hash (String, B.ByteString) <- readWriteTransaction lmdb $ getDatabase (Just "originals")  

  (chanIn, chanOut) <- Chan.newChan 20
  void $ concurrently
      (write_ fileNames chanIn)
      (readWriteTransaction lmdb $ read_ db fileNames chanOut)  

  say $ "re-read and validate"
  objects <- readAll db
  validateObjects objects
  say $ "done"

  where 
    write_ fileNames chanIn =
      S.effects $
      S.mapM (Chan.writeChan chanIn) $
      S.mapM (\f -> async $ do 
                      bs <- B.readFile f
                      pure (f, bs)) $
      S.each fileNames

    read_ db fileNames chanOut = 
      S.effects $ 
      S.mapM (\(f, bs) -> put db (sha256s bs) (Just (f, bs))) $       
      S.mapM (liftIO . wait) $
      S.mapM (\_ -> liftIO $ Chan.readChan chanOut) $
      S.each fileNames
     
    readAll :: Database Hash (String, B.ByteString) -> IO [RpkiObject]
    readAll db = readOnlyTransaction lmdb $ do
        ks <- keys db
        S.toList_ $ 
          S.concat $
          S.map (\(f, bs) -> readObject f bs) $
          S.concat $ 
          S.mapM (get db) $ 
          S.each ks      
          


testSignature :: IO ()
testSignature = do
  parsed <- readRepo
  let objects = [ o | (_, Right o) <- parsed ]

  let roaFile = "/Users/mpuzanov/ripe/tmp//rpki/repo/ripe.net/repository/DEFAULT/5b/6ab497-16d9-4c09-94d9-1be4c416a09c/1/XY9rz4RATnJEwJxBpE7ExbD-Ubk.roa"  
  bs  <- B.readFile roaFile
  let Right roa = readObject roaFile bs

  putStrLn $ "roa blob = " ++ show bs

  let rr@(RpkiObject (RpkiMeta { aki = Just (AKI ki)}) (RoaRO cms@(CMS so))) = roa
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
  let objects = [ o | (_, Right o) <- parsed ]  

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
bySKIMap ros = MultiMap.fromList [ (ski, ro) | ro@(RpkiObject RpkiMeta {..} _) <- ros ]

byAKIMap :: [RpkiObject] -> MultiMap AKI RpkiObject
byAKIMap ros = MultiMap.fromList [ (a, ro) | ro@(RpkiObject RpkiMeta { aki = Just a } _) <- ros ]


streamHttpResponse (URI uri) handler err = 
  lift (try go) >>= \case
    Left (e :: SomeException) -> throwE $ err e
    Right r                   -> except r
  where 
    go = do
      req     <- parseRequest $ T.unpack uri
      tls     <- newManager tlsManagerSettings 
      hashRef <- newIORef S256.init
      s <- withHTTP req tls $ \resp -> 
        handler $ 
        Q.fromChunks $ 
        S.mapM (\b -> modifyIORef hashRef (`S256.update` b) >> pure b) $ 
        Q.toChunks $ responseBody resp
      h <- readIORef hashRef
      pure $ (,Hash $ S256.finalize h) <$> s

parseWithTmp filename f = 
  withSystemTempFile "test_mmap_" $ \name h -> do
    runResourceT $ Q.hPut h $ Q.readFile filename
    f =<< unsafeMMapFile name   

-- RRDP
updateRRDPRepo :: Environment ReadWrite -> IO ()
updateRRDPRepo lmdb = do
  say "begin"

  db :: Database Hash B.ByteString <- readWriteTransaction lmdb $ getDatabase (Just "objects")  

  -- runResourceT $ runExceptT $ streamHttpResponse 
  --   "https://rrdp.ripe.net/670de3e5-de83-4d77-bede-9d8a78454151/1585/snapshot.xml" 
  --   (\s -> Q.writeFile "snapshot.xml")  
  --   show
  
  parseWithTmp "snapshot1.xml" $ \xml -> fileToDb db xml  

    where
      fileToDb db xml =
        case parseSnapshot xml of
          Left e -> putStrLn $ "error = " <> show e
          Right (Snapshot _ _ _ ps) -> do
            (chanIn, chanOut) <- Chan.newChan 20
            void $ Unlift.concurrently 
              (write_ chanIn ps) 
              (readWriteTransaction lmdb $ read_ chanOut db ps)    


      mkObject u b64 = do
        DecodedBase64 b <- first RrdpE$ decodeBase64 b64
        first ParseE $ readObject (T.unpack u) b

      write_ chanIn ps = 
        forM_ ps $ \(SnapshotPublish (URI u) encodedb64) -> do 
          a <- async $ pure $!! (u,) . toBytes_ <$> mkObject u encodedb64
          Chan.writeChan chanIn a

      read_ chanOut db x = forM x $ \_ -> 
        liftIO (Chan.readChan chanOut >>= wait) >>= \case
          Left e             -> abort >> liftIO (putStrLn $ "error = " <> show e)
          Right (_, (h, bs)) -> put db h (Just bs)

      toBytes_ o@(RpkiObject RpkiMeta {..} _) = (hash, BL.toStrict $ Serialise.serialise o)
      toBytes_ c@(RpkiCrl CrlMeta {..} _)     = (hash, BL.toStrict $ Serialise.serialise c)               
    
      readAll :: Database Hash RpkiObject -> IO [RpkiObject]
      readAll db = readOnlyTransaction lmdb $ do
          ks <- keys db
          S.toList_ $ 
            S.concat $ 
            S.mapM (get db) $ 
            S.each ks                  

  -- say $ "hash = " <> show hash

main :: IO ()
main = do 
  -- testCrl
  -- runValidate  
  -- mkLmdb >>= saveSerialised
  -- mkLmdb >>= saveOriginal
  mkLmdb >>= updateRRDPRepo
  -- testSignature

say :: String -> IO ()
say s = do
  let !ss = s
  t <- getCurrentTime
  putStrLn $ show t <> ": " <> ss