{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

import Colog

import Control.Monad
import Control.Parallel.Strategies hiding ((.|))
import Control.Concurrent.Async
import Control.DeepSeq (($!!))
import Control.Exception
import Control.Monad.Reader

import qualified Codec.Serialise as Serialise

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.MultiMap as MultiMap
import Data.MultiMap (MultiMap)

import Data.X509
import Data.X509.Validation

import RPKI.AppMonad
import RPKI.Domain
import RPKI.SignTypes
import RPKI.Validate
import RPKI.Parse.Parse

import System.FilePath.Find

import Data.Time.Clock (getCurrentTime)

import Lmdb.Connection
import qualified Lmdb.Map as LmdbMap
import Lmdb.Types
import Lmdb.Codec

import RPKI.RRDP.Types
import RPKI.RRDP.Parse
import RPKI.RRDP.Update

import RPKI.Rsync

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
import System.IO (withFile, IOMode(..), hClose)
import System.Directory (removeFile)
import System.IO.Temp (withSystemTempFile)
import           Data.Hex                   (hex, unhex)

import qualified UnliftIO.Async as Unlift

import qualified RPKI.Store.Base.LMDB as LMDB
import RPKI.Store.Base.Map
import           RPKI.Store.Stores
import RPKI.Logging


mkLmdb :: IO (Environment 'ReadWrite)
mkLmdb = initializeReadWriteEnvironment mapSize readerNum maxDatabases "./data"
  where 
    mapSize = 8*1000*1000*1000
    readerNum = 120
    maxDatabases = 120    

makeLmdbMap :: ModeBool e => Environment e -> IO (Database Hash B.ByteString)
makeLmdbMap env = 
    withTransaction env $ \tx -> openDatabase tx (Just "objects") dbSettings 
    where
      dbSettings = makeSettings (SortNative NativeSortLexographic) hashCodec valCodec      
      hashCodec = Codec encodeHash (Decoding decodeHash)
      encodeHash = encodeThroughByteString (\(Hash bs) -> bs)
      decodeHash c p = Hash <$> decodeByteString c p
      valCodec = byteString



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

-- saveSerialised :: Environment ReadWrite -> IO ()
-- saveSerialised lmdb = do
--   say "start"
--   fileNames <- getFileNames
--   -- say $ "found" <> show fileNames

--   (chanIn, chanOut) <- Chan.newChan 20
--   void $ concurrently
--       (write_ fileNames chanIn)
--       (withTransaction lmdb $ read_ db fileNames chanOut)  

--   say $ "re-read and validate"
--   objects <- readAll db
--   say $ "objects number = " <> show (length objects)

--   -- validateObjects objects
--   say $ "done"

--   where 
--     write_ fileNames chanIn =
--       S.effects $
--       S.mapM (Chan.writeChan chanIn) $
--       S.mapM (\f -> async $ do
--         bs <- B.readFile f
--         pure (readObject f bs, bs)) $
--       S.each fileNames

--     read_ db fileNames chanOut = 
--       S.effects $ 
--       S.mapM (saveToLmdb db) $ 
--       S.concat $ 
--       S.map fst $
--       S.mapM (liftIO . wait) $
--       S.mapM (\_ -> liftIO $ Chan.readChan chanOut) $
--       S.each fileNames

--     saveToLmdb :: Database Hash RpkiObject -> RpkiObject -> IO ()
--     saveToLmdb db = \case 
--        ro@(RpkiObject RpkiMeta {..} _) -> do
--         -- liftIO $ say $ "hash = " <> show hash
--         put db hash (Just ro)
--        rc@(RpkiCrl CrlMeta {..} _)     -> do
--         -- liftIO $ say $ "hash = " <> show hash
--         put db hash (Just rc) 

--     readAll :: Database Hash RpkiObject -> IO [RpkiObject]
--     readAll db = readOnlyTransaction lmdb $ do
--         ks <- keys db 
--         S.toList_ $ 
--           S.concat $ 
--           S.mapM (get db) $ 
--           S.each ks      
        

-- saveOriginal :: Environment ReadWrite -> IO ()
-- saveOriginal lmdb = do
--   say "start"
--   fileNames <- getFileNames
--   say "found"

--   db :: Database Hash (String, B.ByteString) <- readWriteTransaction lmdb $ getDatabase (Just "originals")  

--   (chanIn, chanOut) <- Chan.newChan 20
--   void $ concurrently
--       (write_ fileNames chanIn)
--       (readWriteTransaction lmdb $ read_ db fileNames chanOut)  

--   say $ "re-read and validate"
--   objects <- readAll db
--   validateObjects objects
--   say $ "done"

--   where 
--     write_ fileNames chanIn =
--       S.effects $
--       S.mapM (Chan.writeChan chanIn) $
--       S.mapM (\f -> async $ do 
--                       bs <- B.readFile f
--                       pure (f, bs)) $
--       S.each fileNames

--     read_ db fileNames chanOut = 
--       S.effects $ 
--       S.mapM (\(f, bs) -> put db (sha256s bs) (Just (f, bs))) $       
--       S.mapM (liftIO . wait) $
--       S.mapM (\_ -> liftIO $ Chan.readChan chanOut) $
--       S.each fileNames
     
--     readAll :: Database Hash (String, B.ByteString) -> IO [RpkiObject]
--     readAll db = readOnlyTransaction lmdb $ do
--         ks <- keys db
--         S.toList_ $ 
--           S.concat $
--           S.map (\(f, bs) -> readObject f bs) $
--           S.concat $ 
--           S.mapM (get db) $ 
--           S.each ks      
          


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
      let SignCRL {
            signatureAlgorithm = (SignatureAlgorithmIdentifier signAlgorithm),
            signatureValue = (SignatureValue sign), 
            encodedValue = encoded 
          } = crl

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
  case [ ro | ro@(RpkiObject RpkiMeta { aki = Nothing } _) <- objects ] of
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

parseWithTmp :: FilePath -> (BL.ByteString -> IO a) -> IO a
parseWithTmp filename f = --BL.readFile filename >>= f
  withSystemTempFile "test_mmap_" $ \name h -> do
  --   -- BL.readFile filename >>= BL.writeFile name
    runResourceT $ Q.toHandle h $ Q.readFile filename
    hClose h
    f =<< unsafeMMapFile name   
  --   -- f =<< BL.hGetContents h   

-- RRDP
validatorUpdateRRDPRepo :: Environment 'ReadWrite -> IO ()
validatorUpdateRRDPRepo lmdb = do
  say "begin"

  m  <- makeLmdbMap lmdb

  -- runResourceT $ runExceptT $ streamHttpResponse 
  --   "https://rrdp.ripe.net/670de3e5-de83-4d77-bede-9d8a78454151/1585/snapshot.xml" 
  --   (\s -> Q.writeFile "snapshot.xml")  
  --   show
  
  parseWithTmp "snapshot.xml" $ \xml -> fileToDb m xml  
    where
      fileToDb m xml =
        -- say $ "xml = " <> show xml
        case parseSnapshot xml of
          Left e -> putStrLn $ "error = " <> show e
          Right (Snapshot _ _ _ ps) -> do
            (chanIn, chanOut) <- Chan.newChan 20
            x <- first (\(e :: SomeException) -> e) <$> (try $ Unlift.concurrently 
              (write_ chanIn ps) 
              (withTransaction lmdb $ \tx -> read_ chanOut tx m ps))
            case x of 
              Left e -> say $ "error = " <> show e
              Right _ -> pure ()

      mkObject u b64 = do
        DecodedBase64 b <- first RrdpE $ decodeBase64 b64 u
        first ParseE $ readObject (T.unpack u) b

      write_ chanIn ps = 
        forM_ ps $ \(SnapshotPublish (URI u) encodedb64) -> do 
          a <- async $ pure $!! (u,) . toBytes_ <$> mkObject u encodedb64          
          Chan.writeChan chanIn (u, a)

      read_ chanOut tx m x = forM x $ \_ -> do
        (u, a) <- Chan.readChan chanOut
        wait a >>= \case
          Left e             -> putStrLn $ "error = " <> show e <> ", u = " <> show u
          Right (_, (h, bs)) -> 
            LmdbMap.insertSuccess' tx m h bs >>= \case 
              True -> pure ()
              False -> LmdbMap.repsert' tx m h bs

      toBytes_ o@(RpkiObject RpkiMeta {..} _) = (hash, BL.toStrict $ Serialise.serialise o)
      toBytes_ c@(RpkiCrl CrlMeta {..} _)     = (hash, BL.toStrict $ Serialise.serialise c)
    
      -- readAll :: Database Hash RpkiObject -> IO [RpkiObject]
      -- readAll db = withReadOnlyTransaction lmdb $ do
      --     ks <- keys db
      --     S.toList_ $ 
      --       S.concat $ 
      --       S.mapM (get db) $ 
      --       S.each ks                  

  -- say $ "hash = " <> show hash

processRRDP :: Environment 'ReadWrite -> IO ()
processRRDP env = do
    say "begin"  
    lmdbStorage <- LMDB.create env "objects"
    let repo = RrdpRepository (URI "https://rrdp.ripe.net/notification.xml") Nothing
    let conf = (AppLogger logTextStdout)
    let store = RpkiObjectStore (SIxMap lmdbStorage [])
    e <- runValidatorT conf $ processRrdp repo store
    say $ "resulve " <> show e
  
saveRsync env = do
    say "begin"  
    lmdbStorage <- LMDB.create env "objects"
    let repo = RsyncRepository (URI "rsync://rpki.afrinic.net/repository/afrinic")
    let conf = (AppLogger logTextStdout, RsyncConf "/tmp/rsync")
    -- e <- (`runReaderT` conf) $ processRsync repo store 

    e <- runValidatorT conf $ rsyncFile (URI "rsync://rpki.ripe.net/ta/ripe-ncc-ta.cer") (const Nothing)
    say $ "done " <> show e

main :: IO ()
main = do 
  -- testCrl
  -- runValidate  
  -- mkLmdb >>= saveSerialised
  -- mkLmdb >>= saveOriginal
  -- usingLoggerT (LogAction putStrLn) $ lift app
  mkLmdb >>= validatorUpdateRRDPRepo
  -- testSignature

say :: String -> IO ()
say s = do
  let !ss = s
  t <- getCurrentTime
  putStrLn $ show t <> ": " <> ss