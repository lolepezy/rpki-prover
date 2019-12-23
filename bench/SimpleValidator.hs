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

import qualified Codec.Serialise as Serialise

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.MultiMap as MultiMap
import Data.MultiMap (MultiMap)

import Data.Maybe

import Data.X509
import Data.X509.Validation

import RPKI.AppMonad
import RPKI.Domain
import RPKI.Errors
import RPKI.Validation.Crypto
import RPKI.Parse.Parse

import System.FilePath.Find

import Data.Time.Clock (getCurrentTime)

import Lmdb.Connection
import qualified Lmdb.Map as LmdbMap
import Lmdb.Types
import Lmdb.Codec

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

import           RPKI.Core
import           RPKI.Logging
import           RPKI.Config
import           RPKI.RRDP.Parse
import           RPKI.RRDP.Types
import           RPKI.RRDP.Update
import qualified RPKI.Store.Base.LMDB as LMDB
import           RPKI.Store.Base.Map
import           RPKI.Store.Base.MultiMap
import           RPKI.Store.Stores
import           RPKI.TAL
import           RPKI.Rsync
import           RPKI.Validation.Objects
import qualified RPKI.Util  as U
import RPKI.Store.Util


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

  let rr@(RoaRO (FullMeta (RpkiMeta { aki = Just (AKI ki)}) _, cms@(CMS so))) = roa
  let CertificateWithSignature 
        _ 
        (SignatureAlgorithmIdentifier signAlgorithm) 
        (SignatureValue sign) encoded = getEECert so

  putStrLn $ "encoded = " ++ show encoded

  let skiMap = bySKIMap objects
  let [x@(CerRO cer@(_, ResourceCertificate parentCert))] = MultiMap.lookup (SKI ki) skiMap

  let s = validateSignature rr cer

  putStrLn $ "x = " <> show x
  putStrLn $ "s = " <> show s

  let bss = [ B.take len $ B.drop b bs | b <- [1..B.length bs - 1], len <- [1..B.length bs - b]]

  let pubKey = certPubKey $ cwsX509certificate $ withRFC parentCert certX509              
  let checks = map (\b -> (b, verifySignature signAlgorithm pubKey b sign)) bss 

  putStrLn $ "valid signature = " <> show [ b | (b, SignaturePass) <- checks]

  putStrLn $ "validateCMSSignature = " ++ (show $ validateCMSSignature cms)

  
-- testCrl :: IO ()
-- testCrl = do
  
--   bs  <- B.readFile "/Users/mpuzanov/ripe/tmp/rpki/repo/ripe.net/repository/ripe-ncc-ta.crl"
--   -- bs  <- B.readFile "/Users/mpuzanov/ripe/tmp/rpki/repo/lacnic/repository/lacnic/0043f52c-eeee-4d62-97e6-44c83c8beb9f/3c4a28de1ccccd1e98eb95feb157d61c6659de78.crl"  
--   -- putStrLn $ "asns = " <> show (decodeASN1' BER bs)
  
--   parsed <- readRepo
--   let objects = [ o | (_, Right o) <- parsed ]  

--   case parseCrl bs of
--     Left e  -> putStrLn $ "x = " <> show e
--     Right x -> do
--       let (CrlMeta { aki = AKI ki }, crl) = x $ URI "rsync://bla"
--       putStrLn $ "aki = " <> show ki
--       let SignCRL {
--             signatureAlgorithm = (SignatureAlgorithmIdentifier signAlgorithm),
--             signatureValue = (SignatureValue sign), 
--             encodedValue = encoded 
--           } = crl

--       let skiMap = bySKIMap objects
--       let xx = MultiMap.lookup (SKI ki) skiMap
--       putStrLn $ "xx = " <> show xx
--       let [x@(RpkiObject _ (CerRO cer@(ResourceCertificate parentCert)))] = xx

--       let pubKey = certPubKey $ getX509Cert $ withRFC parentCert certX509              

--       let bss = [ B.take len $ B.drop b bs | b <- [1..B.length bs - 1], len <- [1..B.length bs - b]]
--       let checks = map (\b -> (b, verifySignature signAlgorithm pubKey b sign)) bss 
--       putStrLn $ "valid signature = " <> show [ b | (b, SignaturePass) <- checks]

--       putStrLn $ "verify = " <> (show $ verifySignature signAlgorithm pubKey encoded sign)
      

validateKeys :: [RpkiObject] -> Either String [SignatureVerification]
validateKeys objects = 
  case [ ro | ro <- objects, isNothing $ aki $ getMeta ro ] of
    []      -> Left $ "No TA certificate"    
    taCerts -> Right $ concat [ validateChildren ro | ro <- taCerts ]
  where    
    validateChildren :: RpkiObject -> [SignatureVerification]
    validateChildren (CerRO parentCert) = concat childrenVerification 
      where 
        childrenVerification = parMap strategy 
            (validateChildParent parentCert) $ 
            children parentCert
        strategy = parListChunk 1000 rseq
    validateChildren _ = []

    validateChildParent parentCert = \case 
      ro@(MftRO (_, cms@(CMS _))) -> eeSignAndCMSSign ro cms 
      ro@(RoaRO (_, cms@(CMS _))) -> eeSignAndCMSSign ro cms
      ro@(GbrRO (_, cms@(CMS _))) -> eeSignAndCMSSign ro cms
      ro@(CerRO _) -> [validateSignature ro parentCert] <> validateChildren ro
      where
        eeSignAndCMSSign  :: RpkiObject -> CMS a -> [SignatureVerification]
        eeSignAndCMSSign ro cms = [ validateCMSSignature cms, validateSignature ro parentCert ]            

    children a = MultiMap.lookup (AKI ki) akiMap
      where
        SKI ki = getSKI a

    akiMap = byAKIMap objects
    

bySKIMap :: [RpkiObject] -> MultiMap SKI RpkiObject
bySKIMap ros = MultiMap.fromList [ (ski, ro) | (Just ski, ro) <- map f ros ]
  where
    f a@(CerRO c) = (Just $ getSKI c, a)
    f a@(MftRO c) = (Just $ getSKI c, a)
    f a@(RoaRO c) = (Just $ getSKI c, a)
    f a@(GbrRO c) = (Just $ getSKI c, a)
    f c@(CrlRO _) = (Nothing, c)


byAKIMap :: WithMeta a => [a] -> MultiMap AKI a
byAKIMap ros = MultiMap.fromList [ (a, ro) | ro <- ros, a <- maybeToList (aki (getMeta ro)) ]

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
  counter <- newIORef (0 :: Int)
  parseWithTmp "snapshot.xml" $ \xml -> fileToDb m xml counter
  c <- readIORef counter
  say $ "saved " <> show c <> " objects."
    where
      fileToDb m xml counter =
        -- say $ "xml = " <> show xml
        case parseSnapshot xml of
          Left e -> putStrLn $ "error = " <> show e
          Right (Snapshot _ _ _ ps) -> do
            (chanIn, chanOut) <- Chan.newChan 20
            x <- first (\(e :: SomeException) -> e) <$> (try $ Unlift.concurrently 
              (write_ chanIn ps) 
              (withTransaction lmdb $ \tx -> read_ chanOut counter tx m ps))
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

      read_ chanOut counter tx m x = forM x $ \_ -> do
        (u, a) <- Chan.readChan chanOut
        wait a >>= \case
          Left e             -> putStrLn $ "error = " <> show e <> ", u = " <> show u
          Right (_, (h, bs)) -> do
            void $ atomicModifyIORef counter $ \c -> (c + 1, ())
            LmdbMap.insertSuccess' tx m h bs >>= \case 
              True -> pure ()
              False -> LmdbMap.repsert' tx m h bs

      toBytes_ o = (hash (getMeta o), BL.toStrict $ Serialise.serialise o)      
    
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
    let repo = RrdpRepository (URI "https://rrdp.ripe.net/notification.xml") Nothing    
    let conf = (createLogger, Config getParallelism, vContext $ URI "something.cer")
    store <- createObjectStore env
    e <- runValidatorT conf $ processRrdp repo store
    say $ "resulve " <> show e
  
saveRsyncRepo env = do  
  let repo = RsyncRepository (URI "rsync://rpki.ripe.net/repository")    
  let conf = (createLogger, RsyncConf "/tmp/rsync", Config getParallelism, vContext $ URI "something.cer")
  store <- createObjectStore env
  runValidatorT conf $ processRsync repo store

saveRsync env = do
    say "begin"  
    let logAction = logTextStdout
    let conf = (createLogger, RsyncConf "/tmp/rsync", vContext $ URI "something.cer")
    e <- runValidatorT conf $ rsyncFile (URI "rsync://rpki.ripe.net/ta/ripe-ncc-ta.cer") (pure . id)
    say $ "done " <> show e

processTAL = do
  say "begin"  
  let conf = (createLogger, RsyncConf "/tmp/rsync", vContext $ URI "something.cer")
  result <- runValidatorT conf $ do
    t <- fromTry (RsyncE . FileReadError . U.fmtEx) $ 
      B.readFile "/Users/mpuzanov/Projects/rpki-validator-3/rpki-validator/src/main/resources/packaging/generic/workdirs/preconfigured-tals/ripe-pilot.tal"
    tal <- fromEither $ first TAL_E $ parseTAL $ U.convert t        
    (u, ro) <- fetchTACertificate tal
    x <- pureToValidatorT $ validateTACert tal u ro
    pure (ro, x)
  say $ "done " <> show result

getTACert = do
  let conf = (createLogger, RsyncConf "/tmp/rsync", vContext $ URI "something.cer")
  runValidatorT conf $ do
    t <- fromTry (RsyncE . FileReadError . U.fmtEx) $ 
      B.readFile "/Users/mpuzanov/Projects/rpki-validator-3/rpki-validator/src/main/resources/packaging/generic/workdirs/preconfigured-tals/ripe-pilot.tal"
    tal <- fromEither $ first TAL_E $ parseTAL $ U.convert t        
    (u, ro) <- fetchTACertificate tal
    x <- pureToValidatorT $ validateTACert tal u ro
    pure (ro, x)


validateTreeFromTA :: Environment 'ReadWrite -> IO ()
validateTreeFromTA env = do  
  let repo = RsyncRepository (URI "rsync://rpki.ripe.net/repository")    
  nu <- now
  let conf = (createLogger, RsyncConf "/tmp/rsync", 
              Config getParallelism, 
              vContext $ URI "something.cer", 
              nu)              
  store <- createObjectStore env
  x <- runValidatorT conf $ do
    taCert <- rsyncFile (URI "rsync://rpki.ripe.net/ta/ripe-ncc-ta.cer") (pure . id)
    processRsync repo store
    validateTree store taCert

  say $ "x = " <> show x

  pure ()

validateTreeFromStore :: Environment 'ReadWrite -> IO ()
validateTreeFromStore env = do    
    nu <- now
    let conf = (createLogger, RsyncConf "/tmp/rsync", 
                Config getParallelism, 
                vContext $ URI "something.cer", 
                nu)              
    store <- createObjectStore env
    x <- runValidatorT conf $ do
      taCert <- rsyncFile (URI "rsync://rpki.ripe.net/ta/ripe-ncc-ta.cer") (pure . id)
      validateTree store taCert
  
    say $ "x = " <> show x
  
    pure ()  

main :: IO ()
main = do 
  -- testCrl
  -- runValidate  
  -- mkLmdb >>= saveSerialised
  -- mkLmdb >>= saveOriginal
  -- usingLoggerT (LogAction putStrLn) $ lift app
  -- processTAL
  -- mkLmdb >>= void . saveRsyncRepo
  mkLmdb >>= validatorUpdateRRDPRepo
  -- testSignature

say :: String -> IO ()
say s = do
  let !ss = s
  t <- getCurrentTime
  putStrLn $ show t <> ": " <> ss

createLogger :: AppLogger
createLogger = AppLogger fullMessageAction
  where
    fullMessageAction = upgradeMessageAction defaultFieldMap $ 
      cmapM fmtRichMessageDefault logTextStdout  