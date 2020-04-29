{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}

import Colog hiding (extract)

import Control.Monad
import Control.Parallel.Strategies hiding ((.|))
import Control.Concurrent.Async

import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as LBS
import           Data.MultiMap                  (MultiMap)
import qualified Data.MultiMap                  as MultiMap
import qualified Data.Text                      as Text

import           Data.Maybe

import           Data.X509.Validation

import           System.FilePath.Find

import           Data.Time.Clock                (getCurrentTime)

import           Lmdb.Codec
import           Lmdb.Connection
import           Lmdb.Types
import RPKI.Store.Base.LMDB (LmdbEnv)

import qualified Data.ByteString.Streaming      as Q
import           Data.ByteString.Streaming.HTTP
import           Streaming

import System.IO.Posix.MMap.Lazy (unsafeMMapFile)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Repository
import           RPKI.Errors
import           RPKI.Parse.Parse
import           RPKI.TopDown
import           RPKI.Logging
import           RPKI.Config
import           RPKI.RRDP.Update
import           RPKI.TAL
import           RPKI.Rsync
import           RPKI.Store.Stores
import           RPKI.Validation.Crypto
import           RPKI.Validation.ObjectValidation
import qualified RPKI.Util  as U
import RPKI.Store.Util


makeLmdbMap :: ModeBool e => Environment e -> IO (Database Hash BS.ByteString)
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

type ReadStuff = (String, Either (ParseError Text.Text) [RpkiObject])

readRepo :: IO [(String, ParseResult RpkiObject)]
readRepo = do
    fileNames <- getFileNames
    forM fileNames $ \f -> do
            bs <- BS.readFile f
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
--         bs <- BS.readFile f
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

--   db :: Database Hash (String, BS.ByteString) <- readWriteTransaction lmdb $ getDatabase (Just "originals")  

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
--                       bs <- BS.readFile f
--                       pure (f, bs)) $
--       S.each fileNames

--     read_ db fileNames chanOut = 
--       S.effects $ 
--       S.mapM (\(f, bs) -> put db (sha256s bs) (Just (f, bs))) $       
--       S.mapM (liftIO . wait) $
--       S.mapM (\_ -> liftIO $ Chan.readChan chanOut) $
--       S.each fileNames
     
--     readAll :: Database Hash (String, BS.ByteString) -> IO [RpkiObject]
--     readAll db = readOnlyTransaction lmdb $ do
--         ks <- keys db
--         S.toList_ $ 
--           S.concat $
--           S.map (\(f, bs) -> readObject f bs) $
--           S.concat $ 
--           S.mapM (get db) $ 
--           S.each ks      
          


-- testSignature :: IO ()
-- testSignature = do
--   parsed <- readRepo
--   let objects = [ o | (_, Right o) <- parsed ]

--   let roaFile = "/Users/mpuzanov/ripe/tmp//rpki/repo/ripe.net/repository/DEFAULT/5b/6ab497-16d9-4c09-94d9-1be4c416a09c/1/XY9rz4RATnJEwJxBpE7ExbD-Ubk.roa"  
--   bs  <- BS.readFile roaFile
--   let Right roa = readObject roaFile bs

--   putStrLn $ "roa blob = " ++ show bs

--   let rr@(RoaRO (FullMeta (RpkiMeta { aki = Just (AKI ki)}) _, cms@(CMS so))) = roa
--   let CertificateWithSignature 
--         _ 
--         (SignatureAlgorithmIdentifier signAlgorithm) 
--         (SignatureValue sign) encoded = getEECert so

--   putStrLn $ "encoded = " ++ show encoded

--   let skiMap = bySKIMap objects
--   let [x@(CerRO cer@(_, ResourceCertificate parentCert))] = MultiMap.lookup (SKI ki) skiMap

--   let s = validateSignature rr cer

--   putStrLn $ "x = " <> show x
--   putStrLn $ "s = " <> show s

--   let bss = [ BS.take len $ BS.drop b bs | b <- [1..BS.length bs - 1], len <- [1..BS.length bs - b]]

--   let pubKey = certPubKey $ cwsX509certificate $ withRFC parentCert certX509              
--   let checks = map (\b -> (b, verifySignature signAlgorithm pubKey b sign)) bss 

--   putStrLn $ "valid signature = " <> show [ b | (b, SignaturePass) <- checks]

--   putStrLn $ "validateCMSSignature = " ++ (show $ validateCMSSignature cms)

  
-- testCrl :: IO ()
-- testCrl = do
  
--   bs  <- BS.readFile "/Users/mpuzanov/ripe/tmp/rpki/repo/ripe.net/repository/ripe-ncc-ta.crl"
--   -- bs  <- BS.readFile "/Users/mpuzanov/ripe/tmp/rpki/repo/lacnic/repository/lacnic/0043f52c-eeee-4d62-97e6-44c83c8beb9f/3c4a28de1ccccd1e98eb95feb157d61c6659de78.crl"  
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

--       let bss = [ BS.take len $ BS.drop b bs | b <- [1..BS.length bs - 1], len <- [1..BS.length bs - b]]
--       let checks = map (\b -> (b, verifySignature signAlgorithm pubKey b sign)) bss 
--       putStrLn $ "valid signature = " <> show [ b | (b, SignaturePass) <- checks]

--       putStrLn $ "verify = " <> (show $ verifySignature signAlgorithm pubKey encoded sign)
      

validateKeys :: [RpkiObject] -> Either String [SignatureVerification]
validateKeys objects = 
  case [ ro | ro <- objects, isNothing $ getAKI ro ] of
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
      ro@(MftRO mft) -> eeSignAndCMSSign ro $ extract mft 
      ro@(RoaRO roa) -> eeSignAndCMSSign ro $ extract roa
      ro@(GbrRO gbr) -> eeSignAndCMSSign ro $ extract gbr
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


byAKIMap :: WithAKI a => [a] -> MultiMap AKI a
byAKIMap ros = MultiMap.fromList [ (a, ro) | ro <- ros, a <- maybeToList (getAKI ro) ]

parseWithTmp :: FilePath -> (LBS.ByteString -> IO a) -> IO a
parseWithTmp filename f = --LBS.readFile filename >>= f
  withSystemTempFile "test_mmap_" $ \name h -> do
  --   -- LBS.readFile filename >>= LBS.writeFile name
    runResourceT $ Q.toHandle h $ Q.readFile filename
    hClose h
    f =<< unsafeMMapFile name   
  --   -- f =<< LBS.hGetContents h   


createAppContext :: AppContext
createAppContext = AppContext {
    logger = createLogger,
    config = Config getParallelism,
    rsyncConf = RsyncConf "/tmp/rsync"
}

processRRDP :: LmdbEnv -> IO ()
processRRDP env = do
    say "begin"      
    let repos = [
                rrdpR (URI "https://rrdp.ripe.net/notification.xml"),
                rrdpR (URI "https://rrdp.arin.net/notification.xml"),
                rrdpR (URI "https://rrdp.apnic.net/notification.xml"),
                rrdpR (URI "https://rrdp.afrinic.net/notification.xml"),
                rrdpR (URI "https://rrdp.afrinic.net/broken.xml")
            ]
    let conf = (createLogger, Config getParallelism, vContext $ URI "something.cer")
    database <- createDatabase env    
    as <- forM repos $ \repo ->
        async $ runValidatorT conf $ fetchRepository createAppContext database repo

    e <- forM as wait

    say $ "result " <> show e

saveRsyncRepo env = do  
    database <- createDatabase env
    let repo = rsyncR (URI "rsync://rpki.ripe.net/repository")    
    let vc = vContext $ URI "something.cer"    
    runValidatorT vc $ fetchRepository createAppContext database repo

saveRsync :: p -> IO ()
saveRsync env = do
    say "begin"  
    e <- runValidatorT (vContext $ URI "something.cer") $ rsyncFile createAppContext 
        (URI "rsync://rpki.ripe.net/ta/ripe-ncc-ta.cer")
    say $ "done " <> show e

loadRsync env = do
    say "begin"      
    store <- createObjectStore env        
    void $ loadRsyncRepository createAppContext (URI "rsync://rpki.ripe.net/repository") 
                "/tmp/rsync/rsync___rpki.ripe.net_repository" store
    say $ "done "

processTAL :: LmdbEnv -> IO ()
processTAL env = do
    say "begin"      
    database <- createDatabase env    
    let appContext = createAppContext
    result <- runValidatorT (vContext $ URI "something.cer") $ do
        t <- fromTry (RsyncE . FileReadError . U.fmtEx) $ 
            -- BS.readFile "/Users/mpuzanov/Projects/rpki-validator-3/rpki-validator/src/main/resources/packaging/generic/workdirs/preconfigured-tals/afrinic.tal"
            BS.readFile "/Users/mpuzanov/Projects/rpki-validator-3/rpki-validator/src/main/resources/packaging/generic/workdirs/preconfigured-tals/lacnic.tal"
        tal <- vHoist $ fromEither $ first TAL_E $ parseTAL $ U.convert t                        
        x <- bootstrapTA appContext tal database
        pure x
    say $ "done " <> show result

getTACert = 
    runValidatorT (vContext $ URI "something.cer") $ do
        t <- fromTry (RsyncE . FileReadError . U.fmtEx) $ 
            BS.readFile "/Users/mpuzanov/Projects/rpki-validator-3/rpki-validator/src/main/resources/packaging/generic/workdirs/preconfigured-tals/apinic.tal"
        tal <- vHoist $ fromEither $ first TAL_E $ parseTAL $ U.convert t        
        (u, ro) <- fetchTACertificate createAppContext tal
        x <- vHoist $ validateTACert tal u ro
        pure (ro, x)


validateTreeFromTA :: LmdbEnv -> IO ()
validateTreeFromTA env = do  
    -- let repo = RsyncPublicationPoint (URI "rsync://rpki.ripe.net/repository")    
    let repo = rsyncR (URI "rsync://rpki.apnic.net/member_repository/")        
    -- let repo = RrdpRepository (URI "https://rrdp.apnic.net/notification.xml") Nothing        
    database <- createDatabase env    
    let appContext = createAppContext
    let taName = TaName "Test TA"
    let vContext' = vContext $ URI "something.cer"
    x <- runValidatorT vContext' $ do
        -- CerRO taCert <- rsyncFile appContext (URI "rsync://rpki.ripe.net/ta/ripe-ncc-ta.cer")
        CerRO taCert <- rsyncFile appContext (URI "rsync://rpki.apnic.net/repository/apnic-rpki-root-iana-origin.cer")        
        let e1 = getRrdpNotifyUri (cwsX509certificate $ getCertWithSignature taCert)
        let e2 = getRepositoryUri (cwsX509certificate $ getCertWithSignature taCert)
        lift3 $ say $ "taCert SIA = " <> show e1 <> ", " <> show e2
        fetchRepository appContext database repo
        lift3 $ validateCA appContext vContext' database taName taCert

    say $ "x = " <> show x



validateTreeFromStore :: LmdbEnv -> IO ()
validateTreeFromStore env = do       
    let appContext = createAppContext
    let taCertURI = URI "rsync://rpki.apnic.net/repository/apnic-rpki-root-iana-origin.cer"
    database <- createDatabase env
    x <- runValidatorT (vContext taCertURI) $ do
        -- CerRO taCert <- rsyncFile (URI "rsync://rpki.ripe.net/ta/ripe-ncc-ta.cer")
        -- CerRO taCert <- rsyncFile (URI "rsync://rpki.apnic.net/repository/apnic-rpki-root-iana-origin.cer")
        CerRO taCert <- rsyncFile appContext taCertURI
        lift3 $ say $ "taCert = " <> show taCert
        let e1 = getRrdpNotifyUri (cwsX509certificate $ getCertWithSignature taCert)
        let e2 = getRepositoryUri (cwsX509certificate $ getCertWithSignature taCert)
        lift3 $ say $ "taCert SIA = " <> show e1 <> ", " <> show e2
        -- lift3 $ validateCA conf (objectStore, resultStore, repositoryStore) taCert
    say $ "x = " <> show x  
    pure ()  

main :: IO ()
main = do 
    -- mkLmdb "./data"  >>= void . saveRsyncRepo
    -- mkLmdb "./data" >>= processRRDP
    -- mkLmdb "./data/" >>= void . saveRsyncRepo
    -- mkLmdb "./data/" >>= validateTreeFromStore
    -- mkLmdb "./data/" >>= validateTreeFromTA
    mkLmdb "./data/" 100 >>= processRRDP



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