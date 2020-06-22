{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes        #-}

import Colog hiding (extract)

import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Lifted
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Parallel.Strategies     hiding ((.|))

import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as LBS
import           Data.MultiMap                   (MultiMap)
import qualified Data.MultiMap                   as MultiMap
import qualified Data.Text                       as Text


import           Data.Maybe

import           Data.String.Interpolate.IsString

import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Parse
import           Data.ASN1.Types

import           Data.X509
import           Data.X509.Validation

import           System.FilePath.Find

import           Data.Time.Clock                (getCurrentTime)

import           Lmdb.Codec
import           Lmdb.Connection
import           Lmdb.Types
import           RPKI.Store.Base.LMDB (LmdbEnv)

import qualified Data.ByteString.Streaming      as Q
import           Data.ByteString.Streaming.HTTP
import           Streaming

import           Data.List.NonEmpty (NonEmpty(..))

import System.IO.Posix.MMap.Lazy (unsafeMMapFile)
import qualified System.IO.Posix.MMap as Mmap
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
import           RPKI.AppContext
import           RPKI.Parallel
import           RPKI.TAL
import           RPKI.Rsync
import           RPKI.Version
import           RPKI.Store.Database
import           RPKI.Store.Repository (getTaPublicationPoints)
import           RPKI.Validation.Crypto
import           RPKI.Validation.ObjectValidation

import RPKI.Store.Base.Storable 

import qualified RPKI.Util  as U
import RPKI.Store.Util



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

      
-- validateKeys :: [RpkiObject] -> Either String [SignatureVerification]
-- validateKeys objects = 
--   case [ ro | ro <- objects, isNothing $ getAKI ro ] of
--     []      -> Left $ "No TA certificate"    
--     taCerts -> Right $ concat [ validateChildren ro | ro <- taCerts ]
--   where    
--     validateChildren :: RpkiObject -> [SignatureVerification]
--     validateChildren (CerRO parentCert) = concat childrenVerification 
--       where 
--         childrenVerification = parMap strategy 
--             (validateChildParent parentCert) $ 
--             children parentCert
--         strategy = parListChunk 1000 rseq
--     validateChildren _ = []

--     validateChildParent parentCert = \case 
--       ro@(MftRO mft) -> eeSignAndCMSSign ro $ extract mft 
--       ro@(RoaRO roa) -> eeSignAndCMSSign ro $ extract roa
--       ro@(GbrRO gbr) -> eeSignAndCMSSign ro $ extract gbr
--       ro@(CerRO _) -> [validateSignature ro parentCert] <> validateChildren ro
--       where
--         eeSignAndCMSSign  :: RpkiObject -> CMS a -> [SignatureVerification]
--         eeSignAndCMSSign ro cms = [ validateCMSSignature cms, validateSignature ro parentCert ]            

--     children a = MultiMap.lookup (AKI ki) akiMap
--       where
--         SKI ki = getSKI a

--     akiMap = byAKIMap objects
    

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


createAppContext :: AppLogger -> ValidatorT vc IO AppEnv
createAppContext logger = do

    -- TODO Make it configurable?
    home <- fromTry (InitE . InitError . fmtEx) $ getEnv "HOME"
    let rootDir = home </> ".rpki"
    
    tald   <- fromEitherM $ first (InitE . InitError) <$> talsDir  rootDir 
    rsyncd <- fromEitherM $ first (InitE . InitError) <$> rsyncDir rootDir
    tmpd   <- fromEitherM $ first (InitE . InitError) <$> tmpDir   rootDir
    lmdb   <- fromEitherM $ first (InitE . InitError) <$> lmdbDir  rootDir 

    lmdbEnv  <- fromTry (InitE . InitError . fmtEx) $ mkLmdb lmdb 1000
    database <- fromTry (InitE . InitError . fmtEx) $ createDatabase lmdbEnv

    -- clean up tmp directory if it's not empty
    fromTry (InitE . InitError . fmtEx) $ 
        listDirectory tmpd >>= mapM_ (removeFile . (tmpd </>))

    versions <- liftIO createDynamicState

    -- TODO Make it configurable
    let parallelism = Parallelism getParallelism 64

    appBottlenecks <- liftIO $ do 
        cpuBottleneck <- newBottleneckIO $ cpuParallelism parallelism
        ioBottleneck  <- newBottleneckIO $ ioParallelism parallelism
        pure $ AppBottleneck cpuBottleneck ioBottleneck

    -- TODO read stuff from the config, CLI
    pure $ AppContext {        
        logger = logger,
        config = Config {
            talDirectory = tald,
            parallelism  = parallelism,
            rsyncConf    = RsyncConf rsyncd,
            rrdpConf     = RrdpConf { 
                tmpRoot = tmpd,
                -- Do not download files bigger than 1Gb
                -- TODO Make it configurable
                maxSize = Size 1024 * 1024 * 1024
            },
            validationConfig = ValidationConfig {
                rrdpRepositoryRefreshInterval  = 2 * 60,
                rsyncRepositoryRefreshInterval = 10 * 60
            }
        },
        versions = versions,
        database = database,
        appBottlenecks = appBottlenecks
    }

-- processRRDP :: LmdbEnv -> IO ()
-- processRRDP env = do
--     say "begin"      
--     let repos = [
--                 rrdpR (URI "https://rrdp.ripe.net/notification.xml")
--                 -- rrdpR (URI "https://rrdp.arin.net/notification.xml"),
--                 -- rrdpR (URI "https://rrdp.apnic.net/notification.xml"),
--                 -- rrdpR (URI "https://rrdp.afrinic.net/notification.xml"),
--                 -- rrdpR (URI "https://rrdp.afrinic.net/broken.xml")
--             ]
--     let conf = (createLogger, Config getParallelism, vContext $ URI "something.cer")
--     database <- createDatabase env    
--     as <- forM repos $ \repo -> do
--         appContext <- createAppContext
--         async $ runValidatorT conf $ fetchRepository appContext database repo

--     e <- forM as wait

--     say $ "result " <> show e
    -- <> show e

-- processTAL :: LmdbEnv -> IO ()
-- processTAL env = do
--     say "begin"      
--     database <- createDatabase env    
--     let appContext = createAppContext
--     result <- runValidatorT (vContext $ URI "something.cer") $ do
--         t <- fromTry (RsyncE . FileReadError . U.fmtEx) $             
--             BS.readFile "/Users/mpuzanov/.rpki-cache/tals/ripe.tal"
--         tal <- vHoist $ fromEither $ first TAL_E $ parseTAL $ U.convert t                        
--         x <- bootstrapTA appContext tal database
--         pure x
--     say $ "done " <> show result

-- getTACert = 
--     runValidatorT (vContext $ URI "something.cer") $ do
--         t <- fromTry (RsyncE . FileReadError . U.fmtEx) $ 
--             BS.readFile "/Users/mpuzanov/Projects/rpki-validator-3/rpki-validator/src/main/resources/packaging/generic/workdirs/preconfigured-tals/apinic.tal"
--         tal <- vHoist $ fromEither $ first TAL_E $ parseTAL $ U.convert t        
--         appContext <- liftIO createAppContext
--         (u, ro) <- fetchTACertificate appContext tal
--         x <- vHoist $ validateTACert tal u ro
--         pure (ro, x)


validateTreeFromTA :: LmdbEnv -> IO ()
validateTreeFromTA env = do  
    -- let repo = RsyncPublicationPoint (URI "rsync://rpki.ripe.net/repository")    
    let repo = rsyncR (URI "rsync://rpki.apnic.net/member_repository/")        
    -- let repo = RrdpRepository (URI "https://rrdp.apnic.net/notification.xml") Nothing        
    database <- createDatabase env    
    appContext <- createAppContext database
    let taName = TaName "Test TA"
    let vContext' = vContext $ URI "something.cer"
    x <- runValidatorT vContext' $ do
        -- CerRO taCert <- rsyncRpkiObject appContext (URI "rsync://rpki.ripe.net/ta/ripe-ncc-ta.cer")
        CerRO taCert <- rsyncRpkiObject appContext (URI "rsync://rpki.apnic.net/repository/apnic-rpki-root-iana-origin.cer")        
        let e1 = getRrdpNotifyUri (cwsX509certificate $ getCertWithSignature taCert)
        let e2 = getRepositoryUri (cwsX509certificate $ getCertWithSignature taCert)
        liftIO $ say $ "taCert SIA = " <> show e1 <> ", " <> show e2
        fetchRepository appContext repo
        -- emptyTopDownContext taName
        -- liftIO $ validateCA appContext vContext' database taName taCert

    say $ "x = " <> show x

validateTreeFromStore :: LmdbEnv -> IO ()
validateTreeFromStore env = do       
    database <- createDatabase env
    appContext <- createAppContext database
    let taCertURI = URI "rsync://repository.lacnic.net/rpki/lacnic/rta-lacnic-rpki.cer"    
    x <- runValidatorT (vContext taCertURI) $ do
        CerRO taCert <- rsyncRpkiObject appContext taCertURI
        let taName = TaName "Test TA"
        storedPubPoints <- roAppTxEx database storageError $ \tx -> 
                                getTaPublicationPoints tx (repositoryStore database) taName
        topDownContext <- emptyTopDownContext appContext taName storedPubPoints taCert
        fromTry (UnspecifiedE . U.fmtEx) $
                validateCA appContext (VContext (taCertURI :| [])) topDownContext taCert    

        Stats {..} <- liftIO $ readTVarIO (objectStats topDownContext)
        logDebugM (logger appContext) [i| TA: #{taName} validCount = #{validCount} |]                                
    say $ "x = " <> show x  
    pure ()  


validateFully :: LmdbEnv -> IO ()
validateFully env = do
    say "begin"      
    database <- createDatabase env    
    appContext <- createAppContext database
    let tals = [ "afrinic.tal", "apnic.tal", "arin.tal", "lacnic.tal", "ripe.tal" ]
    -- let tals = [ "ripe.tal" ]

    as <- forM tals $ \tal -> async $
        runValidatorT (vContext $ URI "something.cer") $ do
            t <- fromTry (RsyncE . FileReadError . U.fmtEx) $             
                BS.readFile $ "/Users/mpuzanov/.rpki-cache/tals/" <> tal
            talContent <- vHoist $ fromEither $ first TAL_E $ parseTAL $ U.convert t                        
            liftIO $ bootstrapTA appContext talContent

    result <- forM as wait

    say $ "done " <> show result



main :: IO ()
main = do 
    -- mkLmdb "./data"  >>= void . saveRsyncRepo
    -- mkLmdb "./data" 100 >>= processRRDP
    -- testBigCrl
    -- mkLmdb "./data/" >>= void . saveRsyncRepo
    -- mkLmdb "./data/" 100 >>= validateTreeFromStore
    -- mkLmdb "./data/" >>= validateTreeFromTA
    mkLmdb "./data/" 100 >>= validateFully

testBigCrl :: IO ()
testBigCrl = do 
    -- b <- Mmap.unsafeMMapFile "/Users/mpuzanov/ripe/tmp/rpki/repo/ripe.net/DEFAULT/28/854c9b-b32d-427c-883f-ac314a8c99d6/1/MVNeG0ke-OOpeCSbLlrCGcZncvY.crl"
    b <- Mmap.unsafeMMapFile "/Users/mpuzanov/ripe/tmp/rpki/repo/ripe.net/DEFAULT/KpSo3VVK5wEHIJnHC2QHVV3d5mk.crl"

    -- let Right z = decodeASN1' BER b
    -- say $ show $ length  z


    case readObject "test.crl" b of
        Left e ->
            say $ "Error: " <> show e
        Right ro -> do 
            let StorableObject _ (SValue (Storable bs)) = toStorableObject ro
            say $ show $ BS.length bs


testQ :: IO ()
testQ = do 
    quu <- atomically $ createQuu 5
    void $ concurrently (writeQ quu `finally` atomically (closeQueue quu)) (readQ quu)
    where 
        writeQ quu = forM_ [0..20 :: Int] $ \c ->
            atomically $ writeQuu quu c            
            
        readQ quu = do 
            readQueueChunked quu 10 (\c -> putStrLn $ "c = " <> show c)
            pure ()
        


say :: String -> IO ()
say s = do
    let !ss = s
    t <- getCurrentTime
    putStrLn $ show t <> ": " <> ss

createLogger :: IO AppLogger
createLogger = do 
    lock <- newMVar True
    pure $ AppLogger fullMessageAction lock
    where
        fullMessageAction = upgradeMessageAction defaultFieldMap $ 
            cmapM fmtRichMessageDefault logTextStdout  