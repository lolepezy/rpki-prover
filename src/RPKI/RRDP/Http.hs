{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}

module RPKI.RRDP.Http where

import Control.Exception.Lifted
import Control.Lens ((^.))
import Control.Monad.Except

import Conduit
import Data.Conduit.Internal (zipSinks)

import Data.Generics.Product.Typed

import Data.IORef.Lifted

import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import           Data.Tuple.Strict

import GHC.Generics (Generic)

import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Simple (getResponseBody, getResponseStatusCode, httpSource)

import RPKI.AppContext
import RPKI.AppMonad
import RPKI.Config
import RPKI.Domain
import RPKI.Parse.Parse
import RPKI.Reporting
import qualified RPKI.Util as U

import qualified Crypto.Hash.SHA256 as S256

import System.IO (Handle, hClose, withFile, IOMode(..))
import System.IO.Temp (withTempFile)

import System.IO.Posix.MMap (unsafeMMapFile)

import Data.Version
import qualified Paths_rpki_prover as Autogen


class Blob bs where    
    readB :: FilePath -> Size -> IO bs
    sizeB :: bs -> Size

instance Blob LBS.ByteString where    
    sizeB = Size . fromIntegral . LBS.length 
    readB f _ = lazyFsRead f

instance Blob BS.ByteString where    
    sizeB = Size . fromIntegral . BS.length 
    readB f s = 
        if s < 50_000_000 
            -- read relatively small files in memory entirely
            -- and mmap bigger ones.
            then fsRead f
            else mapFile f


-- | Download HTTP content to a temporary file and return the context as lazy/strict ByteString. 
-- Snapshots (and probably some deltas) can be quite big so we don't want to keep them in memory,
-- that's why it's preferable to use lazy ByteString.
--
-- NOTE: The file will be deleted from the temporary directory by withTempFile, 
-- but the descriptor taken by readFile will stay until the byte string it GC-ed, 
-- so it's safe to use them after returning from this function.
--
downloadToBS :: (Blob bs, MonadIO m) => 
                Config ->
                URI -> 
                m (bs, Size, HttpStatus)
downloadToBS config uri@(URI u) = liftIO $ do    
    let tmpFileName = U.convert $ U.normalizeUri u
    let tmpDir = config ^. #tmpDirectory
    withTempFile tmpDir tmpFileName $ \name fd -> do
        ((_, size), status) <- 
                downloadConduit uri fd 
                    (sinkGenSize 
                        uri
                        (config ^. typed @RrdpConf . #maxSize)
                        ()
                        (\_ _ -> ()) 
                        id)
        hClose fd                    
        content <- readB name size
        pure (content, size, status)


-- | Do the same as `downloadToBS` but calculate sha256 hash of the data while 
-- streaming it to the file.
downloadHashedBS :: (Blob bs, MonadIO m) => 
                    Config ->
                    URI -> 
                    Hash -> 
                    (Hash -> Either e (bs, Size, HttpStatus)) ->
                    m (Either e (bs, Size, HttpStatus))
downloadHashedBS config uri@(URI u) expectedHash hashMishmatch = liftIO $ do
    -- Download xml file to a temporary file and read it as a lazy bytestring 
    -- to minimize the heap. Snapshots can be pretty big, so we don't want 
    -- a spike in heap usage.
    let tmpFileName = U.convert $ U.normalizeUri u
    let tmpDir = config ^. #tmpDirectory  
    withTempFile tmpDir tmpFileName $ \name fd -> do
        ((actualHash, size), status) <- 
                downloadConduit uri fd 
                    (sinkGenSize                     
                       uri    
                        (config ^. typed @RrdpConf . #maxSize)
                        S256.init
                        S256.update
                        (U.mkHash . S256.finalize))
        if actualHash /= expectedHash 
            then pure $! hashMishmatch actualHash
            else do
                hClose fd
                content <- readB name size
                pure $ Right (content, size, status)
                

-- | Fetch arbitrary file using the streaming implementation
-- 
fetchRpkiObject :: AppContext s ->
                RrdpURL ->             
                ValidatorT IO RpkiObject
fetchRpkiObject appContext uri = do
    (content, _, _) <- fromTry (RrdpE . CantDownloadFile . U.fmtEx) $
                            downloadToBS 
                            (appContext ^. typed @Config) 
                            (getURL uri) 
                        
    fromEitherM $ pure $ first ParseE $ readObject (RrdpU uri) content


downloadToFile :: MonadIO m => 
                URI 
                -> FilePath
                -> Size                
                -> m HttpStatus
downloadToFile uri file limit = liftIO $ do    
    fmap snd $ withFile file WriteMode $ \fd -> do 
        downloadConduit uri fd 
            (sinkGenSize 
                uri
                limit
                ()
                (\_ _ -> ()) 
                id) 


lazyFsRead :: FilePath -> IO LBS.ByteString
lazyFsRead = LBS.readFile

fsRead :: FilePath -> IO BS.ByteString
fsRead = BS.readFile 

mapFile :: FilePath -> IO BS.ByteString
mapFile = unsafeMMapFile

data OversizedDownloadStream = OversizedDownloadStream URI Size 
    deriving stock (Show, Eq, Ord, Generic)    

instance Exception OversizedDownloadStream
    

-- | Stream URL content to a file suing http conduit.
--
downloadConduit :: (MonadIO m, MonadUnliftIO m) => 
                    URI 
                    -> Handle 
                    -> ConduitT BS.ByteString Void (ResourceT m) t
                    -> m (t, HttpStatus)
downloadConduit (URI u) fileHandle extraSink = do 
    req <- liftIO $ parseRequest $ Text.unpack u    
    let req' = req { 
            requestHeaders = (hUserAgent, userAgent) : requestHeaders req,
            -- Since the whole RRDP fetching process is limited in time
            -- it's fine (and sometimes necessary) to set some ridiculously 
            -- high timeout here (10 minutes).
            responseTimeout = responseTimeoutMicro 600_000_000
        }
    status <- liftIO $ newIORef mempty
    let getSrc r = do         
            liftIO $ writeIORef status $ HttpStatus $ getResponseStatusCode r         
            getResponseBody r

    (z, _) <- runConduitRes 
                    $ httpSource req' getSrc
                    .| zipSinks extraSink (sinkHandle fileHandle)    

    (z,) <$> liftIO (readIORef status)

userAgent :: BS.ByteString
userAgent = U.convert $ "rpki-prover-" <> showVersion Autogen.version

-- | Calculate size and extra arbitrary function of the sinked stream
-- | and throw and exception is the size gets too big.
-- 
sinkGenSize :: MonadIO m => 
            URI 
            -> Size             
            -> h
            -> (h -> BS.ByteString -> h)
            -> (h -> a)
            -> ConduitT BS.ByteString o m (a, Size)
sinkGenSize uri maxAllowedSize initialValue updateValue finalValue = do 
    T2 h s <- loop $ T2 initialValue 0
    pure (h, s)
  where    
    loop (T2 ctx s) =
        await >>= \case
            Nothing -> pure $! T2 (finalValue ctx) s
            Just chunk -> let 
                !newSize = s + (fromIntegral (BS.length chunk) :: Size)                
                in if newSize > maxAllowedSize
                    then liftIO $ throwIO $ OversizedDownloadStream uri newSize
                    else loop $! T2 (updateValue ctx chunk) newSize
