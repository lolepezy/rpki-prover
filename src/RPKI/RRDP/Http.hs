{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE RecordWildCards   #-}

module RPKI.RRDP.Http where

import           Control.Lens                   ((^.))
import           Control.Monad.Except
import           Control.Monad.Trans.Control

import           Data.Generics.Product.Fields
import           Data.Generics.Product.Typed

import           Data.Bifunctor                 (first)
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Text                      as Text

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Parse.Parse
import qualified RPKI.Util                      as U

import qualified Data.ByteString.Streaming      as Q
import           Data.ByteString.Streaming.HTTP

import qualified Crypto.Hash.SHA256             as S256

import           System.IO                      (Handle, hClose)
import qualified System.IO.Posix.MMap           as Mmap
import qualified System.IO.Posix.MMap.Lazy      as MmapLazy
import           System.IO.Temp                 (withTempFile)

import           Control.Exception.Lifted
import           Data.IORef.Lifted
import           GHC.Generics                   (Generic)



newtype HttpContext =  HttpContext Manager

withHttp :: MonadIO m => (HttpContext -> m a) -> m a
withHttp f = do
    tlsManager <- liftIO $ newManager tlsManagerSettings
    f $ HttpContext tlsManager
    
downloadToStrictBS :: MonadIO m => 
                    HttpContext ->
                    RrdpConf ->
                    URI -> 
                    (SomeException -> e) ->
                    m (Either e (BS.ByteString, Size))
downloadToStrictBS httpContext rrdp uri cantDownload = 
    downloadToBS httpContext rrdp uri cantDownload Mmap.unsafeMMapFile    

downloadToLazyBS :: MonadIO m => 
                    HttpContext ->
                    RrdpConf ->
                    URI -> 
                    (SomeException -> e) ->
                    m (Either e (LBS.ByteString, Size))
downloadToLazyBS httpContext rrdp uri cantDownload = 
    downloadToBS httpContext rrdp uri cantDownload MmapLazy.unsafeMMapFile    


-- | Download HTTP content to a temporary file and return lazy/strict ByteString content 
-- mapped onto a temporary file. Snapshots (and probably some deltas) can be big 
-- enough so that we don't want to keep them in memory.
--
-- NOTE: The file will be deleted from the temporary directory by withSystemTempFile, 
-- but the descriptor taken by mmap will stay until the byte string it GC-ed, so it's 
-- safe to use them after returning from this function.
downloadToBS :: MonadIO m => 
                HttpContext ->
                RrdpConf ->
                URI -> 
                (SomeException -> e) ->
                (FilePath -> IO bs) ->
                m (Either e (bs, Size))
downloadToBS httpContext RrdpConf {..} uri@(URI u) cantDownload mmap = liftIO $ do
    -- Download xml file to a temporary file and MMAP it to a lazy bytestring 
    -- to minimize the heap. Snapshots can be pretty big, so we don't want 
    -- a spike in heap usage.
    let tmpFileName = U.convert $ U.normalizeUri u
    withTempFile tmpRoot tmpFileName $ \name fd -> 
        streamHttpToFileWithActions httpContext uri cantDownload DoNothing maxSize fd >>= \case        
            Left e  -> pure $ Left e
            Right (_, !size) -> do
                    hClose fd                    
                    content <- mmap name
                    pure $ Right (content, size)

-- | Do the same as `downloadToBS` but calculate sha256 hash of the data while 
-- streaming it to the file.
downloadHashedLazyBS :: (MonadIO m) => 
                        HttpContext ->
                        RrdpConf ->
                        URI -> 
                        Hash -> 
                        (SomeException -> e) ->
                        (Hash -> Either e (LBS.ByteString, Size)) ->
                        m (Either e (LBS.ByteString, Size))
downloadHashedLazyBS httpContext RrdpConf {..} uri@(URI u) hash cantDownload hashMishmatch = liftIO $ do
    -- Download xml file to a temporary file and MMAP it to a lazy bytestring 
    -- to minimize the heap. Snapshots can be pretty big, so we don't want 
    -- a spike in heap usage.
    let tmpFileName = U.convert $ U.normalizeUri u    
    withTempFile tmpRoot tmpFileName $ \name fd -> do
        streamHttpToFileWithActions httpContext uri cantDownload DoHashing maxSize fd >>= \case        
            Left e -> pure $ Left e
            Right (actualHash, !size)
                | actualHash /= hash -> 
                    pure $! hashMishmatch actualHash
                | otherwise -> do
                    hClose fd
                    content <- MmapLazy.unsafeMMapFile name
                    pure $! Right (content, size)


data ActionWhileDownloading = DoNothing | DoHashing

data OversizedDownloadStream = OversizedDownloadStream Size
    deriving stock (Show, Eq, Ord, Generic)    

instance Exception OversizedDownloadStream



-- | Download HTTP stream into a file while 
-- (if needed) calculating its hash at the same time (or anything 
-- else, but so far we've only needed hash and size).
streamHttpToFileWithActions :: MonadIO m =>
                            HttpContext ->
                            URI -> 
                            (SomeException -> err) -> 
                            ActionWhileDownloading -> 
                            Size -> 
                            Handle -> 
                            m (Either err (Hash, Size))
streamHttpToFileWithActions 
                    (HttpContext tlsManager) 
                    (URI uri) 
                    errorMapping 
                    whileDownloading 
                    maxSize 
                    destinationHandle = 
    liftIO $ first errorMapping <$> try go    
    where
        go = do
            req  <- parseRequest $ Text.unpack uri

            hash <- newIORef S256.init
            size <- newIORef (0 :: Size)

            let hashingAction = 
                    case whileDownloading of
                        DoNothing -> const $ pure ()                            
                        DoHashing -> \chunk -> modifyIORef' hash (`S256.update` chunk)                                

            let perChunkAction chunk = do
                    hashingAction chunk
                    currentSize <- readIORef size
                    let !newSize = currentSize + (fromIntegral $ BS.length chunk :: Size)
                    if newSize > maxSize
                        then throwIO $ OversizedDownloadStream newSize
                        else writeIORef size newSize
                    pure chunk                    

            withHTTP req tlsManager $ \resp -> 
                Q.hPut destinationHandle $ 
                    Q.chunkMapM perChunkAction $ responseBody resp

            h <- readIORef hash        
            s <- readIORef size  
            pure $! (U.mkHash $ S256.finalize h, s)
     

-- | Fetch arbitrary file using the streaming implementation
fetchRpkiObject :: MonadIO m => 
                    AppContext s ->
                    RrdpURL ->             
                    ValidatorT vc m RpkiObject
fetchRpkiObject appContext uri = withHttp $ \httpContext -> do
    (content, _) <- fromEitherM $ downloadToStrictBS 
                        httpContext
                        (appContext ^. typed @Config . typed @RrdpConf)
                        (getURL uri) 
                        (RrdpE . CantDownloadFile . U.fmtEx)
    fromEitherM $ pure $ first ParseE $ readObject (RrdpU uri) content
    