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
import           RPKI.RRDP.HttpContext
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


downloadToStrictBS :: MonadIO m => 
                    HttpContext ->
                    RrdpConf ->
                    URI -> 
                    m (BS.ByteString, Size)
downloadToStrictBS httpContext rrdp uri = 
    downloadToBS httpContext rrdp uri Mmap.unsafeMMapFile    

downloadToLazyBS :: MonadIO m => 
                    HttpContext ->
                    RrdpConf ->
                    URI ->
                    m (LBS.ByteString, Size)
downloadToLazyBS httpContext rrdp uri = 
    downloadToBS httpContext rrdp uri MmapLazy.unsafeMMapFile    


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
                (FilePath -> IO bs) ->
                m (bs, Size)
downloadToBS httpContext RrdpConf {..} uri@(URI u) mmap = liftIO $ do
    -- Download xml file to a temporary file and MMAP it to a lazy bytestring 
    -- to minimize the heap. Snapshots can be pretty big, so we don't want 
    -- a spike in heap usage.
    let tmpFileName = U.convert $ U.normalizeUri u
    withTempFile tmpRoot tmpFileName $ \name fd -> do
        (_, !size) <- streamHttpToFileWithActions httpContext uri DoNothing maxSize fd
        hClose fd                    
        content <- mmap name
        pure (content, size)

-- | Do the same as `downloadToBS` but calculate sha256 hash of the data while 
-- streaming it to the file.
downloadHashedLazyBS :: (MonadIO m) => 
                        HttpContext ->
                        RrdpConf ->
                        URI -> 
                        Hash -> 
                        (Hash -> Either e (LBS.ByteString, Size)) ->
                        m (Either e (LBS.ByteString, Size))
downloadHashedLazyBS httpContext RrdpConf {..} uri@(URI u) hash hashMishmatch = liftIO $ do
    -- Download xml file to a temporary file and MMAP it to a lazy bytestring 
    -- to minimize the heap. Snapshots can be pretty big, so we don't want 
    -- a spike in heap usage.
    let tmpFileName = U.convert $ U.normalizeUri u    
    withTempFile tmpRoot tmpFileName $ \name fd -> do
        (actualHash, !size) <- streamHttpToFileWithActions httpContext uri DoHashing maxSize fd
        if actualHash /= hash 
            then pure $! hashMishmatch actualHash
            else do
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
--
-- throws IOException if something goes wrong
streamHttpToFileWithActions :: MonadIO m =>
                            HttpContext ->
                            URI -> 
                            ActionWhileDownloading -> 
                            Size -> 
                            Handle -> 
                            m (Hash, Size)
streamHttpToFileWithActions 
                    (HttpContext tlsManager) 
                    (URI uri) 
                    whileDownloading 
                    maxSize 
                    destinationHandle = 
    liftIO $ do
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
fetchRpkiObject :: AppContext s ->
                RrdpURL ->             
                ValidatorT vc IO RpkiObject
fetchRpkiObject appContext uri = withHttp $ \httpContext -> do
    (content, _) <- fromTry (RrdpE . CantDownloadFile . U.fmtIOEx) $
                        downloadToStrictBS 
                            httpContext
                            (appContext ^. typed @Config . typed @RrdpConf)
                            (getURL uri) 
                        
    fromEitherM $ pure $ first ParseE $ readObject (RrdpU uri) content
    