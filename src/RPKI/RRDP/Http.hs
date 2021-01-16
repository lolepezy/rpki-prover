{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLabels   #-}

module RPKI.RRDP.Http where

import           Control.Lens                   ((^.))
import           Control.Monad.Except
import           Data.Generics.Product.Typed

import           Data.Bifunctor                 (first)
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Text                      as Text

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Parse.Parse
import           RPKI.RRDP.HttpContext
import qualified RPKI.Util                      as U

import qualified Streaming.ByteString      as Q
import           Data.ByteString.Streaming.HTTP

import qualified Crypto.Hash.SHA256             as S256

import Network.HTTP.Types.Status

import           System.IO                      (Handle, hClose)
import qualified System.IO.Posix.MMap           as Mmap
import qualified System.IO.Posix.MMap.Lazy      as MmapLazy
import           System.IO.Temp                 (withTempFile)

import           Control.Exception.Lifted
import           Data.IORef.Lifted
import           GHC.Generics                   (Generic)


lazyFsRead :: FilePath -> IO LBS.ByteString
lazyFsRead = MmapLazy.unsafeMMapFile 
-- lazyFsRead = LBS.readFile

fsRead :: FilePath -> IO BS.ByteString
fsRead = Mmap.unsafeMMapFile 
-- fsRead = BS.readFile 

downloadToStrictBS :: MonadIO m => 
                    AppContext s ->
                    URI -> 
                    m (BS.ByteString, Size, HttpStatus)
downloadToStrictBS appContext uri = 
    downloadToBS appContext uri fsRead    

downloadToLazyBS :: MonadIO m => 
                    AppContext s ->
                    URI ->
                    m (LBS.ByteString, Size, HttpStatus)
downloadToLazyBS appContext uri = 
    downloadToBS appContext uri lazyFsRead    


-- | Download HTTP content to a temporary file and return lazy/strict ByteString content 
-- mapped onto a temporary file. Snapshots (and probably some deltas) can be big 
-- enough so that we don't want to keep them in memory.
--
-- NOTE: The file will be deleted from the temporary directory by withTempFile, 
-- but the descriptor taken by mmap/readFile will stay until the byte string it GC-ed, 
-- so it's safe to use them after returning from this function.
downloadToBS :: MonadIO m => 
                AppContext s ->
                URI -> 
                (FilePath -> IO bs) ->
                m (bs, Size, HttpStatus)
downloadToBS appContext uri@(URI u) readF = liftIO $ do
    -- Download xml file to a temporary file and MMAP it to a lazy bytestring 
    -- to minimize the heap. Snapshots can be pretty big, so we don't want 
    -- a spike in heap usage.
    let tmpFileName = U.convert $ U.normalizeUri u
    let tmpDir = appContext ^. typed @Config . #tmpDirectory
    withTempFile tmpDir tmpFileName $ \name fd -> do
        (status, _, !size) <- 
            streamHttpToFileWithActions 
                (appContext ^. #httpContext) 
                uri 
                DoNothing 
                (appContext ^. typed @Config . typed @RrdpConf . #maxSize) 
                fd
        hClose fd                    
        content <- readF name
        pure (content, size, status)

-- | Do the same as `downloadToBS` but calculate sha256 hash of the data while 
-- streaming it to the file.
downloadHashedLazyBS :: (MonadIO m) => 
                        AppContext s ->
                        URI -> 
                        Hash -> 
                        (Hash -> Either e (LBS.ByteString, Size, HttpStatus)) ->
                        m (Either e (LBS.ByteString, Size, HttpStatus))
downloadHashedLazyBS appContext uri hash' hashMishmatch = 
    downloadHashedBS appContext uri hash' hashMishmatch lazyFsRead

-- | Do the same as `downloadToBS` but calculate sha256 hash of the data while 
-- streaming it to the file.
downloadHashedStrictBS :: MonadIO m => 
                        AppContext s ->
                        URI -> 
                        Hash -> 
                        (Hash -> Either e (BS.ByteString, Size, HttpStatus)) ->
                        m (Either e (BS.ByteString, Size, HttpStatus))
downloadHashedStrictBS appContext uri hash' hashMishmatch = 
    downloadHashedBS appContext uri hash' hashMishmatch fsRead


-- | Do the same as `downloadToBS` but calculate sha256 hash of the data while 
-- streaming it to the file.
downloadHashedBS :: MonadIO m => 
                    AppContext s ->
                    URI -> 
                    Hash -> 
                    (Hash -> Either e (bs, Size, HttpStatus)) ->
                    (FilePath -> IO bs) ->
                    m (Either e (bs, Size, HttpStatus))
downloadHashedBS appContext uri@(URI u) expectedHash hashMishmatch readF = liftIO $ do
    -- Download xml file to a temporary file and MMAP it to a lazy bytestring 
    -- to minimize the heap. Snapshots can be pretty big, so we don't want 
    -- a spike in heap usage.
    let tmpFileName = U.convert $ U.normalizeUri u
    let tmpDir = appContext ^. typed @Config . #tmpDirectory  
    withTempFile tmpDir tmpFileName $ \name fd -> do
        (status, actualHash, !size) <- 
            streamHttpToFileWithActions 
                (appContext ^. #httpContext)  
                uri 
                DoHashing 
                (appContext ^. typed @Config . typed @RrdpConf . #maxSize) 
                fd
        if actualHash /= expectedHash 
            then pure $! hashMishmatch actualHash
            else do
                hClose fd
                content <- readF name
                pure $! Right (content, size, status)


data ActionWhileDownloading = DoNothing | DoHashing

newtype OversizedDownloadStream = OversizedDownloadStream Size
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
                            m (HttpStatus, Hash, Size)
streamHttpToFileWithActions 
                    (HttpContext tlsManager) 
                    (URI uri) 
                    whileDownloading 
                    maxAllowedSize 
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
                if newSize > maxAllowedSize
                    then throwIO $ OversizedDownloadStream newSize
                    else writeIORef size newSize
                pure chunk                    

        Status {..} <- withHTTP req tlsManager $ \response -> do 
                    Q.hPut destinationHandle $ 
                        Q.chunkMapM perChunkAction $ responseBody response
                    pure $ responseStatus response

        h <- readIORef hash        
        actualSize <- readIORef size  
        pure (
            HttpStatus statusCode, 
            U.mkHash $ S256.finalize h, 
            actualSize)
     

-- | Fetch arbitrary file using the streaming implementation
fetchRpkiObject :: AppContext s ->
                RrdpURL ->             
                ValidatorT IO RpkiObject
fetchRpkiObject appContext uri = do
    (content, size, status) <- fromTry (RrdpE . CantDownloadFile . U.fmtEx) $
                                    downloadToStrictBS appContext (getURL uri) 
                        
    fromEitherM $ pure $ first ParseE $ readObject (RrdpU uri) content
    