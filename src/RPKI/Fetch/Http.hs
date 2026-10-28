module RPKI.Fetch.Http where

import           Effectful
import Control.Exception
import Control.Lens

import Conduit
import Data.Conduit.Internal (zipSinks)
import Data.Generics.Product.Typed
import Data.IORef
import Data.Maybe (listToMaybe)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import           Data.Tuple.Strict

import           Data.String.Interpolate.IsString

import GHC.Generics (Generic)

import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Simple

import RPKI.AppTypes
import RPKI.AppContext
import RPKI.AppMonad
import RPKI.Config
import RPKI.Meta.Version
import RPKI.Metrics.Process (countIncomingTraffic)
import RPKI.Domain
import RPKI.Parse.Parse
import RPKI.Reporting
import RPKI.RRDP.Types
import qualified RPKI.Util as U

import qualified Crypto.Hash.SHA256 as S256

import System.IO (Handle, hClose, withFile, IOMode(..))
import System.IO.Temp (withTempFile)

import System.IO.Posix.MMap (unsafeMMapFile)


class Blob bs where    
    readB :: FilePath -> Size -> IO bs
    sizeB :: bs -> Size

instance Blob LBS.ByteString where    
    sizeB = Size . fromIntegral . LBS.length 
    readB f _ = lazyFsRead f

instance Blob BS.ByteString where    
    sizeB = Size . fromIntegral . BS.length 
    readB f _ = mapFile f

-- | Download HTTP content to a temporary file and return the context as lazy/strict ByteString. 
-- Snapshots (and probably some deltas) can be quite big so we don't want to keep them in memory,
-- that's why it's preferable to use lazy ByteString.
--
-- NOTE: The file will be deleted from the temporary directory by withTempFile, 
-- but the descriptor taken by readFile will stay until the byte string it GC-ed, 
-- so it's safe to use them after returning from this function.
--
downloadToBS :: (Blob bs, MonadIO m) => 
                FilePath ->
                URI -> 
                HttpValidators ->
                Size ->
                m (bs, Size, HttpStatus, HttpValidators)
downloadToBS tmpDir uri@(URI u) validators maxSize = liftIO $ do    
    let tmpFileName = U.convert $ U.normalizeUri u
    withTempFile tmpDir tmpFileName $ \name fd -> do
        ((_, size), status, newValidators) <-
                downloadConduit uri validators fd 
                    (sinkGenSize uri maxSize () (\_ _ -> ()) id)
        hClose fd                    
        content <- readB name size
        pure (content, size, status, newValidators)


-- | Do the same as `downloadToBS` but calculate sha256 hash of the data while 
-- streaming it to the file.
downloadHashedBS :: (Blob bs, MonadIO m) => 
                    FilePath ->
                    URI -> 
                    HttpValidators ->
                    Hash -> 
                    Size -> 
                    (Hash -> Either e (bs, Size, HttpStatus, HttpValidators)) ->
                    m (Either e (bs, Size, HttpStatus, HttpValidators))
downloadHashedBS tmpDir uri@(URI u) validators expectedHash maxSize hashMishmatch = liftIO $ do
    -- Download xml file to a temporary file and read it as a lazy bytestring 
    -- to minimize the heap. Snapshots can be pretty big, so we don't want 
    -- a spike in heap usage.
    let tmpFileName = U.convert $ U.normalizeUri u
    withTempFile tmpDir tmpFileName $ \name fd -> do
        ((actualHash, size), status, newValidators) <- 
                downloadConduit uri validators fd 
                    (sinkGenSize uri maxSize S256.init S256.update (U.mkHash . S256.finalize))
        if actualHash /= expectedHash 
            then pure $! hashMishmatch actualHash
            else do
                hClose fd
                content <- readB name size
                pure $ Right (content, size, status, newValidators)
                

-- | Fetch arbitrary file using the streaming implementation
-- 
downloadRpkiObject :: ValidatorIO es => AppContext ->
                    RrdpURL ->
                    Eff es ParsedRpkiObject
downloadRpkiObject AppContext {..} uri = do
    let tmpDir = configValue $ config ^. #tmpDirectory
    let maxSize = config ^. typed @RrdpConf . #maxSize
    (content, _, _, _) <- 
        fromTry (RrdpE . CantDownloadFile . U.fmtEx) $
            downloadToBS tmpDir (getURL uri) noValidators maxSize

    readObject (RrdpU uri) (U.hashed content)


downloadToFile :: MonadIO m => 
                URI 
                -> FilePath
                -> Size                
                -> m HttpStatus
downloadToFile uri file limit = liftIO $ do    
    fmap (\(_, s, _) -> s) $ withFile file WriteMode $ \fd -> do 
        downloadConduit uri noValidators fd 
            (sinkGenSize uri limit () (\_ _ -> ()) id) 

-- | Do the same as `downloadToBS` but calculate sha256 hash of the data while 
-- streaming it to the file.
downloadToFileHashed :: (Blob bs, MonadIO m) 
                    => URI 
                    -> FilePath 
                    -> Hash 
                    -> Size
                    -> (HttpStatus -> Either e (bs, Size, HttpStatus)) 
                    -> (Hash -> Either e (bs, Size, HttpStatus)) 
                    -> m (Either e (bs, Size, HttpStatus))
downloadToFileHashed uri file expectedHash maxSize httpStatusNotOk hashMishmatch = liftIO $
    downloadToFileHashedGen uri file expectedHash maxSize httpStatusNotOk hashMishmatch 
        (\size status -> do             
            content <- readB file size
            pure (content, size, status))


downloadToFileHashedGen :: MonadIO m 
                        => URI 
                        -> FilePath 
                        -> Hash 
                        -> Size 
                        -> (HttpStatus -> Either e r) 
                        -> (Hash -> Either e r) 
                        -> (Size -> HttpStatus -> IO r) 
                        -> m (Either e r)
downloadToFileHashedGen uri file expectedHash maxSize httpStatusNotOk hashMishmatch result = liftIO $
    withFile file WriteMode $ \fd -> do 
        ((actualHash, size), status, _) <- 
                downloadConduit uri noValidators fd 
                    (sinkGenSize uri maxSize S256.init S256.update (U.mkHash . S256.finalize))
        if status /= mempty 
            then pure $! httpStatusNotOk status 
            else if actualHash /= expectedHash 
                then pure $! hashMishmatch actualHash
                else do
                    hClose fd                
                    Right <$> result size status


lazyFsRead :: FilePath -> IO LBS.ByteString
lazyFsRead = LBS.readFile

mapFile :: FilePath -> IO BS.ByteString
mapFile = unsafeMMapFile

data OversizedDownloadStream = OversizedDownloadStream URI Size Size 
    deriving stock (Eq, Ord, Generic)    

instance Exception OversizedDownloadStream
    

instance Show OversizedDownloadStream where 
    show (OversizedDownloadStream uri (Size actualSize) (Size maxSize)) = 
        [i|Http stream for #{uri} is #{actualSize} bytes, bigger than #{maxSize} bytes limit|]    


-- | Stream URL content to a file suing http conduit.
--
downloadConduit :: (MonadIO m, MonadUnliftIO m) => 
                    URI 
                    -> HttpValidators
                    -> Handle 
                    -> ConduitT BS.ByteString Void (ResourceT m) t
                    -> m (t, HttpStatus, HttpValidators)
downloadConduit uri validators fileHandle extraSink =
    downloadToSink uri validators $ fst <$> zipSinks extraSink (sinkHandle fileHandle)


-- | Download an object that is small enough to hold in memory, checking its
-- size and hash on the way, without touching the disk.
downloadHashedToMemory :: MonadIO m
                       => URI
                       -> Hash
                       -> Size
                       -> (HttpStatus -> e)
                       -> (Hash -> e)
                       -> m (Either e (Hashed BS.ByteString))
downloadHashedToMemory uri expectedHash maxSize httpStatusNotOk hashMismatch = liftIO $ do
    (((actualHash, _), body), status, _) <-
        downloadToSink uri noValidators $
            zipSinks (sinkGenSize uri maxSize S256.init S256.update (U.mkHash . S256.finalize))
                     sinkLazy
    pure $ if status /= mempty
        then Left $ httpStatusNotOk status
        else if actualHash /= expectedHash
            then Left $ hashMismatch actualHash
            else Right $! Hashed (LBS.toStrict body) actualHash


downloadToSink :: (MonadIO m, MonadUnliftIO m) =>
                  URI
                  -> HttpValidators
                  -> ConduitT BS.ByteString Void (ResourceT m) t
                  -> m (t, HttpStatus, HttpValidators)
downloadToSink (URI u) HttpValidators {..} sink = do
    req <- liftIO $ parseRequest $ Text.unpack u

    let eTagHeader = case eTag of 
            Nothing          -> []
            Just (ETag etag) -> [(hIfNoneMatch, etag)]

    let lastModifiedHeader = case lastModified of 
            Nothing                     -> []
            Just (LastModified lastMod) -> [(hIfModifiedSince, lastMod)]

    let newHeaders = 
            [(hUserAgent, userAgent)] <> eTagHeader <> lastModifiedHeader <> requestHeaders req

    let req' = req { 
            requestHeaders = newHeaders,
            -- Since the whole RRDP fetching process is limited in time
            -- it's fine (and sometimes necessary) to set some ridiculously 
            -- high timeout here (10 minutes).
            responseTimeout = responseTimeoutMicro 600_000_000 
        }

    httpStatus  <- liftIO $ newIORef mempty
    newValidators <- liftIO $ newIORef noValidators
    let getSrc r = do         
            liftIO $ do 
                writeIORef httpStatus $ HttpStatus $ getResponseStatusCode r         
                writeIORef newValidators $ HttpValidators {
                        eTag         = ETag <$> listToMaybe (getResponseHeader hETag r),
                        lastModified = LastModified <$> listToMaybe (getResponseHeader hLastModified r)
                    }
            getResponseBody r

    z <- runConduitRes
            $ httpSource req' getSrc
            .| countTraffic
            .| sink

    liftIO $ (z,,) <$> readIORef httpStatus <*> readIORef newValidators


-- | Add everything that goes through to the process-wide incoming traffic counter.
countTraffic :: MonadIO m => ConduitT BS.ByteString BS.ByteString m ()
countTraffic = awaitForever $ \chunk -> do 
    countIncomingTraffic $ BS.length chunk
    yield chunk

userAgent :: BS.ByteString
userAgent = U.convert rpkiProverVersion

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
                    then liftIO $ throwIO $ OversizedDownloadStream uri newSize maxAllowedSize
                    else loop $! T2 (updateValue ctx chunk) newSize
