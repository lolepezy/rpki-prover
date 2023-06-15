{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}

module RPKI.RRDP.Http where

import Control.Exception.Lifted
import Control.Lens
import Control.Monad.Except

import Conduit
import Data.Conduit.Internal (zipSinks)

import Data.Generics.Product.Typed

import Data.IORef.Lifted

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import           Data.Tuple.Strict

import GHC.Generics (Generic)

import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Simple (getResponseBody, getResponseStatusCode, getResponseHeader, httpSource)

import RPKI.AppContext
import RPKI.AppMonad
import RPKI.Config
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
                Config ->
                URI -> 
                Maybe ETag ->
                m (bs, Size, HttpStatus, Maybe ETag)
downloadToBS config uri@(URI u) eTag = liftIO $ do    
    let tmpFileName = U.convert $ U.normalizeUri u
    let tmpDir = config ^. #tmpDirectory
    withTempFile tmpDir tmpFileName $ \name fd -> do
        ((_, size), status, newETag) <- 
                downloadConduit uri eTag fd 
                    (sinkGenSize 
                        uri
                        (config ^. typed @RrdpConf . #maxSize)
                        ()
                        (\_ _ -> ()) 
                        id)
        hClose fd                    
        content <- readB name size
        pure (content, size, status, newETag)


-- | Do the same as `downloadToBS` but calculate sha256 hash of the data while 
-- streaming it to the file.
downloadHashedBS :: (Blob bs, MonadIO m) => 
                    Config ->
                    URI -> 
                    Maybe ETag ->
                    Hash -> 
                    (Hash -> Either e (bs, Size, HttpStatus, Maybe ETag)) ->
                    m (Either e (bs, Size, HttpStatus, Maybe ETag))
downloadHashedBS config uri@(URI u) eTag expectedHash hashMishmatch = liftIO $ do
    -- Download xml file to a temporary file and read it as a lazy bytestring 
    -- to minimize the heap. Snapshots can be pretty big, so we don't want 
    -- a spike in heap usage.
    let tmpFileName = U.convert $ U.normalizeUri u
    let tmpDir = config ^. #tmpDirectory  
    withTempFile tmpDir tmpFileName $ \name fd -> do
        ((actualHash, size), status, newETag) <- 
                downloadConduit uri eTag fd 
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
                pure $ Right (content, size, status, newETag)
                

-- | Fetch arbitrary file using the streaming implementation
-- 
fetchRpkiObject :: AppContext s ->
                RrdpURL ->             
                ValidatorT IO RpkiObject
fetchRpkiObject appContext uri = do
    (content, _, _, _) <- fromTry (RrdpE . CantDownloadFile . U.fmtEx) $
                            downloadToBS 
                            (appContext ^. typed @Config) 
                            (getURL uri) 
                            Nothing
                        
    vHoist $ readObject (RrdpU uri) content


downloadToFile :: MonadIO m => 
                URI 
                -> FilePath
                -> Size                
                -> m HttpStatus
downloadToFile uri file limit = liftIO $ do    
    fmap (\(_, s, _) -> s) $ withFile file WriteMode $ \fd -> do 
        downloadConduit uri Nothing fd 
            (sinkGenSize uri limit () (\_ _ -> ()) id) 


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
                    -> Maybe ETag
                    -> Handle 
                    -> ConduitT BS.ByteString Void (ResourceT m) t
                    -> m (t, HttpStatus, Maybe ETag)
downloadConduit (URI u) eTag fileHandle extraSink = do 
    req <- liftIO $ parseRequest $ Text.unpack u    

    let eTagHeader = case eTag of 
            Nothing          -> []
            Just (ETag etag) -> [(hIfNoneMatch, etag)]

    let newHeaders = 
            [(hUserAgent, userAgent)] <> eTagHeader <> requestHeaders req

    let req' = req { 
            requestHeaders = newHeaders,
            -- Since the whole RRDP fetching process is limited in time
            -- it's fine (and sometimes necessary) to set some ridiculously 
            -- high timeout here (10 minutes).
            responseTimeout = responseTimeoutMicro 600_000_000 
        }

    httpStatus  <- liftIO $ newIORef mempty
    newETag     <- liftIO $ newIORef Nothing
    let getSrc r = do         
            liftIO $ do 
                writeIORef httpStatus $ HttpStatus $ getResponseStatusCode r         
                case getResponseHeader "ETag" r of
                    []    -> pure ()
                    e : _ -> writeIORef newETag $ Just $ ETag e
            getResponseBody r

    (z, _) <- runConduitRes 
                    $ httpSource req' getSrc
                    .| zipSinks extraSink (sinkHandle fileHandle)    

    liftIO $ (z,,) <$> readIORef httpStatus <*> readIORef newETag

userAgent :: BS.ByteString
userAgent = U.convert getVersion

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
