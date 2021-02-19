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

import System.IO (Handle, hClose)
import qualified System.IO.Posix.MMap as Mmap
import qualified System.IO.Posix.MMap.Lazy as MmapLazy
import System.IO.Temp (withTempFile)

import RPKI.CommonTypes

import Data.Version
import qualified Paths_rpki_prover as Autogen


class Blob bs where    
    readB :: FilePath -> IO bs
    sizeB :: bs -> Size
instance Blob LBS.ByteString where    
    readB = lazyFsRead
    sizeB = Size . fromIntegral . LBS.length 
instance Blob BS.ByteString where    
    readB = fsRead
    sizeB = Size . fromIntegral . BS.length 


-- | Download HTTP content to a temporary file and return lazy/strict ByteString content 
-- mapped onto a temporary file. Snapshots (and probably some deltas) can be big 
-- enough so that we don't want to keep them in memory.
--
-- NOTE: The file will be deleted from the temporary directory by withTempFile, 
-- but the descriptor taken by mmap/readFile will stay until the byte string it GC-ed, 
-- so it's safe to use them after returning from this function.
--
downloadToBS :: (Blob bs, MonadIO m) => 
                Config ->
                URI -> 
                m (bs, Size, HttpStatus)
downloadToBS config uri@(URI u) = liftIO $ do
    -- Download xml file to a temporary file and MMAP it to a lazy bytestring 
    -- to minimize the heap. Snapshots can be pretty big, so we don't want 
    -- a spike in heap usage.
    let tmpFileName = U.convert $ U.normalizeUri u
    let tmpDir = config ^. #tmpDirectory
    withTempFile tmpDir tmpFileName $ \name fd -> do
        ((_, size), status) <- 
                downloadConduit uri fd 
                    (sinkGenSize 
                        (config ^. typed @RrdpConf . #maxSize)
                        ()
                        (\_ _ -> ()) 
                        id)
        hClose fd                    
        content <- readB name
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
    -- Download xml file to a temporary file and MMAP it to a lazy bytestring 
    -- to minimize the heap. Snapshots can be pretty big, so we don't want 
    -- a spike in heap usage.
    let tmpFileName = U.convert $ U.normalizeUri u
    let tmpDir = config ^. #tmpDirectory  
    withTempFile tmpDir tmpFileName $ \name fd -> do
        ((actualHash, size), status) <- 
                downloadConduit uri fd 
                    (sinkGenSize 
                        (config ^. typed @RrdpConf . #maxSize)
                        S256.init
                        S256.update
                        (U.mkHash . S256.finalize))
        if actualHash /= expectedHash 
            then pure $! hashMishmatch actualHash
            else do
                hClose fd
                content <- readB name
                pure $! Right (content, size, status)
                

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


lazyFsRead :: FilePath -> IO LBS.ByteString
lazyFsRead = MmapLazy.unsafeMMapFile 
-- lazyFsRead = LBS.readFile

fsRead :: FilePath -> IO BS.ByteString
fsRead = Mmap.unsafeMMapFile 
-- fsRead = BS.readFile 

newtype OversizedDownloadStream = OversizedDownloadStream Size
    deriving stock (Show, Eq, Ord, Generic)    

instance Exception OversizedDownloadStream
    

-- | Stream URL content to a file suing http conduit.
--
downloadConduit :: (MonadIO m, MonadUnliftIO m) => 
                    URI 
                    -> Handle 
                    -> ConduitT BS.ByteString Void (ResourceT m) t
                    -> m (t, HttpStatus)
downloadConduit (URI u) fileHandle sink = do 
    req <- liftIO $ parseRequest $ Text.unpack u    
    let req' = req { requestHeaders = (hUserAgent, userAgent) : (requestHeaders req) }
    status <- liftIO $ newIORef mempty
    let getSrc r = do         
            liftIO $ writeIORef status $ HttpStatus $ getResponseStatusCode r         
            getResponseBody r

    (z, _) <- runConduitRes 
                    $ httpSource req' getSrc
                    .| zipSinks sink (sinkHandle fileHandle)    

    (z,) <$> liftIO (readIORef status)

userAgent :: BS.ByteString
userAgent = U.convert $ "rpki-prover-" <> (showVersion Autogen.version)

-- | Calculate size and extra arbitrary function of the sinked stream
-- | and throw and exception is the size gets too big.
-- 
sinkGenSize :: MonadIO m => 
            Size             
            -> h
            -> (h -> BS.ByteString -> h)
            -> (h -> a)
            -> ConduitT BS.ByteString o m (a, Size)
sinkGenSize maxAllowedSize initValue updateValue finalValue = do 
    T2 h s <- loop $ T2 initValue 0
    pure (h, s)
  where    
    loop (T2 ctx s) =
        await >>= \case
            Nothing -> pure $! T2 (finalValue ctx) s
            Just chunk -> let 
                !newSize = s + (fromIntegral (BS.length chunk) :: Size)                
                in if newSize > maxAllowedSize
                    then liftIO $ throwIO $ OversizedDownloadStream newSize
                    else loop $! T2 (updateValue ctx chunk) newSize
