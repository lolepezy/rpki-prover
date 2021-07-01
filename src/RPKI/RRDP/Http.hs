{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}

module RPKI.RRDP.Http where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception.Lifted
import Control.Lens ((^.))
import Control.Monad.Except

import Conduit
import Data.Bifunctor

import Data.Conduit.Internal (zipSinks)

import Data.Generics.Product.Typed

import           Data.List.NonEmpty       (NonEmpty(..))
import qualified Data.List.NonEmpty       as NonEmpty

import Data.IORef.Lifted

import Data.Bifunctor (first)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import           Data.Tuple.Strict

import qualified Data.IP as IP
import Data.Word

import GHC.Generics (Generic)

import Network.DNS.Resolver
import Network.DNS.Lookup
import Network.DNS.Types

import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Simple (getResponseBody, getResponseStatusCode, httpSource)

import Network.Socket

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
                content <- readB name
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
downloadConduit (URI u) fileHandle sink = do 
    req <- liftIO $ parseRequest $ Text.unpack u    
    let req' = req { requestHeaders = (hUserAgent, userAgent) : requestHeaders req }
    status <- liftIO $ newIORef mempty
    let getSrc r = do         
            liftIO $ writeIORef status $ HttpStatus $ getResponseStatusCode r         
            getResponseBody r

    (z, _) <- runConduitRes 
                    $ httpSource req' getSrc
                    .| zipSinks sink (sinkHandle fileHandle)    

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



newtype Port = Port Word16

newtype TextualIP = TextualIP BS.ByteString

data ResolvedIP = ResolvedV4 IP.IPv4 | ResolvedV6 IP.IPv6 

happyEyeballsResolve :: Resolver -> Domain -> Port -> IO (Either NetworkError [TextualIP])
happyEyeballsResolve resolver domain port = do     
    
    addresses <- getAddresses
    
    z <- race
            -- give IPv6 the speed advantage
            getV6
            (getV4 >>= \ipv4 -> threadDelay resolutionDelay >> pure ipv4)            


    pure $ Left $ NetworkError ""

    where
        getV6 = getVX lookupAAAA
        getV4 = getVX lookupA

        getVX :: (Resolver -> Domain -> IO (Either DNSError [a])) -> IO (Either NetworkError [a])
        getVX lookupX = do 
            z <- try $ lookupX resolver domain
            case z of 
                Left (e :: SomeException) -> pure $ Left $ NetworkError $ U.fmt e
                Right (Left e)            -> pure $ Left $ NetworkError $ U.fmt e                
                Right (Right q)           -> pure $ Right q


        resolutionDelay = 50_000

        getAddresses :: IO (Either NetworkError [ResolvedIP])
        getAddresses = 
            withAsync getV6 $ \v6a -> do 
                withAsync getV4 $ \v4a -> do 
                    z <- atomically $ 
                            waitSTM (Left <$> v6a) 
                                `orElse` 
                            waitSTM (Right <$> v4a)

                    case z of 
                        Left (Right v6s) -> 
                            pure $ Right $ map ResolvedV6 v6s

                        -- IPv6 failed, wait for v4 then
                        Left (Left e) ->
                            second (map ResolvedV4) <$> wait v4a                            

                        Right v4 -> do 
                            -- give ipv6 a little more time
                            threadDelay resolutionDelay
                            poll v6a >>= \case 
                                Nothing -> do 
                                    cancel v6a
                                    pure $ map ResolvedV4 <$> v4

                                Just (Left e) -> 
                                    pure $ Left $ NetworkError $ U.fmt e
                                    
                                Just (Right s) -> 
                                    pure $ map ResolvedV6 <$> s                   


-- happyEyeballsResolve1 :: Resolver -> Domain -> Port -> IO (Either NetworkError [TextualIP])
happyEyeballsResolve1 resolver domain port = do     
    
    let resolutionDelay = 50_000
    race
            -- give IPv6 the speed advantage
            getV6
            (getV4 >>= \ipv4 -> threadDelay resolutionDelay >> pure ipv4)            

    where
        getV6 = getVX lookupAAAA
        getV4 = getVX lookupA

        getVX :: (Resolver -> Domain -> IO (Either DNSError [a])) -> IO (Either NetworkError [a])
        getVX lookupX = do 
            z <- try $ lookupX resolver domain
            case z of 
                Left (e :: SomeException) -> pure $ Left $ NetworkError $ U.fmt e
                Right (Left e)            -> pure $ Left $ NetworkError $ U.fmt e                
                Right (Right q)           -> pure $ Right q                