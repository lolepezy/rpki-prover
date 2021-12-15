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
import Control.Concurrent.STM.TBQueue
import Control.Exception.Lifted
import Control.Lens ((^.))
import Control.Monad.Except

import Conduit hiding (connect)
import Data.Bifunctor
import Data.Foldable

import Data.Conduit.Internal (zipSinks)

import Data.Generics.Product.Typed

import           Data.List.NonEmpty       (NonEmpty(..))
import qualified Data.List.NonEmpty       as NonEmpty

import Data.IORef.Lifted

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
import RPKI.Parallel
import RPKI.Reporting
import qualified RPKI.Util as U

import qualified Crypto.Hash.SHA256 as S256

import System.IO (Handle, hClose, withFile, IOMode(..))
import System.IO.Temp (withTempFile)
import System.Timeout (timeout)

import System.IO.Posix.MMap (unsafeMMapFile)

import Data.Version
import qualified Paths_rpki_prover as Autogen
import Data.List (sortOn)
import Data.Ord


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



newtype Port = Port Word16

newtype TextualIP = TextualIP BS.ByteString
    deriving Show

data ResolvedIP = ResolvedV4 IP.IPv4 | ResolvedV6 IP.IPv6 
    deriving Show


-- Not really a happy eye balls (https://datatracker.ietf.org/doc/html/rfc8305), 
-- but some approximation of it.
-- 
resolveIP :: HostName -> ServiceName -> IO (Either NetworkError ResolvedIP)
resolveIP hostName serviceName = do     
    addresses <- getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just hostName) (Just serviceName)

    -- sort them IPv6 first, AF_INET is smaller than AF_INET6
    let ipv6first = sortOn (Down . addrFamily) $ 
                    filter ((\f -> f == AF_INET || f == AF_INET6) . addrFamily) addresses
    pure $ Left $ NetworkError $ Text.pack $ show ipv6first
  where
    tryConnecting AddrInfo {..} = do             
        z <- try $ do 
                sock <- socket addrFamily Stream addrProtocol  
                connect sock addrAddress             
                    `finally`
                    close sock
        pure $ case z of 
            Left (e :: SomeException) -> Just $ NetworkError $ U.fmt e            
            Right _                   -> Nothing  


-- Pretty ad hoc implemntation of Happy Eeyballs algorithm
-- https://datatracker.ietf.org/doc/html/rfc8305
-- 
happyEyeballsResolve :: Resolver -> Domain -> PortNumber -> IO (Either NetworkError ResolvedIP)
happyEyeballsResolve resolver domain port = do     
    
    addressQ <- newCQueueIO 100    

    let writeAddresses = 
            getAddresses >>= \case 
                Left e -> pure $ Left e
                Right addresses -> do 
                    forM_ addresses $ \a -> do 
                        putStrLn $ "address = " <> show a
                        atomically $ writeCQueue addressQ a
                    pure $ Right addresses

    z <- withSocketsDo $ 
            concurrently 
                (writeAddresses `finally` atomically (closeCQueue addressQ))
                (findAvailableAddress addressQ)

    pure $ case z of 
        (Left e, _)        -> Left e
        (Right _, Nothing) -> Left $ NetworkError $ "Couldn't find available IP for port " <> U.fmt port
        (Right _, Just ip) -> Right ip

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

        findAvailableAddress q = go 
          where
            go = do 
                atomically (readCQueue q) >>= \case
                    Nothing -> pure Nothing
                    Just a  -> do 
                        checkConnectivity a port >>= \case 
                            Nothing -> pure $ Just a
                            Just e  -> go

-- Try to connect a socket to the given port
checkConnectivity :: ResolvedIP -> PortNumber -> IO (Maybe NetworkError)
checkConnectivity ip port = do 
    let (socketAddr, protocol, protocolNumber) = 
            case ip of
                ResolvedV4 ipv4 -> 
                    (SockAddrInet port (IP.toHostAddress ipv4), AF_INET, 6)
                -- TODO Figure out proper values for flow info and scope id
                ResolvedV6 ipv6 -> 
                    (SockAddrInet6 port (1 :: FlowInfo) (IP.toHostAddress6 ipv6) (2 :: ScopeID), AF_INET6, 6)
    
    let hints = defaultHints { 
                addrSocketType = Stream,
                addrAddress = socketAddr 
            }

    sock <- socket protocol Stream protocolNumber   
    z <- try 
        $ timeout 1_000_000 
        $ connect sock socketAddr             
            `finally`
            close sock

    pure $ case z of 
        Left (e :: SomeException) -> Just $ NetworkError $ U.fmt e
        Right Nothing             -> Just $ NetworkError "Timed out"
        Right (Just _)            -> Nothing    

                            


-- happyEyeballsResolve1 :: Resolver -> Domain -> Port -> IO (Either NetworkError [TextualIP])
-- happyEyeballsResolve1 :: Resolver -> Domain -> PortNumber -> IO (Either NetworkError [TextualIP])
happyEyeballsResolve1 resolver domain port = do     
    
    addressQ <- newCQueueIO 100    

    let writeAddresses = 
            getAddresses >>= \case 
                Left e -> pure $ Left e
                Right addresses -> do 
                    forM_ addresses $ \a -> atomically $ writeCQueue addressQ a
                    pure $ Right addresses

    withSocketsDo $ 
        concurrently 
            (writeAddresses `finally` atomically (closeCQueue addressQ))
            (findAvailableAddress addressQ)

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

        findAvailableAddress q = go 
          where
            go = do 
                atomically (readCQueue q) >>= \case
                    Nothing -> pure Nothing
                    Just a  -> do 
                        checkConnectivity a >>= \case 
                            Nothing -> pure $ Just a
                            Just e  -> go


        checkConnectivity :: ResolvedIP -> IO (Maybe NetworkError)
        checkConnectivity ip = do 
            putStrLn $ "zzzzzzzz111111"
            let (socketAddr, protocol, protocolNumber) = 
                    case ip of
                        ResolvedV4 ipv4 -> 
                            (SockAddrInet port (IP.toHostAddress ipv4), AF_INET, 4)
                        -- TODO Figuire out proper values for flow info and scope id
                        ResolvedV6 ipv6 -> 
                            (SockAddrInet6 port (1 :: FlowInfo) (IP.toHostAddress6 ipv6) (2 :: ScopeID), AF_INET6, 6)

            let hints = defaultHints { 
                        addrSocketType = Stream,
                        addrAddress = socketAddr 
                    }           
            putStrLn $ "zzzzzzzz"
            z <- getAddrInfo (Just hints) (Just "147.28.0.47") (Just "443")
            putStrLn $ "z = " <> show z
    
            -- sock <- socket protocol Stream protocolNumber
            -- z <- try $ timeout 1000_000 $ connect sock socketAddr            
            -- pure $ case z of 
            --     Left (e :: SomeException) -> Just $ NetworkError $ U.fmt e
            --     Right Nothing             -> Just $ NetworkError "Timed out"
            --     Right (Just _)            -> Nothing

            pure Nothing