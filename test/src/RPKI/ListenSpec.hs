module RPKI.ListenSpec where

import           Control.Exception                 (IOException, bracket, try)
import           Control.Monad

import           Data.Word                         (Word16)

import           Network.Socket

import           Test.Tasty
import qualified Test.Tasty.HUnit                  as HU

import           RPKI.Listen
import           RPKI.Logging                      (LogLevel (..), LogType (..), newLogConfig, withLogger)


listenSpec :: TestTree
listenSpec = testGroup "Listening on addresses" [
        HU.testCase "Listens on an IP address" $ do
            port <- freePort
            addresses <- withListening ["127.0.0.1"] port $ mapM getSocketName
            HU.assertEqual "addresses" [localhost4 port] addresses,

        HU.testCase "Listens once on an address that more than one of them have" $ do
            port <- freePort
            addresses <- withListening ["localhost", "127.0.0.1"] port $ mapM getSocketName
            HU.assertEqual "127.0.0.1" 1 (length $ filter (== localhost4 port) addresses),

        HU.testCase "Doesn't listen at all if an IP address can't be listened on" $ do
            port <- freePort
            -- TEST-NET-1, not an address of this host
            r <- try $ withListening ["127.0.0.1", "192.0.2.1"] port $ \_ -> pure ()
            case r of
                Left (_ :: IOException) -> pure ()
                Right _                 -> HU.assertFailure "Listened without 192.0.2.1"
            -- The socket for 127.0.0.1 is closed, so it can be listened on again
            addresses <- withListening ["127.0.0.1"] port $ mapM getSocketName
            HU.assertEqual "addresses" [localhost4 port] addresses,

        -- On Linux an IPv6 socket takes IPv4 connections too by default,
        -- so :: after 0.0.0.0 would be the same port of the same addresses
        HU.testCase "Listens on all the IPv4 and all the IPv6 addresses together" $ do
            port <- freePort
            ipv6Only <- withListening ["0.0.0.0", "::"] port $ \sockets -> do
                HU.assertEqual "sockets" 2 (length sockets)
                forM sockets $ \sock ->
                    getSocketName sock >>= \case
                        SockAddrInet6 {} -> Just <$> getSocketOption sock IPv6Only
                        _                -> pure Nothing
            HU.assertEqual "IPv6 only" [Just 1] (filter (/= Nothing) ipv6Only)
    ]
  where
    withListening :: [String] -> Word16 -> ([Socket] -> IO a) -> IO a
    withListening addresses port f =
        withLogger (newLogConfig ErrorL MainLog) $ \logger ->
            bracket (listenOn logger "test" addresses port) (mapM_ close) f

    -- A port that nothing listens on
    freePort = withListening ["127.0.0.1"] 0 $ mapM getSocketName >=> \case
            [SockAddrInet port _] -> pure $ fromIntegral port
            other                 -> fail $ "Not one IPv4 address: " <> show other

    localhost4 port = SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1))
