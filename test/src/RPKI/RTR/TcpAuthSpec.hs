{-# LANGUAGE NumericUnderscores #-}

module RPKI.RTR.TcpAuthSpec where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad

import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Char8             as BS8
import qualified Data.Text                         as Text

import           Network.Socket
import           Network.Socket.ByteString         (recv, sendAll)

import           RPKI.Config
import           RPKI.Resources.Resources
import           RPKI.Resources.Types
import           RPKI.RTR.TcpAuth

import           System.Timeout                    (timeout)

import           Test.Tasty
import qualified Test.Tasty.HUnit                  as HU


rtrTcpAuthGroup :: TestTree
rtrTcpAuthGroup = testGroup "RTR TCP authentication" [
        HU.testCase "Should parse keys" $
            HU.assertEqual "Keys" (Right [
                    md5 (Ipv4P $ readIp4 "192.0.2.1/32") "secret",
                    md5 (Ipv6P $ readIp6 "2001:db8::/48") "with  spaces # and a hash",
                    md5 (Ipv4P $ readIp4 "198.51.100.0/24") "x"
                ]) $
                parseTcpAuthKeys $ BS8.unlines [
                    "# A comment",
                    "",
                    "192.0.2.1 md5 secret",
                    "   2001:db8::/48\tmd5   with  spaces # and a hash  \r",
                    "198.51.100.0/24 md5 x"
                ],

        HU.testCase "Should accept a key of 80 bytes" $
            HU.assertEqual "Keys" (Right [md5 (Ipv4P $ readIp4 "192.0.2.1/32") (BS8.replicate 80 'k')]) $
                parseTcpAuthKeys $ "192.0.2.1 md5 " <> BS8.replicate 80 'k',

        HU.testCase "Should reject a key longer than 80 bytes" $
            assertFailsWith "line 1: the key is longer than 80 bytes" $
                parseTcpAuthKeys $ "192.0.2.1 md5 " <> BS8.replicate 81 'k',

        HU.testCase "Should reject an empty key" $
            assertFailsWith "line 2: the key is empty" $
                parseTcpAuthKeys "# comment\n192.0.2.1 md5   ",

        HU.testCase "Should reject a line without a method" $
            assertFailsWith "line 1: expected" $ parseTcpAuthKeys "192.0.2.1",

        HU.testCase "Should reject an unknown method" $
            assertFailsWith "line 1: unknown method 'ao'" $ parseTcpAuthKeys "192.0.2.1 ao secret",

        HU.testCase "Should reject an invalid address" $
            assertFailsWith "line 1: '192.0.2.300' is not an IP address or prefix" $
                parseTcpAuthKeys "192.0.2.300 md5 secret",

        HU.testCase "Should reject a second key for the same peer" $
            assertFailsWith "line 2: there is already a key for 192.0.2.1/32" $
                parseTcpAuthKeys "192.0.2.1 md5 one\n192.0.2.1/32 md5 two",

        HU.testCase "Should report a missing file" $
            loadTcpAuthKeys "test/data/missing-keys" >>=
                assertFailsWith "Failed to read TCP authentication keys file",

        HU.testCase "Should give the addresses of keys for a socket's family" $ do
            let v4 = Ipv4P $ readIp4 "192.0.2.0/24"
                v6 = Ipv6P $ readIp6 "2001:db8::/48"
            HU.assertEqual "IPv4 on an IPv4 socket"
                (Just $ KeyAddress AF_INET (BS.pack [192, 0, 2, 0]) 24)
                (keyAddress AF_INET v4)
            HU.assertEqual "IPv4 on an IPv6 socket"
                (Just $ KeyAddress AF_INET6 (BS.pack $ replicate 10 0 <> [0xff, 0xff, 192, 0, 2, 0]) 24)
                (keyAddress AF_INET6 v4)
            HU.assertEqual "IPv6 on an IPv6 socket"
                (Just $ KeyAddress AF_INET6 (BS.pack $ [0x20, 0x01, 0x0d, 0xb8] <> replicate 12 0) 48)
                (keyAddress AF_INET6 v6)
            HU.assertEqual "IPv6 on an IPv4 socket" Nothing (keyAddress AF_INET v6),

        HU.testCase "Should find the key of a peer by the longest prefix" $ do
            let wide   = md5 (Ipv4P $ readIp4 "192.0.2.0/24") "wide"
                narrow = md5 (Ipv4P $ readIp4 "192.0.2.128/25") "narrow"
                v6     = md5 (Ipv6P $ readIp6 "2001:db8::/48") "v6"
                keys   = [wide, narrow, v6]
            HU.assertEqual "IPv4" (Just narrow) $
                tcpAuthFor keys $ SockAddrInet 179 $ tupleToHostAddress (192, 0, 2, 200)
            HU.assertEqual "IPv4 in the wider prefix" (Just wide) $
                tcpAuthFor keys $ SockAddrInet 179 $ tupleToHostAddress (192, 0, 2, 1)
            HU.assertEqual "IPv4-mapped" (Just narrow) $
                tcpAuthFor keys $ SockAddrInet6 179 0 (0, 0, 0xffff, 0xc00002c8) 0
            HU.assertEqual "IPv6" (Just v6) $
                tcpAuthFor keys $ SockAddrInet6 179 0 (0x20010db8, 0x00000001, 0, 1) 0
            HU.assertEqual "No key" Nothing $
                tcpAuthFor keys $ SockAddrInet 179 $ tupleToHostAddress (198, 51, 100, 1),

        HU.testCase "Should only use IPsec SAs on OpenBSD" $ do
            auth <- loadTcpAuth TcpMd5IpsecKeys
            case tcpMd5Support of
                TcpMd5IpsecSAs -> HU.assertEqual "TCP auth" (Right IpsecTcpMd5) auth
                _              -> assertFailsWith "only supported on OpenBSD" auth,

        -- These need the kernel to take the keys, so only on Linux
        HU.testCase "Should only accept connections with the right TCP-MD5 key" $
            onlyIfSupported $ forM_ [AF_INET, AF_INET6] $ \family -> do
                let key = md5 (Ipv4P $ readIp4 "127.0.0.1/32") "secret"
                withEchoServer family [key] $ \port -> do
                    HU.assertEqual (describe family "with the key") (Just "ping") =<<
                        ping port [key]
                    HU.assertEqual (describe family "without a key") Nothing =<<
                        ping port []
                    HU.assertEqual (describe family "with a wrong key") Nothing =<<
                        ping port [md5 (Ipv4P $ readIp4 "127.0.0.1/32") "wrong"],

        HU.testCase "Should accept connections without a key from other peers" $
            onlyIfSupported $ forM_ [AF_INET, AF_INET6] $ \family ->
                withEchoServer family [md5 (Ipv4P $ readIp4 "192.0.2.1/32") "secret"] $ \port ->
                    HU.assertEqual (describe family "without a key") (Just "ping") =<< ping port []
    ]
  where
    md5 peer secret = TcpAuthKey peer (TcpMd5 $ TcpAuthSecret secret)

    assertFailsWith expected = \case
        Right _ -> HU.assertFailure "Should fail"
        Left e  -> HU.assertBool (Text.unpack e) $ expected `Text.isInfixOf` e

    describe family what = show family <> " server, client " <> what

    onlyIfSupported = when (tcpMd5Support == TcpMd5SocketKeys)


-- | A server on the loopback (or the IPv6 wildcard address, to get
-- IPv4-mapped connections) that echoes 4 bytes back.
withEchoServer :: Family -> [TcpAuthKey] -> (PortNumber -> IO a) -> IO a
withEchoServer family keys f =
    bracket open close $ \sock -> do
        port <- socketPort sock
        withAsync (serve sock) $ \_ -> f port
  where
    open = do
        sock <- socket family Stream defaultProtocol
        setSocketOption sock ReuseAddr 1
        bind sock $ if family == AF_INET
            then SockAddrInet 0 $ tupleToHostAddress (127, 0, 0, 1)
            else SockAddrInet6 0 0 (0, 0, 0, 0) 0
        installTcpAuthKeys sock family keys
        listen sock 16
        pure sock

    serve sock = forever $ do
        (conn, _) <- accept sock
        void $ async $ (`finally` close conn) $
            sendAll conn =<< recv conn 4

-- | Connect to the IPv4 loopback with the keys, Nothing means that the
-- connection was not established or nothing came back.
ping :: PortNumber -> [TcpAuthKey] -> IO (Maybe BS.ByteString)
ping port keys =
    timeout 1_000_000 $
        bracket (socket AF_INET Stream defaultProtocol) close $ \sock -> do
            installTcpAuthKeys sock AF_INET keys
            connect sock $ SockAddrInet port $ tupleToHostAddress (127, 0, 0, 1)
            sendAll sock "ping"
            recv sock 4
