module RPKI.RTR.TlsSpec where

import           Control.Concurrent.Async
import           Control.Monad

import qualified Data.ByteString                   as BS
import           Data.Either                       (isRight)
import qualified Data.Text                         as Text

import           Data.X509                         (CertificateChain (..))

import           Network.Socket
import qualified Network.TLS                       as TLS
import           Network.TLS.Extra.Cipher          (ciphersuite_strong)

import           RPKI.Config
import           RPKI.RTR.RtrServer                (RtrTransport (..), tlsTransport)
import           RPKI.RTR.Tls

import           Test.Tasty
import qualified Test.Tasty.HUnit                  as HU


rtrTlsGroup :: TestTree
rtrTlsGroup = testGroup "RTR over TLS" [
        HU.testCase "Should load RSA PKCS#8 key with a certificate chain" $ do
            credential <- load "rsa-chain.pem" "rsa-pkcs8.key"
            case credential of
                Right (CertificateChain chain, _) ->
                    HU.assertEqual "Server and CA certificates" 2 (length chain)
                Left e -> HU.assertFailure $ Text.unpack e,

        HU.testCase "Should load RSA PKCS#1 key" $
            assertLoads =<< load "rsa-chain.pem" "rsa-pkcs1.key",

        HU.testCase "Should load SEC1 EC key" $
            assertLoads =<< load "ec.pem" "ec-sec1.key",

        HU.testCase "Should reject a key file with multiple keys" $
            assertFailsWith "contains multiple keys" =<< load "rsa-chain.pem" "two-keys.key",

        HU.testCase "Should reject a key file without keys" $
            assertFailsWith "does not contain any usable keys" =<< load "rsa-chain.pem" "ec.pem",

        HU.testCase "Should reject a certificate file without certificates" $
            assertFailsWith "does not contain any certificates" =<< load "ec-sec1.key" "ec-sec1.key",

        HU.testCase "Should report a missing file" $
            assertFailsWith "Failed to read TLS certificate file" =<< load "missing.pem" "ec-sec1.key",

        HU.testCase "Should talk TLS with an RSA certificate" $
            roundTrip "rsa-chain.pem" "rsa-pkcs8.key",

        HU.testCase "Should talk TLS with an EC certificate" $
            roundTrip "ec.pem" "ec-sec1.key"
    ]
  where
    assertLoads = HU.assertBool "Credential should be loaded" . isRight

    assertFailsWith expected = \case
        Right _ -> HU.assertFailure "Credential should not be loaded"
        Left e  -> HU.assertBool (Text.unpack e) $ expected `Text.isInfixOf` e


load :: FilePath -> FilePath -> IO (Either Text.Text TLS.Credential)
load cert key = loadTlsCredential $ tlsConfig cert key

tlsConfig :: FilePath -> FilePath -> RtrTlsConfig
tlsConfig cert key = RtrTlsConfig {
        tlsListen   = RtrListen "localhost" 0,
        tlsCertFile = Public $ "test/data/rtr-tls/" <> cert,
        tlsKeyFile  = Public $ "test/data/rtr-tls/" <> key
    }

-- | Serve one connection with 'tlsServerParams' and 'tlsTransport'
-- and check that bytes go through in both directions.
roundTrip :: FilePath -> FilePath -> IO ()
roundTrip cert key = do
    Right credential <- load cert key
    (serverSock, clientSock) <- socketPair AF_UNIX Stream defaultProtocol

    let pdus = [BS.replicate 20 1, BS.replicate 12 2, BS.replicate 70000 3]
    let query = BS.pack [2, 2, 0, 0, 0, 0, 0, 8]

    let server = do
            ctx <- TLS.contextNew serverSock (tlsServerParams credential)
            TLS.handshake ctx
            let transport = tlsTransport ctx
            received <- transportRecv transport
            transportSend transport pdus
            -- The client closes the connection after reading everything
            closed <- transportRecv transport
            pure (received, closed)

    let client = do
            ctx <- TLS.contextNew clientSock clientParams
            TLS.handshake ctx
            TLS.sendData ctx $ BS.fromStrict query
            received <- recvExactly ctx (sum $ map BS.length pdus)
            TLS.bye ctx
            pure received

    ((serverReceived, serverClosed), clientReceived) <- concurrently server client
    mapM_ close [serverSock, clientSock]

    HU.assertEqual "Server should get the query" query serverReceived
    HU.assertEqual "Client should get all PDUs" (BS.concat pdus) clientReceived
    HU.assertEqual "Server should see the connection closed" BS.empty serverClosed
  where
    recvExactly ctx n = go [] 0
      where
        go acc len
            | len >= n  = pure $ BS.concat $ reverse acc
            | otherwise = do
                chunk <- TLS.recvData ctx
                when (BS.null chunk) $ fail "Connection closed too early"
                go (chunk : acc) (len + BS.length chunk)

    -- The test certificates are not trusted by anything, skip validation
    clientParams = (TLS.defaultParamsClient "localhost" "") {
            TLS.clientShared = TLS.defaultShared {
                TLS.sharedValidationCache = TLS.ValidationCache
                    (\_ _ _ -> pure TLS.ValidationCachePass)
                    (\_ _ _ -> pure ())
            },
            TLS.clientSupported = TLS.defaultSupported {
                TLS.supportedCiphers = ciphersuite_strong
            }
        }
