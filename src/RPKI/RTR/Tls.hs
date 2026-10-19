{-# LANGUAGE StrictData #-}

module RPKI.RTR.Tls (
    loadTlsCredential,
    tlsServerParams
) where

import           Control.Exception
import           Control.Monad.Trans.Except

import           Data.Bifunctor                   (first)
import           Data.String.Interpolate.IsString
import           Data.Text                        (Text)

import           Data.X509                        (CertificateChain (..))
import           Data.X509.File                   (readKeyFile, readSignedObject)

import qualified Network.TLS                      as TLS
import           Network.TLS.Extra.Cipher         (ciphersuite_strong)

import           RPKI.Config


-- | Read the certificate chain and the private key for RTR over TLS.
--
-- The rules are the same as in Routinator: the certificate file contains one
-- or more PEM certificates, the server certificate first and then the rest of
-- the chain; the key file contains exactly one PEM private key (PKCS#1 RSA,
-- PKCS#8 or SEC1 EC). Other PEM blocks in both files are ignored.
--
-- That the key belongs to the certificate is not checked here, a mismatch
-- shows up as failing handshakes.
loadTlsCredential :: RtrTlsConfig -> IO (Either Text TLS.Credential)
loadTlsCredential RtrTlsConfig {..} = runExceptT $ do
    certificates <- readPems "certificate" certPath readSignedObject
    case certificates of
        [] -> throwE [i|TLS certificate file '#{certPath}' does not contain any certificates.|]
        _  -> pure ()

    keys <- readPems "key" keyPath readKeyFile
    key <- case keys of
        []  -> throwE [i|TLS key file '#{keyPath}' does not contain any usable keys.|]
        [k] -> pure k
        _   -> throwE [i|TLS key file '#{keyPath}' contains multiple keys.|]

    pure (CertificateChain certificates, key)
  where
    certPath = configValue tlsCertFile
    keyPath  = configValue tlsKeyFile

    -- PEM parsing errors are thrown when the list is forced, force it here
    -- to report them as a broken file and not as a failure somewhere later.
    readPems :: String -> FilePath -> (FilePath -> IO [a]) -> ExceptT Text IO [a]
    readPems what path readFile_ =
        ExceptT $ first (\(e :: SomeException) -> [i|Failed to read TLS #{what} file '#{path}': #{e}|])
            <$> try (do
                    objects <- readFile_ path
                    _ <- evaluate $ length objects
                    pure objects)


-- | TLS 1.2 and 1.3 with the default strong ciphers and no client
-- certificates, which is what Routinator does as well.
tlsServerParams :: TLS.Credential -> TLS.ServerParams
tlsServerParams credential =
    TLS.defaultParamsServer {
        TLS.serverShared = TLS.defaultShared {
            TLS.sharedCredentials = TLS.Credentials [credential]
        },
        TLS.serverSupported = TLS.defaultSupported {
            -- The default is empty, i.e. no handshake would ever succeed
            TLS.supportedCiphers = ciphersuite_strong
        }
    }
