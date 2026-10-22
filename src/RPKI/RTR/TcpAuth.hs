{-# LANGUAGE StrictData #-}

{- | Authentication of TCP segments of RTR connections (RFC 6810, section 7).

   It sits below TLS or plain TCP and has nothing to do with them: the kernel
   signs and checks every segment of the connections from the routers that 
   have a key and drops the ones that are not signed correctly, so that such 
   a connection doesn't even reach 'accept' without the right key. Routers
   without a key connect as usual.

   Only TCP-MD5 (RFC 2385) is supported for now, see cbits/tcp_auth.c:

     * on Linux the keys are read from a file and set on the listening 
       socket per address prefix of the routers;
     * on OpenBSD the keys are the IPsec SAs that the system has, TCP-MD5 is 
       only turned on for the listening socket.
-}
module RPKI.RTR.TcpAuth (
    TcpAuth (..),
    TcpAuthKey (..),
    TcpAuthMethod (..),
    TcpAuthSecret (..),
    TcpMd5Support (..),
    KeyAddress (..),
    loadTcpAuth,
    installTcpAuth,
    connectionTcpAuth,
    describeTcpAuth,
    loadTcpAuthKeys,
    parseTcpAuthKeys,
    keyAddress,
    installTcpAuthKeys,
    tcpMd5Support,
    tcpAuthFor,
    tcpAuthMethodName
) where

import           Control.Exception
import           Control.Monad

import           Data.Bifunctor                   (first)
import           Data.Char                        (isSpace)
import           Data.Foldable                    (for_)
import           Data.List                        (maximumBy)
import           Data.Ord                         (comparing)
import           Data.String.Interpolate.IsString
import           Data.Text                        (Text)
import           Data.Word

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Builder          as BB
import qualified Data.ByteString.Char8            as BS8
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.Set                         as Set

import           Foreign.C.Error                  (throwErrnoIfMinus1_)
import           Foreign.C.Types
import           Foreign.Ptr

import           Network.Socket

import           RPKI.Config
import           RPKI.Resources.Resources
import           RPKI.Resources.Types


-- | Never shown, so that it doesn't end up in a log.
newtype TcpAuthSecret = TcpAuthSecret BS.ByteString
    deriving stock (Eq)

instance Show TcpAuthSecret where
    show _ = "<secret>"

-- | How the segments from a router are authenticated.
--
-- TCP-AO (RFC 5925) is supposed to become another constructor. It needs
-- more than a secret: the key ids, the MAC algorithm and possibly a few keys
-- per router to roll them over. Linux (6.7+) sets its keys on the listening
-- socket in the same way, with TCP_AO_ADD_KEY, and a socket can have both
-- kinds of keys, for different routers.
data TcpAuthMethod = TcpMd5 TcpAuthSecret
    deriving stock (Eq, Show)

-- | The authentication of the connections from routers in a prefix, a single
-- router is a /32 or a /128.
data TcpAuthKey = TcpAuthKey {
        peer   :: IpPrefix,
        method :: TcpAuthMethod
    }
    deriving stock (Eq, Show)

tcpAuthMethodName :: TcpAuthMethod -> Text
tcpAuthMethodName = \case
    TcpMd5 _ -> "TCP-MD5"

-- | What is set on a listening socket.
data TcpAuth 
    -- | Keys for the routers (Linux)
    = SocketKeys [TcpAuthKey]
    -- | TCP-MD5 is turned on, the keys are the IPsec SAs (OpenBSD)
    | IpsecTcpMd5
    deriving stock (Eq, Show)

-- | How TCP-MD5 can be used on this platform.
data TcpMd5Support = TcpMd5Unsupported | TcpMd5SocketKeys | TcpMd5IpsecSAs
    deriving stock (Eq, Show)

tcpMd5Support :: TcpMd5Support
tcpMd5Support = case c_tcpMd5Support of 
    -- Codes are defined in cbits/tcp_auth.c
    1 -> TcpMd5SocketKeys
    2 -> TcpMd5IpsecSAs
    _ -> TcpMd5Unsupported

-- | Also fails if it cannot be used on this platform, so that it's found
-- out at the start and not when the RTR server starts.
loadTcpAuth :: TcpAuthConfig -> IO (Either Text TcpAuth)
loadTcpAuth = \case 
    TcpAuthKeysFile path -> fmap SocketKeys <$> loadTcpAuthKeys (configValue path)
    TcpMd5IpsecKeys -> pure $ case tcpMd5Support of 
        TcpMd5IpsecSAs   -> Right IpsecTcpMd5
        TcpMd5SocketKeys -> Left "TCP-MD5 with IPsec SAs is only supported on OpenBSD, use a file with the keys instead."
        TcpMd5Unsupported -> Left "TCP-MD5 with IPsec SAs is only supported on OpenBSD."

-- | Set it on a listening socket of the family. It's better done before
-- 'listen', otherwise there's a moment when routers with keys can connect
-- without them.
installTcpAuth :: Socket -> Family -> TcpAuth -> IO ()
installTcpAuth sock family = \case 
    SocketKeys keys -> installTcpAuthKeys sock family keys
    IpsecTcpMd5     -> 
        withFdSocket sock $ \fd ->
            throwErrnoIfMinus1_ "Turning on TCP-MD5" $ c_enableTcpMd5 fd

-- | How an accepted connection is authenticated, if it is.
connectionTcpAuth :: TcpAuth -> Socket -> SockAddr -> IO (Maybe Text)
connectionTcpAuth auth conn peer = case auth of 
    -- The kernel doesn't let a router that has a key connect without it
    SocketKeys keys -> pure $ (\k -> tcpAuthMethodName k.method) <$> tcpAuthFor keys peer
    -- Only connections from routers with an SA have the flag. It's only for
    -- logging, so an error (the connection is already reset?) is a "no".
    IpsecTcpMd5 -> 
        withFdSocket conn $ \fd -> do
            on <- c_tcpMd5Enabled fd
            pure $ if on == 1 then Just "TCP-MD5" else Nothing

describeTcpAuth :: TcpAuth -> Text
describeTcpAuth = \case 
    SocketKeys keys -> [i|#{length keys} TCP authentication key(s)|]
    IpsecTcpMd5     -> "TCP-MD5 keys from the IPsec SAs"

-- | Read the keys from a file with lines like
--
-- > # Comment
-- > 192.0.2.1       md5  some secret
-- > 2001:db8::/48   md5  another secret
--
-- i.e. an address or a prefix of the routers, the method and the key, which
-- is the rest of the line without the whitespace around it.
--
-- Also fails if the keys cannot be used on this platform, so that it's found
-- out at the start and not when the RTR server starts.
loadTcpAuthKeys :: FilePath -> IO (Either Text [TcpAuthKey])
loadTcpAuthKeys path =
    try (BS.readFile path) >>= \case
        Left (e :: SomeException) ->
            pure $ Left [i|Failed to read TCP authentication keys file '#{path}': #{e}|]
        Right content -> pure $ do
            keys <- first (\e -> [i|Invalid TCP authentication keys file '#{path}': #{e}|]) $
                        parseTcpAuthKeys content
            when (null keys) $
                Left [i|TCP authentication keys file '#{path}' does not contain any keys.|]
            case tcpMd5Support of 
                TcpMd5SocketKeys  -> pure keys
                TcpMd5IpsecSAs    -> Left "TCP-MD5 keys from a file are only supported on Linux, on OpenBSD the keys are IPsec SAs."
                TcpMd5Unsupported -> Left "TCP-MD5 is only supported on Linux and OpenBSD."

parseTcpAuthKeys :: BS.ByteString -> Either Text [TcpAuthKey]
parseTcpAuthKeys content =
    reverse . snd <$> foldM parseLine (Set.empty, []) (zip [1 :: Int ..] $ BS8.lines content)
  where
    parseLine acc@(seen, keys) (lineNo, BS8.strip -> line)
        | BS.null line || "#" `BS.isPrefixOf` line = Right acc
        | otherwise = first (\e -> [i|line #{lineNo}: #{e}|]) $ do
            key <- parseKey line
            let peer = key.peer
            -- The kernel would silently replace the first one. With TCP-AO
            -- there can be more keys for a router, but with different ids.
            when (peer `Set.member` seen) $
                Left [i|there is already a key for #{peer}|]
            pure (Set.insert peer seen, key : keys)

    parseKey :: BS.ByteString -> Either Text TcpAuthKey
    parseKey line = do
        let (peerS, rest)    = token line
            (methodS, rest') = token rest
            secret           = BS8.strip rest'
        peer <- parsePeer $ BS8.unpack peerS
        method <- case methodS of
            "md5" -> TcpMd5 <$> md5Secret secret
            ""    -> Left "expected '<address or prefix> md5 <key>'"
            other -> Left [i|unknown method '#{BS8.unpack other}', the only supported one is 'md5'|]
        pure TcpAuthKey {..}

    token = BS8.break isSpace . BS8.dropWhile isSpace

    parsePeer s =
        maybe (Left [i|'#{s}' is not an IP address or prefix|]) Right $ parsePrefix withLength
      where
        withLength
            | '/' `elem` s = s
            | ':' `elem` s = s <> "/128"
            | otherwise    = s <> "/32"

    md5Secret secret
        | BS.null secret            = Left "the key is empty"
        | BS.length secret > maxLen = Left [i|the key is longer than #{maxLen} bytes|]
        | otherwise                 = Right $ TcpAuthSecret secret
      where
        -- TCP_MD5SIG_MAXKEYLEN
        maxLen = 80 :: Int


-- | The address of a key as the kernel wants it for a socket of the family,
-- in network byte order.
data KeyAddress = KeyAddress {
        family       :: Family,
        address      :: BS.ByteString,
        prefixLength :: Word8
    }
    deriving stock (Eq, Show)

-- | An IPv6 socket accepts IPv4 connections as well, with an IPv4-mapped
-- address of the peer, so IPv4 prefixes are mapped there. The prefix length
-- stays that of the IPv4 prefix. IPv6 prefixes are of no use on an IPv4 socket.
keyAddress :: Family -> IpPrefix -> Maybe KeyAddress
keyAddress family = \case
    Ipv4P (ipv4PrefixWords -> (w, len))
        | family == AF_INET  -> Just $ KeyAddress family (bytes $ BB.word32BE w) len
        | family == AF_INET6 -> Just $ KeyAddress family (bytes $ v4Mapped <> BB.word32BE w) len
    Ipv6P (ipv6PrefixWords -> (hi, lo, len))
        | family == AF_INET6 -> Just $ KeyAddress family (bytes $ BB.word64BE hi <> BB.word64BE lo) len
    _ -> Nothing
  where
    bytes = LBS.toStrict . BB.toLazyByteString
    v4Mapped = BB.word64BE 0 <> BB.word16BE 0 <> BB.word16BE 0xffff

-- | Set the keys on a socket of the family, see 'installTcpAuth'.
installTcpAuthKeys :: Socket -> Family -> [TcpAuthKey] -> IO ()
installTcpAuthKeys sock family keys =
    for_ keys $ \TcpAuthKey {..} ->
        for_ (keyAddress family peer) $ \KeyAddress {address, prefixLength} ->
            case method of
                TcpMd5 (TcpAuthSecret secret) ->
                    withFdSocket sock $ \fd ->
                    BS.useAsCStringLen address $ \(addressPtr, _) ->
                    BS.useAsCStringLen secret $ \(secretPtr, secretLen) ->
                        throwErrnoIfMinus1_ [i|Setting the TCP-MD5 key for #{peer}|] $
                            c_setTcpMd5Key fd (packFamily family) (castPtr addressPtr)
                                (fromIntegral prefixLength) (castPtr secretPtr) (fromIntegral secretLen)

-- | The key the kernel uses for the connection from the address: it's the one
-- with the longest prefix that contains the address.
tcpAuthFor :: [TcpAuthKey] -> SockAddr -> Maybe TcpAuthKey
tcpAuthFor keys sockAddr = do
    address <- peerAddress
    case filter (\k -> k.peer `containsAddress` address) keys of
        []       -> Nothing
        matching -> Just $ maximumBy (comparing $ \k -> prefixLen k.peer) matching
  where
    peerAddress = case sockAddr of
        SockAddrInet _ a -> Just $ ipv4 $ hostAddressToWord a
        SockAddrInet6 _ _ (a1, a2, a3, a4) _
            | (a1, a2, a3) == (0, 0, 0xffff) -> Just $ ipv4 a4
            | otherwise -> Just $ Ipv6P $ mkIpv6Prefix (joinWords a1 a2) (joinWords a3 a4) 128
        _ -> Nothing

    ipv4 w = Ipv4P $ mkIpv4Prefix w 32

    hostAddressToWord a =
        let (b1, b2, b3, b4) = hostAddressToTuple a
        in foldl (\w b -> w * 256 + fromIntegral b) 0 [b1, b2, b3, b4]

    containsAddress (Ipv4P p) (Ipv4P a) = p `contains` a
    containsAddress (Ipv6P p) (Ipv6P a) = p `contains` a
    containsAddress _ _                 = False


foreign import ccall unsafe "rpki_prover_tcp_md5_support"
    c_tcpMd5Support :: CInt

foreign import ccall unsafe "rpki_prover_enable_tcp_md5"
    c_enableTcpMd5 :: CInt -> IO CInt

foreign import ccall unsafe "rpki_prover_tcp_md5_enabled"
    c_tcpMd5Enabled :: CInt -> IO CInt

foreign import ccall unsafe "rpki_prover_set_tcp_md5_key"
    c_setTcpMd5Key :: CInt -> CInt -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO CInt
