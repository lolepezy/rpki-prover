{- | Listening on the addresses that the servers (RTR and HTTP API) are
   configured with.
-}
module RPKI.Listen (
    listenOn
) where

import           Control.Exception
import           Control.Monad

import           Data.Either                      (lefts, partitionEithers)
import           Data.Foldable                    (for_)
import           Data.List                        (nub)
import qualified Data.List                        as List
import           Data.String.Interpolate.IsString
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Word                        (Word16)

import           Foreign.C.Error                  (Errno (..), eADDRINUSE, eADDRNOTAVAIL)
import           GHC.IO.Exception                 (IOException (..))

import           Network.Socket

import           RPKI.Logging


{- | Listening sockets for every one of the addresses, each of them an IP
   address or a name. The server is what log messages start with.

   A name is listened on at all its addresses and not only at the first one:
   localhost is normally both 127.0.0.1 and ::1 and clients can use any of
   them. Some of them may not be usable, e.g. ::1 where IPv6 is disabled,
   which is fine as long as one of them is. An IP address is only itself,
   so it has to be usable: listening on fewer addresses than asked for would
   leave the clients that use the others without a server and nothing would
   tell why. If any of the addresses can't be listened on, it throws.

   An IPv6 address is only IPv6, also ::, which on Linux takes IPv4 connections
   too by default. Then :: and 0.0.0.0 would be the same port of the same
   addresses and can't be listened on together.
-}
listenOn :: Logger logger => logger -> Text -> [String] -> Word16 -> IO [Socket]
listenOn logger server addresses port = do
    resolved <- forM addresses $ \address ->
        (address,) <$> resolve address `catch` \(e :: IOException) -> do
            logError logger [i|#{server}: cannot resolve #{address}: #{e}|]
            throwIO e

    -- The same address may come from more than one of them, e.g. from
    -- localhost and 127.0.0.1, and it can only be listened on once.
    opened <- forM (nub $ concatMap snd resolved) $ \addr ->
        (addr,) <$> try @IOException (open addr)

    let sockets = [ sock | (_, Right sock) <- opened ]
        outcomes addrs = [ (addr, r) | addr <- addrs, Just r <- [lookup addr opened] ]
        unusable = [ (address, errors)
                   | (address, addrs) <- resolved
                   , (errors, []) <- [partitionEithers $ map snd $ outcomes addrs] ]

    case unusable of
        [] -> do
            -- Some addresses of a name may have failed, which is fine
            for_ opened $ \case
                (addr, Right _) -> logInfo logger [i|#{server}: listening on #{addrAddress addr}.|]
                (addr, Left e)  -> logWarn logger [i|#{server}: cannot listen on #{addrAddress addr}: #{e}|]
            pure sockets

        (unusableAddress, errors) : _ -> do
            mapM_ close sockets
            -- All of them, what worked too: two of them may be in conflict,
            -- or something else may be listening on some of them
            logError logger $ Text.intercalate "\n" $
                [i|#{server}: cannot listen on #{List.intercalate ", " (map fst unusable)}, not starting. |] <>
                [i|Port #{port} of every address given:|]
                : [ [i|    #{address}: |] <> Text.intercalate ", " (map describe $ outcomes addrs)
                  | (address, addrs) <- resolved ]
                <> errorHints (lefts $ map snd opened)
            throwIO $ case errors of
                e : _ -> e
                []    -> userError [i|No addresses for #{unusableAddress}|]
  where
    describe (addr, Right _) = [i|#{addrAddress addr} OK|]
    describe (addr, Left e)  = [i|#{addrAddress addr} failed (#{ioe_description e})|]

    errorHints errors =
        [ "Address in use: something else listens on the port at that address, e.g. another " <>
          "program, or it is one of the addresses above too: on Linux, an address and the " <>
          "wildcard of its family (0.0.0.0 or ::) can't both be listened on."
        | any (hasErrno eADDRINUSE) errors ] <>
        [ "Address not available: it is not an address of this host, or its interface is not up."
        | any (hasErrno eADDRNOTAVAIL) errors ]

    hasErrno (Errno errno) e = ioe_errno e == Just errno

    resolve address = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
            }
        nub <$> getAddrInfo (Just hints) (Just address) (Just $ show port)

    open addr =
        bracketOnError (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)) close $ \sock -> do
            setSocketOption sock ReuseAddr 1
            when (addrFamily addr == AF_INET6) $
                setSocketOption sock IPv6Only 1
            withFdSocket sock setCloseOnExecIfNeeded
            bind sock $ addrAddress addr
            listen sock 1024
            pure sock
