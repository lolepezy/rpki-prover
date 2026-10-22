{-# LANGUAGE StrictData #-}

{- | The RTR state persisted outside of the cache, so that after a restart the
   RTR server can carry on with the same session and data right away, even
   if the cache is gone, e.g. after an upgrade that changes the database
   format. Routers then don't notice the restart at all.

   It is the session, the serial and the payloads at that serial, in a format
   that has nothing to do with the cache:

     * header, all numbers big-endian:
         - "RPKI-RTR" (8 bytes)
         - format version (Word16), 'snapshotFormatVersion'
         - session ID (Word16)
         - serial (Word32)
         - world version of the payloads (Int64, nanoseconds since the epoch)
         - when it was written (Int64, nanoseconds since the epoch)
     * payloads:
         - length of the PDUs (Word64)
         - SHA256 of the PDUs (32 bytes)
         - length of the LZ4-compressed PDUs (Word64)
         - LZ4-compressed PDUs

   The PDUs are what a Cache Response of protocol version 2 contains, i.e.
   IPv4 and IPv6 Prefix, Router Key and ASPA announcements, so the payloads
   are in a format that never changes. Its header, then, is the only thing
   that may need a new format version.

   Persisted state is only used if its payloads are younger than the Expire
   Interval: routers don't keep data longer than that either.
-}
module RPKI.RTR.Snapshot (
    RtrSnapshot (..),
    snapshotFileName,
    snapshotFormatVersion,
    persistedPayloads,
    freshEnough,
    encodeSnapshot,
    decodeSnapshot,
    writeSnapshot,
    readSnapshot,
    payloadsFromPdus
) where

import           Codec.Compression.LZ4            (compress, decompress)

import           Control.Exception
import           Control.Monad

import           Data.Binary                      (get)
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS
import           Data.Foldable                    (toList)
import qualified Data.Sequence                    as Seq
import qualified Data.Set                         as Set
import           Data.String.Interpolate.IsString
import           Data.Text                        (Text)
import           Data.Word                        (Word16)

import qualified Crypto.Hash.SHA256               as S256

import           System.Directory                 (doesFileExist, renameFile)
import           System.FilePath                  (takeDirectory)
import           System.IO
import           System.Posix.IO                  (OpenMode (..), closeFd, defaultFileFlags, openFd)
import           System.Posix.Unistd              (fileSynchronise)

import           RPKI.AppTypes
import           RPKI.Domain
import           RPKI.Resources.Types
import           RPKI.RTR.Pdus
import           RPKI.RTR.Protocol
import           RPKI.RTR.Types
import           RPKI.Time
import           RPKI.Util                        (encodeBase64)

import           Time.Types                       (Seconds)


data RtrSnapshot = RtrSnapshot {
        worldVersion :: WorldVersion,
        sessionId    :: RtrSessionId,
        serial       :: SerialNumber,
        writtenAt    :: Instant,
        payloads     :: RtrPayloads
    }
    deriving stock (Show, Eq)

-- | In the root directory, which is not touched when the cache is erased.
snapshotFileName :: FilePath
snapshotFileName = "rtr-state"

snapshotMagic :: BS.ByteString
snapshotMagic = "RPKI-RTR"

snapshotFormatVersion :: Word16
snapshotFormatVersion = 1

-- | Whether the payloads are younger than the Expire Interval, i.e. whether 
-- routers that got them before the restart may still have them.
freshEnough :: Seconds -> Instant -> RtrSnapshot -> Bool
freshEnough expireInterval now RtrSnapshot {..} = 
    closeEnoughMoments (Earlier $ Instant $ versionToInt worldVersion) (Later now) expireInterval

-- | The payloads part of the file for the PDUs of 'V2' payloads. It is
-- the same for every state with these payloads, so it's made once
-- (see 'ServedPayloads') and not every time the state is written.
persistedPayloads :: BS.ByteString -> BS.ByteString
persistedPayloads pdus =
    LBS.toStrict $ runPut $ do
        putWord64be $ fromIntegral $ BS.length pdus
        putByteString $ S256.hash pdus
        putWord64be $ fromIntegral $ BS.length compressed
        putByteString compressed
  where
    -- It only fails for inputs over 2GB
    compressed = maybe (error "Cannot compress RTR payloads") id $ compress pdus

encodeSnapshot :: Instant -> RtrState -> LBS.ByteString
encodeSnapshot (Instant writtenAt) RtrState {..} =
    runPut $ do
        putByteString snapshotMagic
        putWord16be snapshotFormatVersion
        let RtrSessionId s = currentSessionId
        putWord16be s
        let SerialNumber n = currentSerial
        putInt32be n
        putInt64be $ versionToInt lastKnownWorldVersion
        putInt64be writtenAt
        putByteString $ persisted served

decodeSnapshot :: LBS.ByteString -> Either Text RtrSnapshot
decodeSnapshot bytes = do
    (sessionId, serial, worldVersion, writtenAt, pdus, checksum, compressed) <-
        case runGetOrFail getSnapshot bytes of
            Left (_, _, e)       -> Left [i|Broken RTR state: #{e}|]
            Right (_, _, parsed) -> Right parsed
    uncompressed <- maybe (Left "Cannot decompress the payloads of the RTR state") Right $
                        decompress compressed
    unless (BS.length uncompressed == pdus && S256.hash uncompressed == checksum) $
        Left "Payloads of the RTR state don't match their checksum"
    payloads <- payloadsFromPdus uncompressed
    pure RtrSnapshot {..}
  where
    getSnapshot = do
        magic <- getByteString $ BS.length snapshotMagic
        unless (magic == snapshotMagic) $ fail "not an RTR state file"
        format <- getWord16be
        unless (format == snapshotFormatVersion) $
            fail [i|format version #{format}, only #{snapshotFormatVersion} is supported|]
        sessionId    <- RtrSessionId <$> getWord16be
        serial       <- SerialNumber <$> getInt32be
        worldVersion <- asVersion <$> getInt64be
        writtenAt    <- Instant <$> getInt64be
        pdus         <- fromIntegral <$> getWord64be
        checksum     <- getByteString 32
        compressed   <- getByteString . fromIntegral =<< getWord64be
        pure (sessionId, serial, worldVersion, writtenAt, pdus, checksum, compressed)

-- | Payloads from the PDUs of a full Cache Response of protocol version 2.
payloadsFromPdus :: BS.ByteString -> Either Text RtrPayloads
payloadsFromPdus bytes =
    case runGetOrFail (go Seq.empty Seq.empty Seq.empty) (LBS.fromStrict bytes) of
        Left (_, offset, e) -> Left [i|Broken PDU in the RTR state at #{offset}: #{e}|]
        Right (_, _, (vrps, routerKeys, aspas)) ->
            Right RtrPayloads {
                -- Only the unique ones matter for RTR
                vrps       = mempty,
                uniqueVrps = uniqVrpsPackedBy cmpPacked4 cmpPacked6 $ createVrps vrps,
                bgpSec     = Set.fromList $ toList routerKeys,
                aspas      = Set.fromList $ toList aspas
            }
  where
    go !vrps !routerKeys !aspas =
        isEmpty >>= \case
            True  -> pure (vrps, routerKeys, aspas)
            False -> do
                version <- getWord8
                unless (version == 2) $ fail [i|unexpected protocol version #{version}|]
                pdu <- parseVersionedPdu V2 =<< get
                case pdu of
                    IPv4PrefixPdu Announcement prefix asn maxLength ->
                        go (vrps Seq.|> Vrp asn (Ipv4P prefix) maxLength) routerKeys aspas
                    IPv6PrefixPdu Announcement prefix asn maxLength ->
                        go (vrps Seq.|> Vrp asn (Ipv6P prefix) maxLength) routerKeys aspas
                    RouterKeyPdu asn Announcement ski spki -> let
                        key = BGPSecPayload {
                                bgpSecSki  = ski,
                                bgpSecAsns = [asn],
                                bgpSecSpki = SPKI $ encodeBase64 $ DecodedBase64 $ LBS.toStrict spki
                            }
                        in go vrps (routerKeys Seq.|> key) aspas
                    AspaPdu Announcement customer providers ->
                        go vrps routerKeys (aspas Seq.|> Aspa customer (Set.fromList providers))
                    other ->
                        fail [i|unexpected PDU #{other}|]

-- | Write it so that the file is either the previous or the new state,
-- also if the system crashes in the middle.
writeSnapshot :: FilePath -> Instant -> RtrState -> IO ()
writeSnapshot path writtenAt rtrState = do
    let tmp = path <> ".tmp"
    withBinaryFile tmp WriteMode $ \h ->
        LBS.hPut h $ encodeSnapshot writtenAt rtrState
    syncPath tmp
    renameFile tmp path
    syncPath $ takeDirectory path
  where
    syncPath p = bracket (openFd p ReadOnly defaultFileFlags) closeFd fileSynchronise

-- | Nothing if there is no file.
readSnapshot :: FilePath -> IO (Maybe (Either Text RtrSnapshot))
readSnapshot path =
    doesFileExist path >>= \case
        False -> pure Nothing
        True  ->
            try (BS.readFile path) >>= \case
                Left (e :: SomeException) -> pure $ Just $ Left [i|Cannot read RTR state: #{e}|]
                Right bytes -> pure $ Just $ decodeSnapshot $ LBS.fromStrict bytes
