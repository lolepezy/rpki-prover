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
   IPv4 and IPv6 Prefix, Router Key and ASPA announcements.

   Every PDU starts with its protocol version and type and has its length in
   bytes 4-7, so the payloads are read PDU by PDU, and the ones of an unknown
   version and type are skipped. That is what keeps the files of every version
   of rpki-prover readable when the payloads change:

     * New payloads, or new layouts of the existing ones, come with new PDUs,
       i.e. with a new version and type. Older files are still read with the
       decoders in 'pduDecoders', which must therefore never be removed.

     * A file written before a payload type was known has no PDUs of it, and
       that is exactly what routers got from that version of rpki-prover, so
       the session carries on.

     * A file written by a newer rpki-prover may have PDUs that this one
       doesn't know. They are skipped, but routers may still have them, and
       diffs from the rest of the payloads would never withdraw them. So the
       payloads are served in a new session, see 'unknownPdus'.

     * The format version only changes with the header, or if a PDU that is
       already persisted changes its layout without changing its version and
       type, e.g. if version 2 changes before 8210bis is final. Then the
       decoders of the older format versions must stay too.

   test/data/rtr-state has a file of every format version, they must stay
   readable.

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

import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS
import           Data.Foldable                    (toList)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import qualified Data.Sequence                    as Seq
import qualified Data.Set                         as Set
import           Data.String.Interpolate.IsString
import           Data.Text                        (Text)
import           Data.Word                        (Word16, Word8)

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
        payloads     :: RtrPayloads,
        -- | PDUs skipped because there is no decoder for their version and
        -- type, i.e. the state was written by a newer rpki-prover. Routers
        -- may have them.
        unknownPdus  :: Int
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
    (payloads, unknownPdus) <- payloadsFromPdus uncompressed
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

-- | Decoders of the PDUs in persisted payloads by protocol version and PDU
-- type, each reading what follows these two bytes.
--
-- Never remove one, and if 'parseVersionedPdu' changes the layout of one of
-- them, keep the old layout here (see the module header).
pduDecoders :: Map (Word8, Word8) (Get Pdu)
pduDecoders = Map.fromList [ ((2, code), parseVersionedPdu V2 (PduCode code)) | code <- [4, 6, 9, 11] ]

-- | Payloads from the PDUs of a full Cache Response, and the number of PDUs
-- skipped because there is no decoder for them.
payloadsFromPdus :: BS.ByteString -> Either Text (RtrPayloads, Int)
payloadsFromPdus bytes =
    case runGetOrFail (go Seq.empty Seq.empty Seq.empty 0) (LBS.fromStrict bytes) of
        Left (_, offset, e) -> Left [i|Broken PDU in the RTR state at #{offset}: #{e}|]
        Right (_, _, (vrps, routerKeys, aspas, unknown)) ->
            Right (RtrPayloads {
                -- Only the unique ones matter for RTR
                vrps       = mempty,
                uniqueVrps = uniqVrpsPackedBy cmpPacked4 cmpPacked6 $ createVrps vrps,
                bgpSec     = Set.fromList $ toList routerKeys,
                aspas      = Set.fromList $ toList aspas
            }, unknown)
  where
    go !vrps !routerKeys !aspas !unknown =
        isEmpty >>= \case
            True  -> pure (vrps, routerKeys, aspas, unknown)
            False -> do
                (version, code, len) <- lookAhead $
                    (,,) <$> getWord8 <*> getWord8 <*> (skip 2 >> getWord32be)
                -- Every PDU has at least these 8 bytes
                when (len < 8) $ fail [i|wrong PDU length #{len}|]
                case Map.lookup (version, code) pduDecoders of
                    Nothing -> do
                        skip $ fromIntegral len
                        go vrps routerKeys aspas (unknown + 1)
                    Just decode ->
                        isolate (fromIntegral len) (skip 2 >> decode) >>= \case
                            IPv4PrefixPdu Announcement prefix asn maxLength ->
                                go (vrps Seq.|> Vrp asn (Ipv4P prefix) maxLength) routerKeys aspas unknown
                            IPv6PrefixPdu Announcement prefix asn maxLength ->
                                go (vrps Seq.|> Vrp asn (Ipv6P prefix) maxLength) routerKeys aspas unknown
                            RouterKeyPdu asn Announcement ski spki -> let
                                key = BGPSecPayload {
                                        bgpSecSki  = ski,
                                        bgpSecAsns = [asn],
                                        bgpSecSpki = SPKI $ encodeBase64 $ DecodedBase64 $ LBS.toStrict spki
                                    }
                                in go vrps (routerKeys Seq.|> key) aspas unknown
                            AspaPdu Announcement customer providers ->
                                go vrps routerKeys (aspas Seq.|> Aspa customer (Set.fromList providers)) unknown
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
