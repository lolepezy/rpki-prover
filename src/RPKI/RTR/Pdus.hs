{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}

module RPKI.RTR.Pdus where

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Text                as Text

import           Data.Binary
import           Data.Binary.Get          (getByteString, runGetOrFail)
import           Data.Binary.Put          (runPut, putLazyByteString, putByteString)

import           Data.Int

import           Control.Monad            (unless, when, replicateM)
import           RPKI.Domain              (toNormalBS, KI (..), SKI (..), skiLen, toShortBS)
import           RPKI.Resources.Resources
import           RPKI.Resources.Types
import           RPKI.RTR.Protocol
import           RPKI.Util
import Data.Text.Encoding (decodeUtf8, encodeUtf8)


errorPduType :: PduCode
errorPduType = PduCode 10

toPduCode :: Pdu -> PduCode 
toPduCode NotifyPdu {}        = PduCode 0
toPduCode SerialQueryPdu {}   = PduCode 1
toPduCode ResetQueryPdu       = PduCode 2
toPduCode CacheResponsePdu {} = PduCode 3
toPduCode IPv4PrefixPdu {}    = PduCode 4
toPduCode IPv6PrefixPdu {}    = PduCode 6
toPduCode EndOfDataPdu {}     = PduCode 7
toPduCode CacheResetPdu       = PduCode 8
toPduCode RouterKeyPdu {}     = PduCode 9     
toPduCode ErrorPdu {}         = PduCode 10    
toPduCode AspaPdu {}          = PduCode 11

-- | Value of the "length" field of a PDU, i.e. the total size of its 
-- serialised form. Must always agree with what 'pduToBytes' actually writes,
-- which is why both take the PDU and its version as one inseparable value.
-- 
pduLength :: VersionedPdu -> Word32 
pduLength (VersionedPdu pdu protocolVersion) = case pdu of     
    NotifyPdu {}        -> 12
    SerialQueryPdu {}   -> 12
    ResetQueryPdu       -> 8
    CacheResponsePdu {} -> 8
    IPv4PrefixPdu {}    -> 20
    IPv6PrefixPdu {}    -> 32
    CacheResetPdu       -> 8

    -- Intervals are only sent starting from V1
    EndOfDataPdu {} -> case protocolVersion of 
                            V0 -> 12
                            V1 -> 24
                            V2 -> 24

    AspaPdu _ _ providers -> 
        12 + 4 * fromIntegral (length providers)

    RouterKeyPdu _ _ ski spki -> 
        fromIntegral $ 12 + (fromIntegral (skiLen ski) :: Int64) + LBS.length spki

    ErrorPdu _ pduBytes errorMessage -> 
        fromIntegral $ 16 + 
                        maybe 0 LBS.length pduBytes + 
                        maybe 0 (fromIntegral . BS.length . encodeUtf8) errorMessage

-- 
-- | Serialise PDU into bytes according to the RTR protocal 
-- 
pduToBytes :: VersionedPdu -> LBS.ByteString
pduToBytes versionedPdu@(VersionedPdu pdu protocolVersion) = 
    runPut $ pduHeader >> pduContent
    where
        pduHeader = put protocolVersion >> put (toPduCode pdu)            

        pduContent = case pdu of 
            NotifyPdu sessionId serial ->         
                put sessionId >> put pduLen >> put serial

            SerialQueryPdu sessionId serial ->                
                put sessionId >> put pduLen >> put serial

            ResetQueryPdu -> 
                put (0 :: Word16) >> put pduLen
        
            CacheResponsePdu sessionId -> 
                put sessionId >> put pduLen

            IPv4PrefixPdu flags prefix asn maxLength -> do 
                put (0 :: Word16)
                put pduLen
                put flags
                put (ipv4PrefixLen prefix)
                put maxLength
                put (0 :: Word8)
                put prefix
                put asn                

            IPv6PrefixPdu flags prefix asn maxLength -> do 
                put (0 :: Word16)
                put pduLen
                put flags
                put (ipv6PrefixLen prefix)
                put maxLength
                put (0 :: Word8)
                put prefix
                put asn                

            EndOfDataPdu sessionId serial intervals -> do
                put sessionId
                put pduLen  
                put serial 
                case protocolVersion of 
                    V0 -> pure ()
                    V1 -> put intervals
                    V2 -> put intervals

            CacheResetPdu -> 
                put (0 :: Word16) >> put pduLen

            -- This is illegal to use for V0, but we are not going to complain here
            RouterKeyPdu (ASN asn') flags (SKI (KI ski)) spki -> do                
                put flags
                put (0 :: Word8)
                put pduLen
                putByteString $ toNormalBS ski
                put asn'
                putLazyByteString spki                

            -- This is illegal to use for V0 and V1, but we are not going to complain here
            AspaPdu flags customer providers -> do
                put flags
                put (0 :: Word8)
                put pduLen
                put customer
                mapM_ put providers

            ErrorPdu errorCode causingPdu errorText -> do                
                put errorCode
                put pduLen
                case causingPdu of
                    Nothing          -> put (0 :: Word32)
                    Just causingPdu' -> do 
                        put (fromIntegral (LBS.length causingPdu') :: Word32)
                        putLazyByteString causingPdu' 
                case errorText of
                    Nothing         -> put (0 :: Word32)
                    Just errorText' -> do 
                        let encodedError = encodeUtf8 errorText'
                        put (fromIntegral (BS.length encodedError) :: Word32)
                        putByteString encodedError
    
        pduLen = pduLength versionedPdu :: Word32


-- Decide with PDUs are to be sent to a connection supporting specific RTR version 
compatibleWith :: Pdu -> ProtocolVersion -> Bool

-- V0 doesn't support router keys
-- https://datatracker.ietf.org/doc/html/rfc6810#section-5
compatibleWith RouterKeyPdu{} version = version > V0

-- ASPA is only defined in V2
-- https://datatracker.ietf.org/doc/html/draft-ietf-sidrops-8210bis
compatibleWith AspaPdu{} version = version >= V2

compatibleWith _ _ = True

-- 
-- | Parse PDUs from bytestrings according to the RTR protocal 
-- 
bytesToVersionedPdu :: LBS.ByteString -> Either PduParseError VersionedPdu
bytesToVersionedPdu bs = 
    case runGetOrFail parsePduHeader bs of
        Left (_, _, errorMessage) -> 
            Left $ ParsedNothing InvalidRequest $ convert errorMessage

        Right (remainder, _, header@(PduHeader version pduType)) ->         
            case runGetOrFail (parseVersionedPdu version pduType) remainder of
                Left (_, _, errorMessage) -> 
                    Left $ ParsedOnlyHeader InvalidRequest 
                            (Text.pack $ "Error parsing of " <> show bs <> ", " <> errorMessage)
                            header
                Right (_, _, pdu) -> 
                    Right $ VersionedPdu pdu version            
    where
        parsePduHeader = PduHeader <$> get <*> get


-- | Parse the body of a PDU after header is parsed.
-- 
parseVersionedPdu :: ProtocolVersion -> PduCode -> Get Pdu
parseVersionedPdu protocolVersion pduType = 
    case pduType of 
        PduCode 0  -> do
            sessionId <- get
            len :: Int32  <- get
            assertLength len 12
            serial    <- get
            pure $ NotifyPdu sessionId serial

        PduCode 1  -> do 
            sessionId <- get
            len :: Int32  <- get
            assertLength len 12
            serial    <- get
            pure $ SerialQueryPdu sessionId serial                    

        PduCode 2  -> do 
            zero :: Word16 <- get 
            unless (zero == 0) $ fail "Field must be zero for ResetQueryPdu"
            len  :: Int32 <- get
            assertLength len 8                    
            pure ResetQueryPdu

        PduCode 3  -> do 
            sessionId :: RtrSessionId <- get
            len :: Int32              <- get
            assertLength len 8
            pure $ CacheResponsePdu sessionId

        PduCode 4  -> do 
            zero :: Word16 <- get 
            unless (zero == 0) $ fail "Field must be zero for IPv4PrefixPdu"
            len  :: Int32 <- get
            assertLength len 20
            flags <- get
            PrefixLength pLen <- get
            maxLen :: PrefixLength <- get
            zero1 :: Word8         <- get
            unless (zero1 == 0) $ fail "Field must be zero for IPv4PrefixPdu"
            w32 :: Word32 <- get            
            asn :: ASN    <- get            
            pure $ IPv4PrefixPdu flags (mkIpv4Block w32 pLen) asn maxLen

        PduCode 6 -> do 
            zero :: Word16 <- get 
            unless (zero == 0) $ fail "Field must be zero for IPv6PrefixPdu"
            len  :: Int32 <- get
            assertLength len 32
            flags <- get
            PrefixLength pLen <- get
            maxLen :: PrefixLength <- get
            zero1 :: Word8     <- get
            unless (zero1 == 0) $ fail "Field must be zero for IPv6PrefixPdu"
            words' <- (,,,) <$> get <*> get <*> get <*> get
            asn :: ASN <- get            
            pure $ IPv6PrefixPdu flags (mkIpv6Block words' pLen) asn maxLen
            

        PduCode 7 -> do
            sessionId      <- get
            len  :: Word32 <- get                    
            serial         <- get 
            case protocolVersion of 
                V0 -> do 
                    assertLength len 12
                    -- 1) It doesn't matter what we put into intervals, since it's 
                    -- not going to be used anyway in V0 protocol.
                    -- 2) Normally we are not even supposed to parse this type of PDUs                    
                    pure $ EndOfDataPdu sessionId serial defIntervals
                _ -> do                        
                    intervals <- get
                    assertLength len 24
                    pure $ EndOfDataPdu sessionId serial intervals

        PduCode 8 -> do 
            zero :: Word16 <- get 
            unless (zero == 0) $ fail "Field must be zero for ResetQueryPdu"
            len  :: Word32 <- get
            assertLength len 8
            pure CacheResetPdu

        PduCode 9 -> do 
            flags         <- get
            zero :: Word8 <- get
            unless (zero == 0) $ fail "Field must be zero for RouterKeyPdu"
            len  :: Word32 <- get
            ski <- getByteString 20
            asn' <- get
            let remainingBytes = len - 32
            spki <- getByteString $ fromIntegral remainingBytes

            assertLength len $ 12 + 
                    (fromIntegral (BS.length ski) :: Word32) + 
                    (fromIntegral (BS.length spki) :: Word32)
             
            pure $ RouterKeyPdu asn' flags 
                    (SKI (KI $ toShortBS ski)) 
                    (LBS.fromStrict spki)

        PduCode 11 -> do
            when (protocolVersion < V2) $ fail "ASPA PDU is only supported in V2"
            flags         <- get
            zero :: Word8 <- get
            unless (zero == 0) $ fail "Field must be zero for AspaPdu"
            len :: Word32 <- get
            customer      <- get
            unless (len >= 12 && (len - 12) `mod` 4 == 0) $ 
                fail $ "Wrong length " <> show len <> " for AspaPdu"
            providers :: [ASN] <- replicateM (fromIntegral ((len - 12) `div` 4)) get
            case flags of 
                Withdrawal -> 
                    unless (null providers) $ 
                        fail "Withdrawal AspaPdu must not have providers"
                Announcement -> do
                    when (null providers) $ 
                        fail "Announcement AspaPdu must have at least one provider"
                    when (length providers > 1 && ASN 0 `elem` providers) $ 
                        fail "AspaPdu with multiple providers must not contain AS0"
                    unless (and $ zipWith (<) providers (drop 1 providers)) $ 
                        fail "Providers in AspaPdu must be unique and in ascending order"
            pure $ AspaPdu flags customer providers

        PduCode 10 -> do 
            errorCode                    <- get
            fullLength :: Word32         <- get
            encapsulatedPduLen :: Word32 <- get                        

            encapsulatedPdu <- if encapsulatedPduLen == 0 
                then pure Nothing
                else Just <$> getByteString (fromIntegral encapsulatedPduLen)            

            textLen :: Word32 <- get
            encodedMessage <- if textLen == 0
                then pure Nothing
                else Just <$> getByteString (fromIntegral textLen)

            assertLength fullLength $ 16 + textLen + encapsulatedPduLen                    

            pure $ ErrorPdu errorCode 
                    (LBS.fromStrict <$> encapsulatedPdu) 
                    (decodeUtf8 <$> encodedMessage)

        PduCode n  -> fail $ "Invalid PDU type " <> show n

    where
        assertLength len mustBe =     
            unless (len == mustBe) $ 
                fail $ "Wrong length " <> show len <> ", must be " <> show mustBe

 
toVersioned :: Session -> Pdu -> VersionedPdu
toVersioned (Session protocolVersion) pdu = VersionedPdu pdu protocolVersion