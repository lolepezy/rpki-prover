{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}

module RPKI.RTR.Pdus where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Text
import qualified Data.Text            as Text

import           Data.Binary
import           Data.Binary.Get      (getByteString, getRemainingLazyByteString, runGetOrFail)
import           Data.Binary.Put      (runPut)

import           Data.Int
import           GHC.TypeLits

import           Control.Monad        (unless)
import           Data.Data
import           RPKI.Domain          (KI (..), SKI (..), skiLen, toShortBS)
import           RPKI.Resources.Types
import           RPKI.RTR.Types
import Data.Hex
import RPKI.Resources.Resources



toPduCode :: Pdu -> Word8 
toPduCode NotifyPdu {}        = 0
toPduCode SerialQueryPdu {}   = 1
toPduCode ResetQueryPdu       = 2
toPduCode CacheResponsePdu {} = 3
toPduCode IPv4PrefixPdu {}    = 4
toPduCode IPv6PrefixPdu {}    = 6
toPduCode EndOfDataPdu {}     = 7
toPduCode CacheResetPdu       = 8
toPduCode RouterKeyPdu {}     = 9     
toPduCode ErrorPdu {}         = 10    

-- 
pduLength :: Pdu -> ProtocolVersion -> Int32 
pduLength NotifyPdu {} _        = 12
pduLength SerialQueryPdu {} _   = 12
pduLength ResetQueryPdu       _ = 8
pduLength CacheResponsePdu {} _ = 8
pduLength IPv4PrefixPdu {}  _   = 20
pduLength IPv6PrefixPdu {}  _   = 32
pduLength EndOfDataPdu {} V0    = 12
pduLength EndOfDataPdu {} V1    = 24
pduLength CacheResetPdu _       = 8

pduLength (RouterKeyPdu _ _ ski bs2) _ = 
    fromIntegral $ 8 + 4 + (fromIntegral (skiLen ski) :: Int64) + BSL.length bs2

pduLength (ErrorPdu _ bs1 bs2) _ = 
    fromIntegral $ 8 + 4 + 4 + BSL.length bs1 + BSL.length bs2

-- 
-- | Serialise PDU into bytes according to the RTR protocal 
-- 
pduToBytes :: Pdu -> Session -> BSL.ByteString
pduToBytes pdu (Session protocolVersion)  = 
    runPut $ pduHeader >> pduContent
    where
        pduHeader = put protocolVersion >> put (toPduCode pdu)            

        pduContent = case pdu of 
            NotifyPdu sessionId serial ->         
                put sessionId >> put serial >> put pduLen

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
                put $ ipv4PrefixLen prefix
                put (fromIntegral maxLength :: Word8)
                put (0 :: Word16)
                put prefix
                put asn                

            IPv6PrefixPdu flags prefix asn maxLength -> do 
                put (0 :: Word16)
                put pduLen
                put flags
                put $ ipv6PrefixLen prefix
                put (fromIntegral maxLength :: Word8)
                put (0 :: Word16)
                put prefix
                put asn                

            EndOfDataPdu sessionId serial intervals -> do
                put sessionId
                put pduLen  
                put serial 
                case protocolVersion of 
                    V0 -> pure ()
                    V1 -> put intervals

            CacheResetPdu -> 
                put (0 :: Word16) >> put pduLen

            -- This is illegal to use for V0, but we are not going to complain here
            RouterKeyPdu (ASN asn') flags (SKI (KI ski)) spki -> do                
                put flags
                put (0 :: Word8)
                put pduLen
                put ski
                put asn'
                put spki

            ErrorPdu errorCode causingPdu errorText -> do
                put errorCode
                put pduLen
                put (fromIntegral (BSL.length causingPdu) :: Int32)
                put causingPdu 
                put (fromIntegral (BSL.length errorText) :: Int32)
                put errorText
    
        pduLen = pduLength pdu protocolVersion :: Int32


        ipvXPrefixPdu :: Binary prefix => Flags -> prefix -> Put
        ipvXPrefixPdu flags prefix = do 
            put (0 :: Word16)
            put pduLen
            put flags
            put prefix    
        


-- 
-- | Parse PDUs from bytestrings according to the RTR protocal 
-- 
bytesToVersionedPdu :: BS.ByteString -> Either (ErrorCode, Maybe Text) VersionedPdu
bytesToVersionedPdu bs = 
    case runGetOrFail parsePdu (BSL.fromStrict bs) of
        Left (bs, offset, errorMessage) -> 
            Left (InvalidRequest, Just $ Text.pack $ "Error parsing " <> show bs)
            
        Right (_, _, pdu) -> Right pdu
    where
        parsePdu = do 
            protocolVersion <- get
            pdu <- parseVersionedPdu $ Session protocolVersion            
            pure $ VersionedPdu pdu protocolVersion


bytesToPduOfKnownVersion :: Session -> BS.ByteString -> Either (ErrorCode, Text) Pdu
bytesToPduOfKnownVersion s@(Session protocolVersion) bs = 
    case runGetOrFail parsePdu (BSL.fromStrict bs) of
        Left (bs, offset, errorMessage) -> 
            Left (InvalidRequest, Text.pack $ "Error parsing " <> show (hex bs) <> ": " <> errorMessage)
            
        Right (_, _, VersionedPdu pdu _) -> Right pdu
    where
        parsePdu = do 
            _ignore :: ProtocolVersion <- get
            pdu <- parseVersionedPdu s
            pure $ VersionedPdu pdu protocolVersion


parseVersionedPdu :: Session -> Get Pdu
parseVersionedPdu (Session protocolVersion) = do 
    pduCode :: Word8 <- get
    case pduCode of 
        0  -> do
            sessionId <- get
            len :: Int32  <- get
            assertLength len 12
            serial    <- get
            pure $ NotifyPdu sessionId serial

        1  -> do 
            sessionId <- get
            len :: Int32  <- get
            assertLength len 12
            serial    <- get
            pure $ SerialQueryPdu sessionId serial                    

        2  -> do 
            zero :: Word16 <- get 
            unless (zero == 0) $ fail "Field must be zero for ResetQueryPdu"
            len  :: Int32 <- get
            assertLength len 8                    
            pure $ ResetQueryPdu

        3  -> do 
            sessionId :: RtrSessionId <- get
            len :: Int32              <- get
            assertLength len 8
            pure $ CacheResponsePdu sessionId

        -- TODO Finish them
        -- 4  -> do 
        --     zero :: Word16 <- get 
        --     unless (zero == 0) $ fail "Field must be zero for IPv4PrefixPdu"
        --     len  :: Int32 <- get
        --     assertLength len 20
        --     flags <- get
        --     rtrPrefix <- get
        --     pure $ IPv4PrefixPdu flags rtrPrefix

        -- 6  -> do 
        --     zero :: Word16 <- get 
        --     unless (zero == 0) $ fail "Field must be zero for IPv6PrefixPdu"
        --     len  :: Int32 <- get
        --     assertLength len 32
        --     flags     <- get
        --     rtrPrefix <- get
        --     pure $ IPv6PrefixPdu flags rtrPrefix

        7  -> do
            sessionId     <- get
            len  :: Int32 <- get                    
            serial        <- get 
            case protocolVersion of 
                V0 -> do 
                    assertLength len 12
                    -- 1) It doesn't matter what we put into intervals, since it's 
                    -- not going to be used anyway in V0 protocol.
                    -- 2) Normally we are not even supposed to parse this type of PDUs                    
                    pure $ EndOfDataPdu sessionId serial defIntervals
                V1 -> do                        
                    intervals <- get
                    assertLength len 24
                    pure $ EndOfDataPdu sessionId serial intervals

        8  -> do 
            zero :: Word16 <- get 
            unless (zero == 0) $ fail "Field must be zero for ResetQueryPdu"
            len  :: Int32 <- get
            assertLength len 8
            pure CacheResetPdu

        9  -> do 
            flags         <- get
            zero :: Word8 <- get
            unless (zero == 0) $ fail "Field must be zero for RouterKeyPduV1"
            len  :: Int32 <- get
            ski <- getByteString 20
            asn' <- get
            spki <- getRemainingLazyByteString

            assertLength len $ 8 + 4 + 
                    (fromIntegral (BS.length ski) :: Int32) + 
                    (fromIntegral (BSL.length spki) :: Int32)

            pure $ RouterKeyPdu asn' flags (SKI (KI $ toShortBS ski)) spki

        10 -> do 
            errorCode <- get
            encapsulatedPduLen :: Int32 <- get
            encapsulatedPdu <- getByteString $ fromIntegral encapsulatedPduLen
            textLen :: Int32 <- get
            text <- getByteString $ fromIntegral textLen
            pure $ ErrorPdu errorCode (BSL.fromStrict encapsulatedPdu) (BSL.fromStrict text)

        n  -> fail $ "Invalid PDU type " <> show n

    where
        assertLength len mustBe =     
            unless (len == mustBe) $ 
                fail $ "Wrong length " <> show len <> ", must be " <> show mustBe

 
toVersioned :: Session -> Pdu -> VersionedPdu
toVersioned (Session protocolVersion) pdu = VersionedPdu pdu protocolVersion