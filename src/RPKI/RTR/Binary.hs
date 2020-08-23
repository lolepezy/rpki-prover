{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE StrictData   #-}
{-# LANGUAGE OverloadedStrings   #-}

module RPKI.RTR.Binary where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Text
import qualified Data.Text            as Text

import           Data.Binary
import           Data.Binary.Get      (getByteString, runGetOrFail)
import           Data.Binary.Put      (runPut)

import           Data.Int
import           GHC.TypeLits

import           Control.Monad        (unless)
import           Data.Data
import           RPKI.Domain          (KI (..), SKI (..), skiLen, toShortBS)
import           RPKI.Resources.Types
import           RPKI.RTR.Types



pduLength :: Pdu version pduType -> Int32 
pduLength (NotifyPdu _ _)       = 12
pduLength (SerialQueryPdu _ _)  = 12
pduLength ResetQueryPdu         = 8
pduLength (CacheResponsePdu _)  = 8
pduLength (IPv4PrefixPdu _ _)   = 20
pduLength (IPv6PrefixPdu _ _)   = 32
pduLength (EndOfDataPduV0 _ _)  = 12
pduLength EndOfDataPduV1 {}     = 24
pduLength CacheResetPdu         = 8

pduLength (RouterKeyPduV1 _ _ ski bs2) = 
    fromIntegral $ 8 + 4 + (fromIntegral (skiLen ski) :: Int64) + BSL.length bs2

pduLength (ErrorPdu _ bs1 bs2) = 
    fromIntegral $ 8 + 4 + 4 + BSL.length bs1 + BSL.length bs2

pduToBytes :: forall version pduType . KnownNat pduType => 
                VersionToBytes version =>             
                Pdu version pduType -> BSL.ByteString
pduToBytes pdu = 
    runPut $ pduHeader >> pduContent
    where
        pduHeader = do
            put $ versionToBytes (Proxy :: Proxy version)
            put ((fromIntegral $ natVal (Proxy :: Proxy pduType)) :: Word8)

        pduContent = case pdu of 
            NotifyPdu sessionId serial ->         
                put sessionId >> put serial >> put pduLen

            SerialQueryPdu sessionId serial ->                
                put sessionId >> put pduLen >> put serial

            ResetQueryPdu -> 
                put (0 :: Word16) >> put pduLen
        
            CacheResponsePdu sessionId -> 
                put sessionId >> put pduLen

            IPv4PrefixPdu flags rtrPrefix -> ipvXPrefixPdu flags rtrPrefix 
            IPv6PrefixPdu flags rtrPrefix -> ipvXPrefixPdu flags rtrPrefix

            EndOfDataPduV0 sessionId serial ->
                put sessionId >> put pduLen >> put serial

            EndOfDataPduV1 sessionId serial intervals -> do
                put sessionId
                put pduLen  
                put serial 
                put intervals                

            CacheResetPdu -> 
                put (0 :: Word16) >> put pduLen

            RouterKeyPduV1 (ASN asn') flags (SKI (KI ski)) spki -> do                
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
    
        pduLen = pduLength pdu :: Int32

        ipvXPrefixPdu :: Flags -> RtrPrefix -> Put
        ipvXPrefixPdu flags rtrPrefix = do 
            put (0 :: Word16)
            put pduLen
            put flags
            put (fromIntegral (prefixLength rtrPrefix) :: Word8)
            put (fromIntegral (maxLength rtrPrefix) :: Word8) 
            put (0 :: Word8)  
            put (prefix rtrPrefix)
            put (fromIntegral (asn rtrPrefix) :: Int32)    


bytesToPdu :: BS.ByteString -> Either Text APdu
bytesToPdu bs = 
    case runGetOrFail getIt (BSL.fromStrict bs) of
        Left (bs, offset, errorMessage) -> Left $ Text.pack $ "Error parsing " <> show bs 
        Right (_, _, pdu) -> Right pdu
    where 
        assertLength len mustBe =     
            unless (len == mustBe) $ 
                fail $ "Wrong length " <> show len <> ", must be " <> show mustBe

        getIt = do 
            protocolVersion :: Word8 <- get
            case protocolVersion of
                0 -> parsePdu V0
                1 -> parsePdu V1
                n -> fail $ "Invalid protocol version " <> show n

        parsePdu :: ProtocolVersion -> Get APdu 
        parsePdu protocolVersion = do 
            pduType :: Word8 <- get
            case pduType of 
                0  -> do
                    sessionId <- get
                    len :: Int32  <- get
                    assertLength len 12
                    serial    <- get
                    pure $ APdu $ NotifyPdu sessionId serial

                1  -> do 
                    sessionId <- get
                    len :: Int32  <- get
                    assertLength len 12
                    serial    <- get
                    pure $ APdu $ SerialQueryPdu sessionId serial                    

                2  -> do 
                    zero :: Word16 <- get 
                    unless (zero == 0) $ fail "Field must be zero for ResetQueryPdu"
                    len  :: Int32 <- get
                    assertLength len 8                    
                    pure $ APdu ResetQueryPdu

                3  -> do 
                    sessionId :: SessionId <- get
                    len :: Int32           <- get
                    assertLength len 8
                    pure $ APdu $ CacheResponsePdu sessionId

                4  -> do 
                    zero :: Word16 <- get 
                    unless (zero == 0) $ fail "Field must be zero for IPv4PrefixPdu"
                    len  :: Int32 <- get
                    assertLength len 20
                    flags <- get
                    rtrPrefix <- get
                    pure $ APdu $ IPv4PrefixPdu flags rtrPrefix

                6  -> do 
                    zero :: Word16 <- get 
                    unless (zero == 0) $ fail "Field must be zero for IPv6PrefixPdu"
                    len  :: Int32 <- get
                    assertLength len 32
                    flags     <- get
                    rtrPrefix <- get
                    pure $ APdu $ IPv6PrefixPdu flags rtrPrefix

                7  -> do
                    sessionId     <- get
                    len  :: Int32 <- get                    
                    serial        <- get 
                    case protocolVersion of 
                        V0 -> do 
                            assertLength len 12
                            pure $ APdu $ EndOfDataPduV0 sessionId serial
                        V1 -> do                        
                            intervals <- get
                            assertLength len 24
                            pure $ APdu $ EndOfDataPduV1 sessionId serial intervals                        

                8  -> do 
                    zero :: Word16 <- get 
                    unless (zero == 0) $ fail "Field must be zero for ResetQueryPdu"
                    len  :: Int32 <- get
                    assertLength len 8
                    pure $ APdu CacheResetPdu

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

                    pure $ APdu $ RouterKeyPduV1 asn' flags (SKI (KI $ toShortBS ski)) spki

                10 -> do 
                    errorCode <- get
                    encapsulatedPduLen :: Int32 <- get
                    encapsulatedPdu <- getByteString $ fromIntegral encapsulatedPduLen
                    textLen :: Int32 <- get
                    text <- getByteString $ fromIntegral textLen
                    pure $ APdu $ ErrorPdu errorCode (BSL.fromStrict encapsulatedPdu) (BSL.fromStrict text)
                    
                n  -> fail $ "Invalid PDU type " <> show n

getRemainingLazyByteString :: Get a0
getRemainingLazyByteString = error "not implemented"


    