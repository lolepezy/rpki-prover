{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE StrictData   #-}

module RPKI.RTR.Binary where

import qualified Data.ByteString         as BS
import           Data.ByteString.Builder

import           Data.Int
import           GHC.TypeLits

import           Data.Data
import           RPKI.Domain             (KI (..), SKI (..), skiLen)
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
pduLength (RouterKeyPduV1 _ _ ski bs2) = fromIntegral $ 8 + 4 + skiLen ski + BS.length bs2
pduLength (ErrorPdu _ bs1 bs2)         = fromIntegral $ 8 + 4 + 4 + BS.length bs1 + BS.length bs2


pduToBytes :: forall version pduType . KnownNat pduType => 
            TypeToBytes version =>             
            Pdu version pduType -> Builder
pduToBytes pdu = 
    pduHeader <> pduContent
    where
        pduHeader = 
            typeToBytes (Proxy :: Proxy version) <> 
            word8 (fromIntegral $ natVal (Proxy :: Proxy pduType))

        pduContent = case pdu of 
            NotifyPdu sessionId serial ->                 
                valueToBytes sessionId <> 
                valueToBytes serial <> 
                pduLen

            SerialQueryPdu sessionId serial ->                
                valueToBytes sessionId <> 
                pduLen <> 
                valueToBytes serial

            ResetQueryPdu -> int16BE 0 <> pduLen
        
            CacheResponsePdu sessionId -> valueToBytes sessionId <> pduLen

            IPv4PrefixPdu flags rtrPrefix -> ipvXPrefixPdu flags rtrPrefix 
            IPv6PrefixPdu flags rtrPrefix -> ipvXPrefixPdu flags rtrPrefix

            EndOfDataPduV0 sessionId serial ->
                valueToBytes sessionId <>     
                pduLen <>
                valueToBytes serial

            EndOfDataPduV1 sessionId serial intervals ->                
                valueToBytes sessionId <>     
                pduLen <>
                valueToBytes serial <>
                int32BE (refreshInterval intervals) <>
                int32BE (retryInterval intervals) <>
                int32BE (expireInterval intervals)

            CacheResetPdu -> int16BE 0 <> pduLen

            RouterKeyPduV1 (ASN asn') flags (SKI (KI ski)) spki ->                 
                valueToBytes flags <>
                word8 0 <>
                pduLen <>
                shortByteString ski <>
                word32BE asn' <>
                byteString spki

            ErrorPdu errorCode causingPdu errorText ->                
                valueToBytes errorCode <>
                pduLen <>
                int32BE (fromIntegral $ BS.length causingPdu) <>
                byteString causingPdu <>    
                int32BE (fromIntegral $ BS.length errorText) <>
                byteString errorText
    
        pduLen = int32BE $ pduLength pdu

        ipvXPrefixPdu :: KnownNat c => Flags c -> RtrPrefix -> Builder
        ipvXPrefixPdu flags rtrPrefix =     
            int16BE 0 <> 
            pduLen <>
            valueToBytes flags <>
            word8 (fromIntegral $ prefixLength rtrPrefix) <> 
            word8 (fromIntegral $ maxLength rtrPrefix) <> 
            word8 0 <> 
            byteString (prefix rtrPrefix) <> 
            int32BE (fromIntegral $ asn rtrPrefix)    