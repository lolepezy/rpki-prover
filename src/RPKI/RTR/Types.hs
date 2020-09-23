{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StrictData          #-}


module RPKI.RTR.Types where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Data.Binary
import Data.Int

import RPKI.Domain (SKI(..), KI(..))
import RPKI.Resources.Types
import Data.Data
import GHC.Generics (Generic)

data ProtocolVersion = 
    V0 -- | as defined by https://tools.ietf.org/rfc/rfc6810
  | V1 -- | as defined by https://tools.ietf.org/rfc/rfc8210
  deriving stock (Show, Eq, Ord, Typeable, Generic)


data Pdu = NotifyPdu SessionId SerialNumber
        | SerialQueryPdu SessionId SerialNumber
        | ResetQueryPdu
        | CacheResponsePdu SessionId
        | IPv4PrefixPdu Flags RtrPrefix 
        | IPv6PrefixPdu Flags RtrPrefix     
        | EndOfDataPdu SessionId SerialNumber Intervals
        | CacheResetPdu
        | RouterKeyPdu ASN Flags SKI BSL.ByteString
        | ErrorPdu ErrorCode BSL.ByteString BSL.ByteString
    deriving stock (Show, Eq, Ord, Generic)

data VersionedPdu = VersionedPdu Pdu ProtocolVersion
    deriving stock (Show, Eq, Ord, Generic)

newtype SerialNumber = SerialNumber Int32
    deriving stock (Show, Eq, Ord, Generic)

newtype SessionId = SessionId Int16
    deriving stock (Show, Eq, Ord, Generic)

data Flags = Announcement | Withdrawal
    deriving stock (Show, Eq, Ord, Generic)

data ErrorCode = CorruptData
        | PduInternalError
        | NoDataAvailable
        | InvalidRequest
        | UnsupportedProtocolVersion
        | UnsupportedPduType
        | WithdrawalOfUnknownRecord
        | DuplicateAnnouncementReceived
        | UnexpectedProtocolVersion
    deriving  (Show, Eq, Ord, Generic)


newtype Session = Session ProtocolVersion
    deriving stock (Show, Eq, Ord, Generic)

data RtrPrefix = RtrPrefix {
    prefixLength :: Int8,
    maxLength :: Int8,
    prefix :: BS.ByteString,
    asn :: Int32
} deriving stock (Show, Eq, Ord, Generic)

data Intervals = Intervals {
    refreshInterval :: Int32,
    retryInterval :: Int32,
    expireInterval:: Int32
} deriving stock (Show, Eq, Ord)

instance Binary SessionId
instance Binary SerialNumber

-- Orphans
instance Binary ASN
instance Binary SKI
instance Binary KI


defIntervals :: Intervals
defIntervals = Intervals { 
    refreshInterval = 3600,
    retryInterval = 600,
    expireInterval = 7200
}

instance Binary ProtocolVersion where
    put f = put $ case f of 
        V0 -> 0 :: Word8
        V1 -> 1

    get = f <$> get
        where 
            f (0 :: Word8) = V0
            f 1            = V1
            f n            = error $ "No error code value for " <> show n

instance Binary Flags where 
    put f = put $ case f of 
        Withdrawal   -> 0 :: Word8
        Announcement -> 1

    get = f <$> get
        where 
            f (0 :: Word8) = Withdrawal
            f 1            = Announcement
            f n            = error $ "No flags value for " <> show n

instance Binary RtrPrefix where 
    put RtrPrefix {..} = do
        put (fromIntegral prefixLength :: Word8)
        put (fromIntegral maxLength :: Word8) 
        put (0 :: Word8)  
        put prefix
        put (fromIntegral asn :: Int32)    
        
    get = do
        prefixLength <- get            
        maxLength    <- get
        _skipZero :: Word8 <- get
        prefix       <- get
        asn          <- get
        pure $ RtrPrefix prefixLength maxLength prefix asn

errorCodes :: [(ErrorCode, Word8)]
errorCodes = [
        (CorruptData,                   0),
        (PduInternalError,              1),  
        (NoDataAvailable,               2),
        (InvalidRequest,                3),
        (UnsupportedProtocolVersion,    4),
        (UnsupportedPduType,            5),
        (WithdrawalOfUnknownRecord,     6),
        (DuplicateAnnouncementReceived, 7),
        (UnexpectedProtocolVersion,     8)
    ]

instance Binary ErrorCode where         
    put code =
        case lookup code errorCodes of
            Just n  -> put n
            Nothing -> error $ "Oops, there's not code for " <> show code

    get = do
        numeric <- get
        case filter ((== numeric) . snd) errorCodes of 
            [(c, _)] -> pure c
            _        -> error $ "No error code value for " <> show numeric


instance Binary Intervals where         
    put Intervals {..} = do 
        put refreshInterval
        put retryInterval
        put expireInterval        

    get = Intervals <$> get <*> get <*> get
    

-- 
-- Wrap around at 2^31 - 1
-- https://tools.ietf.org/html/rfc8210#page-5
-- 
nextSerial :: SerialNumber -> SerialNumber 
nextSerial (SerialNumber n) = 
    SerialNumber $ 
        if (fromIntegral n :: Integer) == (2 :: Integer)^(31 :: Integer) - 1
            then 0
            else n + 1    

initialSerial :: SerialNumber
initialSerial = SerialNumber 1337