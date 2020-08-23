{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE StrictData   #-}

module RPKI.RTR.Types where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Builder

import Data.Binary
import Data.Int
import GHC.TypeLits

import RPKI.Domain (SKI(..), KI(..))
import RPKI.Resources.Types
import Data.Data
import GHC.Generics (Generic)

data ProtocolVersion = 
    V0 -- | as defined by https://tools.ietf.org/rfc/rfc6810
  | V1 -- | as defined by https://tools.ietf.org/rfc/rfc8210


data APdu where
    APdu :: forall (version :: ProtocolVersion) (pduType :: Nat) . Pdu version pduType -> APdu

-- | PDUs definede both by V0 and V1 protocols
data Pdu (version :: ProtocolVersion) (pduType :: Nat) where
    NotifyPdu        :: SessionId -> SerialNumber -> Pdu version 0
    SerialQueryPdu   :: SessionId -> SerialNumber -> Pdu version 1
    ResetQueryPdu    :: Pdu version 2    
    CacheResponsePdu :: SessionId -> Pdu version 3
    IPv4PrefixPdu    :: Flags -> RtrPrefix -> Pdu version 4
    IPv6PrefixPdu    :: Flags -> RtrPrefix -> Pdu version 6
    EndOfDataPduV0   :: SessionId -> SerialNumber -> Pdu 'V0 7
    EndOfDataPduV1   :: SessionId -> SerialNumber -> Intervals -> Pdu 'V1 7
    CacheResetPdu    :: Pdu version 8        
    RouterKeyPduV1   :: ASN -> Flags -> SKI -> BSL.ByteString -> Pdu 'V1 9
    ErrorPdu         :: ErrorCode -> BSL.ByteString -> BSL.ByteString -> Pdu version 10    
    

newtype SerialNumber = SerialNumber Int16
    deriving stock (Show, Eq, Ord, Generic)

newtype SessionId = SessionId Int16
    deriving stock (Show, Eq, Ord, Generic)

data Flags = Announcement | Withdrawal
    deriving stock (Show, Eq, Ord, Generic)

data ErrorCode
  = CorruptData
  | PduInternalError
  | NoDataAvailable
  | InvalidRequest
  | UnsupportedProtocolVersion
  | UnsupportedPduType
  | WithdrawalOfUnknownRecord
  | DuplicateAnnouncementReceived
  | UnexpectedProtocolVersion
  deriving stock (Show, Eq, Ord, Generic)


data RtrPrefix = RtrPrefix {
    prefixLength :: Int8,
    maxLength :: Int8,
    prefix :: BS.ByteString,
    asn :: Int32
}

data Intervals = Intervals {
    refreshInterval :: Int32,
    retryInterval :: Int32,
    expireInterval:: Int32
} deriving stock (Show, Eq, Ord)

class VersionToBytes a where    
    versionToBytes :: Proxy a -> Word8

class ValueToBytes a where    
    valueToBytes :: a -> Builder
    
instance VersionToBytes 'V0 where versionToBytes _ = 0
instance VersionToBytes 'V1 where versionToBytes _ = 1

-- instance KnownNat c => ValueToBytes (ErrorCode c) where 
--     valueToBytes _ = word8 $ fromIntegral $ natVal (Proxy :: Proxy c)    


instance Binary SessionId
instance Binary SerialNumber

-- Orphans
instance Binary ASN
instance Binary SKI
instance Binary KI

instance Binary Flags where 
    put f = put $ case f of 
        Withdrawal   -> 0 :: Word8
        Announcement -> 1

    get = f <$> get
        where 
            f (0 :: Word8) = Withdrawal
            f 1            = Announcement

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
    
