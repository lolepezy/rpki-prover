{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE StrictData   #-}

module RPKI.RTR.Types where

import qualified Data.ByteString as BS
import Data.ByteString.Builder

import Data.Int
import GHC.TypeLits

import RPKI.Domain (SKI(..))
import RPKI.Resources.Types
import Data.Data

data ProtocolVersion = 
    V0 -- | as defined by https://tools.ietf.org/rfc/rfc6810
  | V1 -- | as defined by https://tools.ietf.org/rfc/rfc8210


data Intervals = Intervals {
    refreshInterval :: Int32,
    retryInterval :: Int32,
    expireInterval:: Int32
} deriving stock (Show, Eq, Ord)

-- | PDUs definede both by V0 and V1 protocols
data Pdu (version :: ProtocolVersion) (pduType :: Nat) where
    NotifyPdu        :: SessionId -> SerialNumber -> Pdu version 0
    SerialQueryPdu   :: SessionId -> SerialNumber -> Pdu version 1
    ResetQueryPdu    :: Pdu version 2    
    CacheResponsePdu :: SessionId -> Pdu version 3
    IPv4PrefixPdu    :: KnownNat c => Flags c -> RtrPrefix -> Pdu version 4
    IPv6PrefixPdu    :: KnownNat c => Flags c -> RtrPrefix -> Pdu version 6
    EndOfDataPduV0   :: SessionId -> SerialNumber -> Pdu 'V0 7
    EndOfDataPduV1   :: SessionId -> SerialNumber -> Intervals -> Pdu 'V1 7
    CacheResetPdu    :: Pdu version 8        
    RouterKeyPduV1   :: KnownNat c => 
                        ASN -> Flags c -> SKI -> BS.ByteString -> Pdu 'V1 9
    ErrorPdu         :: KnownNat code => 
                        ErrorCode code -> BS.ByteString -> BS.ByteString -> Pdu version 10    
    

newtype SerialNumber = SerialNumber Int16
    deriving stock (Show, Eq, Ord)

newtype SessionId = SessionId Int16
    deriving stock (Show, Eq, Ord)

data Flags (numeric :: Nat) where 
    Announcement :: Flags 1
    Withdrawal   :: Flags 0

data ErrorCode (numeric :: Nat) where
    CorruptData                   :: ErrorCode 0
    PduInternalError              :: ErrorCode 1 
    NoDataAvailable               :: ErrorCode 2
    InvalidRequest                :: ErrorCode 3
    UnsupportedProtocolVersion    :: ErrorCode 4
    UnsupportedPduType            :: ErrorCode 5
    WithdrawalOfUnknownRecord     :: ErrorCode 6
    DuplicateAnnouncementReceived :: ErrorCode 7
    UnexpectedProtocolVersion     :: ErrorCode 8

data RtrPrefix = RtrPrefix {
    prefixLength :: Int8,
    maxLength :: Int8,
    prefix :: BS.ByteString,
    asn :: Int32
}

class TypeToBytes a where    
    typeToBytes :: Proxy a -> Builder

class ValueToBytes a where    
    valueToBytes :: a -> Builder
    
instance TypeToBytes 'V0 where typeToBytes _ = word8 0
instance TypeToBytes 'V1 where typeToBytes _ = word8 1

instance ValueToBytes SessionId where 
    valueToBytes (SessionId s) = int16BE s

instance ValueToBytes SerialNumber where 
    valueToBytes (SerialNumber s) = int16BE s

instance KnownNat n => ValueToBytes (Flags n) where 
    valueToBytes _ = word8 $ fromIntegral $ natVal (Proxy :: Proxy n)    

instance KnownNat c => ValueToBytes (ErrorCode c) where 
    valueToBytes _ = word8 $ fromIntegral $ natVal (Proxy :: Proxy c)    
