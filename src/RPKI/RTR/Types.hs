{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE StrictData   #-}

module RPKI.RTR.Types where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB

import Data.Int
import GHC.TypeLits

import RPKI.Resources.Types
import Data.Data

data ProtocolVersion = 
    V0 -- | defined by https://tools.ietf.org/rfc/rfc6810
  | V1 -- | defined by https://tools.ietf.org/rfc/rfc8210


data Intervals = Intervals {
    refreshInterval :: Int,
    retryInterval :: Int,
    expireInterval:: Int
} deriving stock (Show, Eq, Ord)


data Pdu (version :: ProtocolVersion) (rtrType :: Nat) where
    NotifyPdu        :: SessionId -> SerialNumber -> Pdu version 0
    SerialQueryPdu   :: SessionId -> SerialNumber -> Pdu version 1
    ResetQueryPdu    :: Pdu version 2    
    CacheResponsePdu :: SessionId -> Pdu version 3
    IPv4PrefixPdu    :: Flags c -> RtrPrefix -> Pdu version 4
    IPv6PrefixPdu    :: Flags c -> RtrPrefix -> Pdu version 6
    EndOfDataPduV0   :: SessionId -> SerialNumber -> Pdu 'V0 7
    EndOfDataPduV1   :: SessionId -> SerialNumber -> Intervals -> Pdu 'V1 7
    CacheResetPdu    :: Pdu version 8        
    RouterKeyPduV1   :: ASN -> Flags c -> BS.ByteString -> BS.ByteString -> Pdu 'V1 9
    ErrorPdu         :: ErrorCode code -> BS.ByteString -> BS.ByteString -> Pdu version 10    
    

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

-- TODO
data RtrPrefix = RtrPrefix


pduLength :: Pdu version rt -> Int32 
pduLength (NotifyPdu _ _)       = 12
pduLength (SerialQueryPdu _ _)  = 12
pduLength ResetQueryPdu         = 8
pduLength (CacheResponsePdu _)  = 8
pduLength (IPv4PrefixPdu _ _)   = 20
pduLength (IPv6PrefixPdu _ _)   = 32
pduLength (EndOfDataPduV0 _ _)  = 12
pduLength EndOfDataPduV1 {}     = 24
pduLength CacheResetPdu         = 8
pduLength (RouterKeyPduV1 _ _ bs1 bs2) = fromIntegral $ 8 + 4 + BS.length bs1 + BS.length bs2
pduLength (ErrorPdu _ bs1 bs2)         = fromIntegral $ 8 + 4 + 4 + BS.length bs1 + BS.length bs2


pduToBytes :: forall version pduType . TypeToBytes version =>             
            Pdu version pduType -> 
            BB.Builder
pduToBytes pdu@(NotifyPdu sessionId serial) = 
    pduHeader pdu <> 
    valueToBytes sessionId <> 
    valueToBytes serial <>
    BB.int32BE (pduLength pdu)

pduToBytes pdu@(SerialQueryPdu sessionId serial) = 
    pduHeader pdu <>
    valueToBytes sessionId <> 
    BB.int32BE (pduLength pdu) <>
    valueToBytes serial

pduToBytes pdu@ResetQueryPdu = 
    pduHeader pdu <> 
    BB.int16BE 0 <>     
    BB.int32BE (pduLength pdu)
    

pduToBytes pdu@(CacheResponsePdu _)  = pduHeader pdu
pduToBytes pdu@(IPv4PrefixPdu _ _)   = pduHeader pdu
pduToBytes pdu@(IPv6PrefixPdu _ _)   = pduHeader pdu
pduToBytes pdu@(EndOfDataPduV0 _ _)  = pduHeader pdu
pduToBytes pdu@EndOfDataPduV1 {}     = pduHeader pdu
pduToBytes pdu@CacheResetPdu         = pduHeader pdu
pduToBytes pdu@(RouterKeyPduV1 _ _ bs1 bs2) = pduHeader pdu
pduToBytes pdu@(ErrorPdu _ bs1 bs2)         = pduHeader pdu


pduHeader :: forall pduType version . KnownNat pduType => 
            TypeToBytes version =>             
            Pdu version pduType -> BB.Builder
pduHeader _ = typeToBytes (Proxy :: Proxy version) <> 
              BB.word8 (fromIntegral $ natVal (Proxy :: Proxy pduType))
        
class TypeToBytes a where    
    typeToBytes :: Proxy a -> BB.Builder

class ValueToBytes a where    
    valueToBytes :: a -> BB.Builder
    
instance TypeToBytes 'V0 where typeToBytes _ = BB.word8 0
instance TypeToBytes 'V1 where typeToBytes _ = BB.word8 1

instance ValueToBytes SessionId where 
    valueToBytes (SessionId s) = BB.int16BE s

instance ValueToBytes SerialNumber where 
    valueToBytes (SerialNumber s) = BB.int16BE s