{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData   #-}

module RPKI.RTR.Types where

import qualified Data.ByteString as BS

import Data.Int (Int16)
import GHC.TypeLits (Nat)

import RPKI.Resources.Types

data ProtocolVersion
  = V0 -- | defined by https://tools.ietf.org/rfc/rfc6810
  | V1 -- | defined by https://tools.ietf.org/rfc/rfc8210


data Intervals = Intervals {
    refreshInterval :: Int,
    retryInterval :: Int,
    expireInterval:: Int
} deriving stock (Show, Eq, Ord)


data Pdu (version :: ProtocolVersion) (rtrType :: Nat) where
    NotifyPdu        :: SessionId -> SerialNumber -> Pdu protocol 0
    SerialQueryPdu   :: SessionId -> SerialNumber -> Pdu protocol 1
    ResetQueryPdu    :: Pdu protocol 2    
    CacheResponsePdu :: SessionId -> Pdu protocol 3
    IPv4PrefixPdu    :: Flags c -> RtrPrefix -> Pdu protocol 4
    IPv6PrefixPdu    :: Flags c -> RtrPrefix -> Pdu protocol 6
    EndOfDataPduV0   :: SessionId -> SerialNumber -> Pdu 'V0 7
    EndOfDataPduV1   :: SessionId -> SerialNumber -> Intervals -> Pdu 'V1 7
    CacheResetPdu    :: Pdu protocol 8        
    RouterKeyPdu     :: ASN -> Flags c -> BS.ByteString -> BS.ByteString -> Pdu protocol 9
    ErrorPdu         :: ErrorCode code -> BS.ByteString -> BS.ByteString -> Pdu protocol 10
    

newtype SerialNumber = SerialNumber Int16
    deriving stock (Show, Eq, Ord)

newtype SessionId = SessionId Int16
    deriving stock (Show, Eq, Ord)


data Flags (numeric :: Nat) where 
    Announcement :: Flags 1
    Withdrawal   :: Flags 0

data ErrorCode (numeric :: Nat) where
    CorruptData :: ErrorCode 0
    PduInternalError :: ErrorCode 1 
    NoDataAvailable :: ErrorCode 2
    InvalidRequest :: ErrorCode 3
    UnsupportedProtocolVersion :: ErrorCode 4
    UnsupportedPduType :: ErrorCode 5
    WithdrawalOfUnknownRecord :: ErrorCode 6
    DuplicateAnnouncementReceived :: ErrorCode 7
    UnexpectedProtocolVersion :: ErrorCode 8

-- TODO
data RtrPrefix = RtrPrefix


pduLength :: Pdu protocol rt -> Int 
pduLength (NotifyPdu _ _)       = 12
pduLength (SerialQueryPdu _ _)  = 12
pduLength ResetQueryPdu         = 8
pduLength (CacheResponsePdu _)  = 8
pduLength (IPv4PrefixPdu _ _)   = 20
pduLength (IPv6PrefixPdu _ _)   = 32
pduLength (EndOfDataPduV0 _ _)  = 12
pduLength EndOfDataPduV1 {}     = 24
pduLength CacheResetPdu         = 8
pduLength (RouterKeyPdu _ _ bs1 bs2) = 8 + 4 + BS.length bs1 + BS.length bs2
pduLength (ErrorPdu _ bs1 bs2)       = 8 + 4 + 4 + BS.length bs1 + BS.length bs2