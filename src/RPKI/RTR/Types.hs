{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StrictData          #-}


module RPKI.RTR.Types where

import qualified Data.ByteString.Lazy     as BSL

import           Data.Binary
import           Data.Int
import           Data.Text                (Text)

import           Data.Data
import           GHC.Generics             (Generic)
import           RPKI.Domain              (KI (..), SKI (..))
import           RPKI.Resources.Resources
import           RPKI.Resources.Types



data ProtocolVersion = 
    V0 -- | as defined by https://tools.ietf.org/rfc/rfc6810
  | V1 -- | as defined by https://tools.ietf.org/rfc/rfc8210
  deriving stock (Show, Eq, Ord, Typeable, Generic)


-- | PDUs cover both V0 and V1 versions
data Pdu = NotifyPdu RtrSessionId SerialNumber
        | SerialQueryPdu RtrSessionId SerialNumber
        | ResetQueryPdu
        | CacheResponsePdu RtrSessionId
        | IPv4PrefixPdu Flags Ipv4Prefix ASN PrefixLength
        | IPv6PrefixPdu Flags Ipv6Prefix ASN PrefixLength    
        | EndOfDataPdu RtrSessionId SerialNumber Intervals
        | CacheResetPdu
        | RouterKeyPdu ASN Flags SKI BSL.ByteString
        | ErrorPdu ErrorCode (Maybe BSL.ByteString) (Maybe Text)
    deriving stock (Show, Eq, Ord, Generic)

newtype PduCode = PduCode Word8 
    deriving stock (Show, Eq, Ord, Generic)

data VersionedPdu = VersionedPdu Pdu ProtocolVersion
    deriving stock (Show, Eq, Ord, Generic)

newtype SerialNumber = SerialNumber Int32
    deriving stock (Show, Eq, Ord, Generic)

newtype RtrSessionId = RtrSessionId Int16
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


data PduHeader = PduHeader ProtocolVersion PduCode
    deriving stock (Show, Eq, Ord, Generic)

data PduParseError = 
      ParsedNothing ErrorCode Text 
    | ParsedOnlyHeader ErrorCode Text PduHeader 
    deriving stock (Show, Eq, Ord, Generic)

newtype Session = Session ProtocolVersion
    deriving stock (Show, Eq, Ord, Generic)

data Intervals = Intervals {
    refreshInterval :: Int32,
    retryInterval   :: Int32,
    expireInterval  :: Int32
} deriving stock (Show, Eq, Ord)


instance Binary RtrSessionId
instance Binary SerialNumber
instance Binary PduCode

-- Orphans
instance Binary SKI
instance Binary KI


defIntervals :: Intervals
defIntervals = Intervals { 
    refreshInterval = 3600,
    retryInterval   = 600,
    expireInterval  = 7200
}     

instance Binary ProtocolVersion where
    put f = put $ case f of 
        V0 -> 0 :: Word8
        V1 -> 1

    get = do 
        n :: Word8 <- get
        case n of 
            0 -> pure V0
            1 -> pure V1            
            _ -> fail $ "No error code value for " <> show n

instance Binary Flags where 
    put f = put $ case f of 
        Withdrawal   -> 0 :: Word8
        Announcement -> 1

    get = do 
        n :: Word8 <- get
        case n of 
            0 -> pure Withdrawal
            1 -> pure Announcement            
            _ -> fail $ "No flags value for " <> show n    


instance Binary Ipv4Prefix where 
    put prefix = do
        let (w0, w1, w2, w3) = prefixV4ToBytes prefix
        put w0
        put w1
        put w2
        put w3
        
    get = fail "Not implemented"

instance Binary Ipv6Prefix where 
    put prefix = do
        let (w0, w1, w2, w3) = prefixV6ToBytes prefix
        put w0
        put w1
        put w2
        put w3
        
    get = fail "Not implemented"

instance Binary ASN where 
    put (ASN a) = put a        
    get = ASN <$> get

instance Binary PrefixLength where 
    put (PrefixLength a) = put a        
    get = PrefixLength <$> get
        

errorCodes :: [(ErrorCode, Word16)]
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
    put code = let 
        Just n = lookup code errorCodes
        in put n

    get = do
        numeric <- get
        case filter ((== numeric) . snd) errorCodes of 
            [(c, _)] -> pure c
            _        -> fail $ "No error code value for " <> show numeric


instance Binary Intervals where         
    put Intervals {..} = do 
        put refreshInterval
        put retryInterval
        put expireInterval        

    get = Intervals <$> get <*> get <*> get
    
