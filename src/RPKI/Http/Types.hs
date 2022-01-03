{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module RPKI.Http.Types where

import           Data.Bifunctor (bimap)

import qualified Data.ByteString.Lazy as LBS

import           Data.Text                   (Text)
import qualified Data.Text                   as Text

import           Data.Text.Encoding          (encodeUtf8)

import           Data.Aeson as Json
import           Data.Aeson.Types

import           GHC.Generics                (Generic)
import qualified Data.Vector as V

import qualified Data.ByteString.Base16      as Hex

import           Servant.API
import           Network.HTTP.Media ((//))

import           RPKI.AppTypes
import           RPKI.Domain
import           RPKI.Orphans.Json
import           RPKI.Reporting
import           RPKI.Http.Messages

import           RPKI.Resources.Types
import           RPKI.Time
import           RPKI.Util (mkHash)


data ValidationsDto = ValidationsDto {
        version     :: WorldVersion,
        timestamp   :: Instant,
        validations :: [ValidationDto]
    } deriving stock (Generic)

data ValidationDto = ValidationDto {
        issues  :: [VIssue],
        path    :: [Text],
        url     :: Text
    } deriving stock (Generic)

data MetricsDto = MetricsDto {
        issues  :: [VIssue],
        path    :: [Text],
        url     :: Text
    } deriving stock (Generic)

data VrpDto = VrpDto {
        asn       :: ASN,
        prefix    :: IpPrefix,
        maxLength :: PrefixLength,
        ta        :: Text
    } deriving stock (Eq, Show, Generic)

newtype RObject = RObject (Located RpkiObject)
    deriving stock (Eq, Show, Generic)

parseHash :: Text -> Either Text Hash
parseHash hashText = bimap 
    (Text.pack . ("Broken hex: " <>) . show)
    mkHash
    $ Hex.decode $ encodeUtf8 hashText
        
data ManualCVS = ManualCVS

newtype RawCVS = RawCVS { unRawCSV :: LBS.ByteString }

instance Accept ManualCVS where
    contentType _ = "text" // "csv"

instance MimeRender ManualCVS RawCVS where
    mimeRender _ = unRawCSV    

instance ToJSON RObject
instance ToJSON VrpDto     

instance ToJSON ValidationsDto

instance ToJSON ValidationDto where
    toJSON ValidationDto {..} = object [         
            "url"       .= url,
            "full-path" .= path,
            "issues"    .= Array (V.fromList vIssues)
        ]
      where        
        vIssues = flip map issues $ \case
            VErr e             -> issueObj "error" e
            VWarn (VWarning e) -> issueObj "warning" e            
        issueObj t e = object [ 
                "type" .= (t :: Text), 
                "message" .= toMessage e 
            ]
        