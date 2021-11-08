{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE OverloadedStrings    #-}

module RPKI.Http.Types where

import           Data.Bifunctor (bimap)

import qualified Data.ByteString.Lazy as LBS

import           Data.Text                   (Text)
import qualified Data.Text                   as Text

import           Data.Text.Encoding          (encodeUtf8)

import           Data.Aeson                  hiding ((.=))
import           GHC.Generics                (Generic)

import qualified Data.ByteString.Base16      as Hex

import           Servant.API
import           Network.HTTP.Media ((//))

import           RPKI.Domain
import           RPKI.Orphans.Json
import           RPKI.Reporting

import           RPKI.Resources.Types
import           RPKI.Util (mkHash)


data ValidationResult = ValidationResult {
    problems :: [VProblem],
    context  :: [Text]
} deriving stock (Generic)

data VrpDto = VrpDto {
    asn       :: ASN,
    prefix    :: IpPrefix,
    maxLength :: PrefixLength,
    ta        :: Text
} deriving stock (Eq, Show, Generic)

newtype RObject = RObject (Located RpkiObject)
    deriving stock (Eq, Show, Generic)

instance ToJSON ValidationResult
instance ToJSON RObject
instance ToJSON VrpDto 

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