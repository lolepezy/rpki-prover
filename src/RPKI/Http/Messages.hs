{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE QuasiQuotes           #-}

module RPKI.Http.Messages where

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.ByteString.Short       as BSS

import           Data.Text                   (Text)
import qualified Data.Text                   as Text

import qualified Data.List          as List
import qualified Data.List.NonEmpty          as NonEmpty

import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Set             (Set)
import qualified Data.Set             as Set

import Data.Maybe (maybeToList)

import           Data.ByteArray              (convert)
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Data.String.Interpolate.IsString

import qualified Crypto.PubKey.Curve25519    as C25519
import qualified Crypto.PubKey.Curve448      as C448
import           Crypto.PubKey.DSA           (Params (..), PublicKey (..))
import           Crypto.PubKey.ECC.Types
import qualified Crypto.PubKey.Ed25519       as E25519
import qualified Crypto.PubKey.Ed448         as E448
import           Crypto.PubKey.RSA.Types     (PublicKey (..))
import           Data.ASN1.BitArray
import           Data.ASN1.Types
import qualified Data.ByteString.Base16      as Hex
import qualified Data.ByteString.Base16.Lazy as HexLazy
import           Data.Hourglass
import           Data.X509                   as X509

import           RPKI.Domain                 as Domain
import           RPKI.Config
import           RPKI.CommonTypes

import           RPKI.Reporting
import           RPKI.Resources.IntervalSet
import           RPKI.Resources.Types
import           RPKI.Store.Base.Storable

import           RPKI.Store.Database
import           RPKI.Time
import qualified RPKI.Util                   as U


import RPKI.Http.Types



toMessage :: AppError -> Text
toMessage = \case
    ValidationE v -> toValidationMessage v
    other -> Text.pack $ show other    
--     TAL_E t   -> toTalMessage t
--     RrdpE r -> RrdpE _
--     RsyncE r -> RsyncE _
--     StorageE s -> StorageE _
    
--     InitE i -> InitE _
--     UnspecifiedE t t1 -> UnspecifiedE _ _


toValidationMessage :: ValidationError -> Text
toValidationMessage = \case      
      SPKIMismatch (EncodedBase64 talPKI) (EncodedBase64 actualPKI) -> 
          [i|Mismatch between subject public key info in the TAL #{talPKI} and the actual one #{actualPKI}.|]

      UnknownObjectAsTACert -> 
          [i|TA certificate is not a certificate, but some other object.|]

      ObjectIsTooSmall s -> [i|Object is too small (#{s} bytes) for a valid RPKI object.|]
      ObjectIsTooBig s   -> [i|Object is too big (#{s} bytes) for a valid RPKI object.|]

      InvalidSignature e -> [i|Object signature is invalid, error: #{e}.|]

      CMSSignatureAlgorithmMismatch sigEE sigAttr -> 
          [i|Signature algorithm on the EE certificate is #{sigEE} but the CSM attributes says #{sigAttr}.|]

      TACertAKIIsNotEmpty u -> [i|TA certificate #{u} has an AKI.|]

      CertNoPolicyExtension -> [i|Certificate has no policy extension.|]
          
      CertWrongPolicyExtension b -> [i|Certificate policy extension is broken: #{b}.|]

      NoMFT aki _ -> 
          [i|No manifest found for #{aki}.|]

      NoCRLOnMFT aki _ -> 
          [i|No CRL found on the manifest manifest found for AKI #{aki}.|]

      MoreThanOneCRLOnMFT aki locations entries ->
          [i|Multiple CRLs #{fmtMftEntries entries} found on the manifest manifest found for AKI #{aki} for CA #{fmtLocations locations}.|]

      NoMFTSIA locations -> 
          [i|No SIA pointing to the manifest on the certificate #{fmtLocations locations}.|]

      MFTOnDifferentLocation url locations -> 
          [i|Manifest location #{url} is not the same as SIA on the certificate #{fmtLocations locations}.|]

      BadFileNameOnMFT filename message -> 
            [i|File #{filename} is malformed #{message}.|]

      NoCRLExists aki locations -> 
            [i|No CRL exists with AKI #{aki} for CA #{fmtLocations locations}.|]

      CRLOnDifferentLocation crlDP locations -> 
          [i|CRL distribution point #{crlDP} is not the same as CRL location #{fmtLocations locations}.|]

      CRLHashPointsToAnotherObject hash locations -> 
          [i|CRL hash #{hash} points to different object for CA #{fmtLocations locations}.|]

      NextUpdateTimeNotSet -> 
          [i|Next update time is not set.|]

      NextUpdateTimeIsInThePast {..} -> 
          [i|Next update time #{nextUpdateTime} is in the past (current time is #{now}).|]

      ThisUpdateTimeIsInTheFuture {..} -> 
          [i|This update time #{thisUpdateTime} is in the future (current time is #{now}).|]

      RevokedResourceCertificate -> 
          [i|Object's EE certificate is revoked.|]

      CertificateIsInTheFuture {..} -> 
          [i|Certificate's 'not valid before' time #{before} is in the future.|]

      CertificateIsExpired {..} ->
          [i|Certificate is expired, its 'not valid after' time #{after} is in the past.|]

      (AKIIsNotEqualsToParentSKI childAKI parentSKI) ->
          [i|Certificate's AKI #{childAKI} is not the same as its parent's SKI #{parentSKI}.|]

      ManifestEntryDontExist hash filename -> 
          [i|Manifest entry #{filename} with hash #{hash} not found.|]

      OverclaimedResources resources -> 
          [i|Certificate (or EE) claims resources #{resources} not present on parent certificate.|]

      InheritWithoutParentResources -> 
          [i|Certificate has 'inherit' as resource set, but its parent doesn't have resources.|]

      UnknownUriType url -> 
          [i|URL type is neither rsync nor RRDP, #{url}.|]          

      CertificateDoesntHaveSIA -> 
          [i|Certificate doesn't have SIA with publication point.|]

      CircularReference hash locations ->
          [i|Object with hash #{hash} and location #{fmtLocations locations} creates reference cycle.|]

      ManifestLocationMismatch filename locations -> 
          [i|Object has manifest entry #{filename}, but was found at the different location #{fmtLocations locations}.|]

      InvalidVCardFormatInGbr e -> [i|InvalidVCard format: #[e}.|]

      RoaPrefixIsOutsideOfResourceSet roaPrefix resources -> 
          [i|ROA prefix #{roaPrefix} is not inside of the EE certificate resources #{resources}.|]

      RoaPrefixLenghtsIsBiggerThanMaxLength (Vrp _ prefix maxLength) -> 
          [i|VRP is malformed, length of the prefix #{prefix} is bigger than #{maxLength}.|]
  where
    fmtMftEntries = mconcat . 
                    List.intersperse "," . 
                    map (\(T2 t h) -> t <> Text.pack (":" <> show h))


fmtLocations :: Locations -> Text
fmtLocations = mconcat . 
               List.intersperse "," . 
               map (Text.pack . show) . 
               NonEmpty.toList
