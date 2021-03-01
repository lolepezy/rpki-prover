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
    ParseE (ParseError t) -> t
    ValidationE v -> toValidationMessage v    
    RrdpE r  -> toRrdpMessage r
    RsyncE r -> toRsyncMessage r
    TAL_E (TALError t) -> t
    InitE (InitError t) -> t
    
    StorageE (StorageError t) -> t
    StorageE (DeserialisationError t) -> t
    
    UnspecifiedE context e -> 
        [i|Unspecified error #{context}, details: #{e}.|]


toRsyncMessage :: RsyncError -> Text
toRsyncMessage = \case 
    RsyncProcessError errorCode e ->
        [i|Rsync client returned code #{errorCode}, error = #{e}.|]

    FileReadError e ->
        [i|Can't read local file created by rsync client #{e}.|]

    RsyncRunningError e ->
        [i|Error running rsync client #{e}.|]

    RsyncDownloadTimeout t ->
        [i|Could not update repository in #{t}s.|]

    UnknownRsyncProblem e ->
        [i|Unknown problem with rsync #{e}.|]


toRrdpMessage :: RrdpError -> Text
toRrdpMessage = \case
    BrokenXml t    -> [i|XML parsing error: #{t}.|]
    BrokenSerial s -> [i|Malformed serial number: #{s}.|]
    NoSessionId    -> [i|Session ID is not set.|]
    NoSerial       -> [i|Serial number is not set.|]
    NoSnapshotHash -> [i|Snapshot hash is not set.|]
    NoSnapshotURI  -> [i|Snapshot URL is not set.|]
    NoDeltaSerial  -> [i|Delta serial is not set.|]
    NoDeltaURI     -> [i|Delta URL is not set.|]
    NoDeltaHash    -> [i|Delta hash is not set.|]
    BadHash h      -> [i|String #{h} is not a valid SHA256 hash.|]
    NoVersion      -> [i|RRDP version is not set.|]  
    BadVersion v   -> [i|String #{v} is not a valid RRDP version.|]  
    NoPublishURI   -> [i|An "publish" element doesn't have URL attribute.|]  

    BadBase64 base64 url -> [i|Base64 #{base64} for URL #{url} is invalid.|]  

    BadURL u -> [i|Unsupported or invalid URL #{u}.|]  

    NoHashInWithdraw -> [i|No "hash" attribute in a "withdraw" element.|]  

    ContentInWithdraw url content -> 
        [i|Content inside of "withdraw" element with url #{url}.|]  

    LocalSerialBiggerThanRemote local remote -> 
        [i|Local RRDP serial is #{local} higher than then remote #{remote}.|]  

    NonConsecutiveDeltaSerials deltaPairs ->           
        [i|Non-consecutive deltas: #{mconcat (map show deltaPairs)}.|]  

    CantDownloadFile e -> [i|Cannot download file: #{e}.|]  

    CantDownloadNotification e -> [i|Cannot download notification.xml: #{e}.|]  

    CantDownloadSnapshot e -> [i|Cannot download snapshot #{e}.|]  
    CantDownloadDelta e    -> [i|Cannot download delta #{e}.|]  

    SnapshotHashMismatch {..} -> 
        [i|Snapshot hash is #{actualHash} but required hash is #{expectedHash}.|]  

    SnapshotSessionMismatch {..} -> 
        [i|Snapshot session ID is #{actualSessionId} but required hash is #{expectedSessionId}.|]  

    SnapshotSerialMismatch {..} -> 
        [i|Snapshot serial is #{actualSerial} but required hash is #{expectedSerial}.|]  

    DeltaHashMismatch {..} -> 
        [i|Delta #{serial} hash is #{actualHash} but required hash is #{expectedHash}.|]  

    DeltaSessionMismatch {..} -> 
        [i|Delta's session ID is #{actualSessionId} but required session ID is #{expectedSessionId}.|]  

    DeltaSerialMismatch {..} -> 
        [i|Delta serial is #{actualSerial} but required serial is #{expectedSerial}.|]        

    DeltaSerialTooHigh {..} -> 
        [i|Delta serial #{actualSerial} is larger than maximal expected #{expectedSerial}.|]        
    
    NoObjectToReplace url hash -> 
        [i|No object with url #{url} and hash #{hash} to replace.|]        

    NoObjectToWithdraw url hash ->
        [i|No object with url #{url} and hash #{hash} to withdraw.|]        

    ObjectExistsWhenReplacing url hash -> 
        [i|Cannot replace object with url #{url}: object with hash #{hash} already exists.|]        

    UnsupportedObjectType url -> 
        [i|Unsupported object type #{url}.|]        
        
    RrdpDownloadTimeout t -> 
        [i|Could not update repository in #{t}s.|]        

    UnknownRrdpProblem e -> 
        [i|Unknown problem with RRDP: #{e}.|]  


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
