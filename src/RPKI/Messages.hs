{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}

module RPKI.Messages where

import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.List                   as List
import qualified Data.List.NonEmpty          as NonEmpty

import           Data.Hourglass
import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set
import           Data.String.Interpolate.IsString

import           Data.ASN1.Types (OID)

import           RPKI.Domain                 as Domain
import           RPKI.Reporting
import           RPKI.Util (fmtLocations, hex)



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

    SlurmE r    -> toSlurmMessage r
    InternalE t -> toInternalErrorMessage t
    
    UnspecifiedE context e -> 
        [i|Unspecified error in #{context}, details: #{e}.|]


toRsyncMessage :: RsyncError -> Text
toRsyncMessage = \case 
    RsyncProcessError errorCode e ->
        [i|Rsync client returned code #{errorCode}, error = #{e}|]

    FileReadError e                -> [i|Can't read local file created by rsync client #{e}.|]
    RsyncRunningError e            -> [i|Error running rsync client #{e}.|]
    RsyncDownloadTimeout t         -> [i|Could not update repository in #{t}.|]
    RsyncFailedToParseObject url   -> [i|Failed to parse object #{url}.|]             
    RsyncUnsupportedObjectType url -> [i|Unsupported object type #{url}.|]             
    UnknownRsyncProblem e          -> [i|Unknown problem with rsync #{e}.|]

toRrdpMessage :: RrdpError -> Text
toRrdpMessage = \case
    BrokenXml t    -> [i|XML parsing error: #{t}.|]
    BrokenSerial s -> [i|Malformed serial number: #{s}.|]
    NoSessionId    -> [i|Session ID is not set.|]
    NoSerial       -> [i|Serial number is not set.|]
    NoSnapshotHash -> [i|Snapshot hash is not set.|]
    NoSnapshotURI  -> [i|Snapshot URL is not set.|]
    
    BrokenSnapshotUri u -> 
        [i|Snapshot URL in notification url is malformed #{u}.|]

    SnapshotUriHostname repoHost snapshoHost -> 
        [i|Snapshot URL hostname #{snapshoHost} is not the same as repository hostname #{repoHost}.|]

    NoDeltaSerial  -> [i|Delta serial is not set.|]
    NoDeltaURI     -> [i|Delta URL is not set.|]
    NoDeltaHash    -> [i|Delta hash is not set.|]

    BrokenDeltaUri u -> 
        [i|Delta URL in notification url is malformed #{u}.|]

    DeltaUriHostname repoHost deltaHost -> 
        [i|Delta URL hostname #{deltaHost} is not the same as repository hostname #{repoHost}.|]

    BadHash h      -> [i|String #{h} is not a valid SHA256 hash.|]
    NoVersionInNotification -> [i|RRDP version is not set in the notification.xml file.|]  
    NoVersionInSnapshot     -> [i|RRDP version is not set in snapshot.|]  
    NoVersionInDelta        -> [i|RRDP version is not set in delta.|]  
    BadVersion v   -> [i|String #{v} is not a valid RRDP version.|]  
    NoPublishURI   -> [i|An "publish" element doesn't have URL attribute.|]  

    BadBase64 base64 -> [i|Base64 #{base64} is invalid.|]  

    BadURL u -> [i|Unsupported or invalid URL #{u}.|]  

    NoHashInWithdraw -> [i|No "hash" attribute in a "withdraw" element.|]  

    ContentInWithdraw url _ -> 
        [i|Content inside of "withdraw" element with url #{url}.|]  

    LocalSerialBiggerThanRemote local remote -> 
        [i|Local RRDP serial is #{local} higher than the remote #{remote}.|]  

    NonConsecutiveDeltaSerials deltaPairs ->           
        [i|Non-consecutive deltas: #{mconcat (map show deltaPairs)}.|]  

    CantDownloadFile e -> [i|Cannot download file: #{e}.|]  

    CantDownloadNotification e -> [i|Cannot download notification.xml: #{e}.|]  

    CantDownloadSnapshot e -> [i|Cannot download snapshot #{e}.|]  
    CantDownloadDelta e    -> [i|Cannot download delta #{e}.|]  
    FailedToParseSnapshotItem e -> [i|Cannot process a snapshot item #{e}.|]  
    FailedToParseDeltaItem e    -> [i|Cannot process a delta item #{e}.|]  

    SnapshotHashMismatch {..} -> 
        [i|Snapshot hash is #{actualHash} but required hash is #{expectedHash}.|]  

    SnapshotSessionMismatch {..} -> 
        [i|Snapshot session ID is #{actualSessionId} but required session ID is #{expectedSessionId}.|]  

    SnapshotSerialMismatch {..} -> 
        [i|Snapshot serial is #{actualSerial} but required serial is #{expectedSerial}.|]  

    DeltaHashMismatch {..} -> 
        [i|Delta #{serial} hash is #{actualHash} but required hash is #{expectedHash}.|]  

    DeltaSessionMismatch {..} -> 
        [i|Delta's session ID is #{actualSessionId} but required session ID is #{expectedSessionId}.|]  

    DeltaSerialMismatch {..} -> 
        [i|Delta serial is #{actualSerial} but required serial is #{expectedSerial}.|]        

    DeltaSerialTooHigh {..} -> 
        [i|Delta serial #{actualSerial} is larger than maximal expected #{expectedSerial}.|]        
    
    RrdpMetaMismatch {..} -> 
        [i|RRDP metadata mismatch while saving data, expected session #{expectedSessionId} and serial #{expectedSerial} |] <> 
        [i|but got session #{actualSessionId} and serial #{actualSerial}.|]

    NoObjectToReplace url hash -> 
        [i|No object with url #{url} and hash #{hash} to replace.|]        

    NoObjectToWithdraw url hash ->
        [i|No object with url #{url} and hash #{hash} to withdraw.|]        

    ObjectExistsWhenReplacing url hash -> 
        [i|Cannot replace object with url #{url}: object with hash #{hash} already exists.|]        

    RrdpUnsupportedObjectType url -> 
        [i|Unsupported object type #{url}.|]        
        
    RrdpDownloadTimeout t -> 
        [i|Could not update repository in #{t}.|]        

    UnknownRrdpProblem e -> 
        [i|Unknown problem with RRDP: #{e}.|]  


toValidationMessage :: ValidationError -> Text
toValidationMessage = \case      
      SPKIMismatch (SPKI (EncodedBase64 talPKI)) (SPKI (EncodedBase64 actualPKI)) -> 
          [i|Mismatch between subject public key info in the TAL #{talPKI} and the actual one #{actualPKI}.|]

      UnknownObjectAsTACert -> 
          [i|TA certificate is not a certificate, but some other object.|]

      ObjectIsTooSmall s -> [i|Object is too small (#{s} bytes) for a valid RPKI object.|]
      ObjectIsTooBig s   -> [i|Object is too big (#{s} bytes) for a valid RPKI object.|]

      InvalidSignature e -> [i|Object signature is invalid, error: #{e}.|]
      InvalidKI e       -> [i|Certificate SKI is invalid, error: #{e}.|]

      CMSSignatureAlgorithmMismatch sigEE sigAttr -> 
          [i|Signature algorithm on the EE certificate is #{sigEE} but the CSM attributes says #{sigAttr}.|]

      NoValidatedVersion -> "No tree validations has been run, cannot validate the object"

      NoAKI -> "No AKI found"
      ParentCertificateNotFound  -> "Could not find parent ceertificate for the object"
      ObjectNotOnManifest        -> "Object is not listed on the parent manifest"

      UnsupportedHashAlgorithm digest -> [i|Unsupported hashing algorithm #{digest}.|]
      NotFoundOnChecklist hash file -> [i|File #{file} with hash #{hash} is not found on the checklist.|]
      ChecklistFileNameMismatch hash f1 f2 -> [i|File with hash #{hash} has name #{f1} but marked as #{f2} on the checklist.|]

      TACertAKIIsNotEmpty u -> [i|TA certificate #{u} has an AKI.|]
      
      TACertPreferCachedCopy TACertValidities {..} -> 
          [i|New TA certificate has validity period of #{notBefore}-#{notAfter}, previous one has #{cachedNotBefore}-#{cachedNotAfter}. |] <>
          [i|Will use previous TA certificate. NOTE: this means something really bad happened to the TA, consider stopping to use it at all.|]

      CertNoPolicyExtension -> [i|Certificate has no policy extension.|]
          
      CertBrokenExtension oid b -> [i|Certificate extension #{fmtOID oid} is broken: #{b}.|]
      UnknownCriticalCertificateExtension oid b -> [i|Unknown critical certificate extension, OID: #{fmtOID  oid}, content #{b}.|]
      MissingCriticalExtension oid -> [i|Missing critical certificate extension #{fmtOID oid}.|]
      BrokenKeyUsage t -> [i|Broken keyUsage extension: #{t}.|]

      ObjectHasMultipleLocations locs -> 
          [i|The same object has multiple locations #{fmtUrlList locs}, this is suspicious.|]

      NoMFT (AKI aki) -> 
          [i|No manifest found for AKI #{aki}.|]

      NoMFTButCachedMft (AKI aki) -> 
          [i|No manifest found for AKI #{aki}, but there's a cached version of it.|]

      NoCRLOnMFT (AKI aki) -> 
          [i|No CRL found on the manifest manifest found for AKI #{aki}.|]

      MoreThanOneCRLOnMFT aki entries ->
          [i|Multiple CRLs #{fmtMftEntries entries} found on the manifest manifest found for AKI #{aki} for the CA.|]

      NoMFTSIA -> 
          [i|No SIA pointing to the manifest on the certificate.|]

      MFTOnDifferentLocation url locations -> 
          [i|Manifest location #{url} is not the same as SIA on the certificate #{fmtLocations locations}.|]

      BadFileNameOnMFT filename message -> 
            [i|File #{filename} is malformed #{message}.|]

      ZeroManifestEntries -> 
            [i|Manifest doesn't contain any entries.|]

      NonUniqueManifestEntries nonUniqueEntries -> 
            [i|File #{fmtBrokenMftEntries nonUniqueEntries}.|]

      NoCRLExists aki hash -> 
            [i|No CRL exists with AKI #{aki} and hash #{hash} for CA.|]

      ManifestEntryDoesn'tExist hash filename -> 
          [i|Manifest entry #{filename} with hash #{hash} not found.|]

      ManifestEntryHasWrongFileType hash filename type_ ->         
        [i|Manifest entry #{filename} with hash #{hash} points to an object that has type #{type_}.|]

      ManifestNumberDecreased {..} ->
        [i|Manifest number #{newMftNumber} is smaller than the previous #{oldMftNumber}, |] <> 
        [i|will fall back to the previous one.|]

      MftFallback e failedMftNumber -> 
        [i|Fallback to the last valid manifest happened, manifest #{failedMftNumber} failed with the error: #{toMessage e}|]

      CRLOnDifferentLocation crlDP locations -> 
          [i|CRL distribution point #{crlDP} is not the same as CRL location #{fmtLocations locations}.|]

      CRL_AKI_DifferentFromCertSKI parentSki crlAki -> 
          [i|The AKI of the CRL #{crlAki} is different from SKI of the parent certificate #{parentSki}.|]

      CRLHashPointsToAnotherObject hash -> 
          [i|CRL hash #{hash} points to different object for CA.|]

      NextUpdateTimeNotSet -> 
          [i|Next update time is not set.|]

      NextUpdateTimeIsInThePast {..} -> 
          [i|Next update time #{nextUpdateTime} is in the past (current time is #{now}).|]

      ThisUpdateTimeIsInTheFuture {..} -> 
          [i|This update time #{thisUpdateTime} is in the future (current time is #{now}).|]

      NextUpdateTimeBeforeThisUpdateTime {..} -> 
          [i|Next update time #{nextUpdateTime} is before this update time #{thisUpdateTime}.|]

      RevokedResourceCertificate -> 
          [i|Object's EE certificate is revoked.|]

      ObjectValidityIsInTheFuture {..} -> 
          [i|Object's 'not valid before' time #{before} is in the future.|]

      ObjectIsExpired {..} ->
          [i|Object is expired, its 'not valid after' time #{after} is in the past.|]

      AKIIsNotEqualsToParentSKI childAKI parentSKI ->
          [i|Certificate's AKI #{childAKI} is not the same as its parent's SKI #{parentSKI}.|]

      OverclaimedResources resources -> 
          [i|Certificate (or EE) claims resources #{resources} not present on parent certificate.|]

      InheritWithoutParentResources -> 
          [i|Certificate has 'inherit' as resource set, but its parent doesn't have resources.|]

      ResourceSetMustBeInherit -> [i|Resources set must be 'inherit'.|]

      UnknownUriType url -> [i|URL type is neither rsync nor RRDP, #{url}.|]          
      BrokenUri url e    -> [i|Error #{e} parsing URL #{url}.|]          

      CertificateDoesntHaveSIA -> 
          [i|Certificate doesn't have SIA with publication point.|]

      AIANotSameAsParentLocation aiaUrl locations -> 
          [i|AIA of the child #{aiaUrl} does not point to the real parent location #{fmtLocations locations}.|]          

      CircularReference hash ->
          [i|Object with hash #{hash} creates reference cycle.|]

      CertificatePathTooDeep maxDepth ->
          [i|The CA tree reached maximum depth of #{maxDepth}.|]

      TreeIsTooBig maxTreeSize ->          
          [i|The number of object in CA tree reached maximum of #{maxTreeSize}.|]

      TooManyRepositories maxTaRepositories ->          
          [i|The number of new repositories added by one TA reached maximum of #{maxTaRepositories}.|]

      ValidationTimeout (Seconds maxDuration) -> 
          [i|Validation did not finish within #{maxDuration}s and was interrupted.|]

      ManifestLocationMismatch filename locations -> 
          [i|Object has manifest entry #{filename}, but was found at the different location #{fmtLocations locations}.|]

      InvalidVCardFormatInGbr e -> [i|Broken VCard or invalid VCard properties: #{e}.|]

      RoaPrefixIsOutsideOfResourceSet roaPrefix resources -> 
          [i|ROA prefix #{roaPrefix} is not inside of the EE certificate resources #{resources}.|]

      RoaPrefixLenghtsIsBiggerThanMaxLength (Vrp _ prefix maxLength) -> 
          [i|VRP is malformed, length of the prefix #{prefix} is bigger than #{maxLength}.|]

      AspaOverlappingCustomerProvider customer providers -> 
        [i|ASPA contains customer ASN #{customer} in the list of provider ASNs #{providers}.|]

      BGPCertSIAPresent bs -> 
        [i|SIA extension is present on the BGPSec certificate: #{hex bs}.|]

      BGPCertIPv4Present -> [i|IPv4 extension is present on the BGPSec certificate.|]
      BGPCertIPv6Present -> [i|IPv6 extension is present on the BGPSec certificate.|]
      BGPCertBrokenASNs  -> [i|AS extension is not present on the BGPSec certificate.|]

      AspaNoAsn       -> [i|ASN extension is not present on the ASPA EE certificate or has 'inherit' value.|]
      AspaIPv4Present -> [i|IPv4 extension is present on the ASPA EE certificate.|]
      AspaIPv6Present -> [i|IPv6 extension is present on the ASPA EE certificate.|]      
      AspaAsNotOnEECert customer eeAsns -> 
        [i|Customer ASN (#{customer}) is not in the EE certificate AS set (#{eeAsns}).|]      
    
      SplAsnNotInResourceSet asn asns ->
        [i|#{asn} is not in the EE certificate AS set (#{asns}).|]      

      SplNotIpResources prefixes -> 
        [i|Prefix list must not have IP resources on its EE certificate, but has #{prefixes}.|]

      ReferentialIntegrityError message -> [i|Referential integrity problem: #{message}.|]

      WeirdCaPublicationPoints urls -> 
        [i|Invalid CA publication points found: #{fmtUrlList urls}.|]

  where
    fmtUrlList = mconcat . 
                 List.intersperse "," . map (show . getURL)

    fmtMftEntries = mconcat . 
                    List.intersperse "," . 
                    map (\(MftPair t h) -> t <> Text.pack (":" <> show h))

    fmtBrokenMftEntries = mconcat . 
                    List.intersperse "," . 
                    map (\(h, fs) -> Text.pack $ "Hash: " <> show h <> " -> " <> show fs)


toSlurmMessage :: SlurmError -> Text
toSlurmMessage = \case 
    SlurmFileError file t  -> [i|Failed to read SLURM file #{file}: #{t}.|]
    SlurmParseError file t -> [i|Failed to parse SLURM file #{file}: #{t}.|]
    SlurmValidationError t -> [i|Invalid SLURM file(s): #{t}.|]

toInternalErrorMessage :: InternalError -> Text
toInternalErrorMessage = \case 
    InternalError t      -> t
    WorkerTimeout t      -> t
    WorkerOutOfCpuTime t -> t
    WorkerOutOfMemory t  -> t
    WorkerDetectedDifferentExecutable t  -> t

fmtOID :: OID -> Text
fmtOID oid = Text.intercalate "." $ map (Text.pack . show) oid



formatValidations :: Validations -> Text
formatValidations (Validations vs) = 
    mconcat 
    $ List.intersperse "\n"  
    [ formatForObject s issues | (Scope s, issues) <- Map.toList vs ]
  where    
    formatForObject s issues = 
        [i|#{focusToText $ NonEmpty.head s}:
#{issuesText}|]
      where
        issuesText :: Text = mconcat 
            $ List.intersperse "\n" 
            $ map (\is -> [i|    #{formatIssue is}|]) 
            $ Set.toList issues

formatIssue :: VIssue -> Text
formatIssue (VErr e) = toMessage e
formatIssue (VWarn (VWarning e)) = toMessage e
