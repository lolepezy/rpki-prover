{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RPKI.Http.Dto where

import           Control.Lens
import           Control.Applicative

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Builder          as BB

import qualified Data.List.NonEmpty               as NonEmpty
import qualified Data.Set                         as Set
import qualified Data.Vector                      as V
import qualified Data.Map.Strict                  as Map
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Tuple.Strict
import           Data.Proxy
import           Data.Foldable

import qualified Data.X509 as X509
import qualified Crypto.PubKey.RSA.Types as RSA

import           RPKI.AppMonad
import           RPKI.AppState
import           RPKI.Domain
import           RPKI.Messages
import           RPKI.Resources.Resources
import qualified RPKI.Resources.IntervalContainers as IS
import           RPKI.Parse.Parse
import           RPKI.Time
import           RPKI.Reporting
import           RPKI.Http.Types
import           RPKI.Resources.Types
import           RPKI.Resources.Validity
import           RPKI.RTR.Types
import           RPKI.Validation.Types
import           RPKI.Util
import           RPKI.AppTypes (WorldVersion)
import qualified RPKI.Util.KeyMap as KeyMap

{-
    Mainly domain objects -> DTO convertions. 
-}

toVrpDtos :: PerTA Vrps -> [VrpDto]
toVrpDtos vrpsPerTa =     
    [ VrpDto {..} | 
        (TaName ta, Vrps vrps) <- perTA vrpsPerTa,
        Vrp asn prefix maxLength <- V.toList vrps ]

toVrpDto :: Vrp -> TaName -> VrpDto
toVrpDto (Vrp a p len) (TaName ta) = VrpDto a p len ta

toVrpV :: Maybe Vrps -> V.Vector AscOrderedVrp
toVrpV = maybe mempty uniqVrps

toVrpMinimalDtos :: Maybe Vrps -> [VrpMinimalDto]
toVrpMinimalDtos = map asDto . V.toList . toVrpV
  where
    asDto (AscOrderedVrp (Vrp asn prefix maxLength)) = VrpMinimalDto {..}


bgpSecToDto :: BGPSecPayload -> BgpCertDto
bgpSecToDto BGPSecPayload {..} = BgpCertDto {
        ski = bgpSecSki,
        asns = bgpSecAsns,
        subjectPublicKeyInfo = bgpSecSpki
    }

aspaToDto :: Aspa -> AspaDto
aspaToDto aspa =
    AspaDto {
        customer = aspa ^. #customer,
        providers = Set.toList $ aspa ^. #providers
}

gbrObjectToDto :: GbrObject -> GbrDto
gbrObjectToDto g = gbrToDto $ getCMSContent $ g ^. #cmsPayload

gbrToDto :: Gbr -> GbrDto
gbrToDto (Gbr vcardBS) = let    
    vcardProperty = \case 
        VCardVersion t -> ("Version", t) 
        FN t  -> ("FN", t)         
        ADR t -> ("ADR", t) 
        TEL t -> ("TEL", t) 
        EMAIL t -> ("EMAIL", t) 
    vcard = case parseVCard $ toNormalBS vcardBS of
            Left e                     -> Map.singleton "error" ("Invalid VCard: " <> e)
            Right (VCard ps, warnings) -> 
                Map.fromList (map vcardProperty ps) <>
                maybe mempty (Map.singleton "warnings") warnings
    in GbrDto {..}


validationsToDto :: WorldVersion -> Validations -> ValidationsDto OriginalVDto
validationsToDto version validations =
    ValidationsDto {
            worldVersion = version,
            timestamp    = versionToInstant version,
            validations  = toVDtos validations
        }              

toVDtos :: Validations -> [OriginalVDto]
toVDtos (Validations vMap) = 
    flip map (Map.toList vMap) $ \(Scope scope, issues) ->        
        OriginalVDto $ ValidationDto {
            issues = map toDto $ Set.toList issues,
            path   = NonEmpty.toList scope,
            url    = NonEmpty.head scope
        }
  where
    toDto = \case
        VErr e               -> ErrorDto $ toMessage e
        (VWarn (VWarning w)) -> WarningDto $ toMessage w


vrpDtosToCSV :: [VrpDto] -> RawCSV
vrpDtosToCSV vrpDtos =
    rawCSV
        (str "ASN,IP Prefix,Max Length,Trust Anchor\n")
        (mconcat $ map toBS vrpDtos)
  where
    toBS VrpDto {            
            maxLength = PrefixLength ml,
            ..
        } = str (show asn) <> ch ',' <>
            text (prefixStr prefix) <> ch ',' <>
            str (show ml) <> ch ',' <>
            str (convert ta) <> ch '\n'

vrpExtDtosToCSV :: [VrpExtDto] -> RawCSV
vrpExtDtosToCSV vrpDtos =
    rawCSV
        (str "URI,ASN,IP Prefix,Max Length,Trust Anchor\n")
        (mconcat $ map toBS vrpDtos)
  where
    toBS VrpExtDto {        
            vrp = VrpDto {
                maxLength = PrefixLength ml,
                ..
            },
            ..
        } = str (Text.unpack uri) <> ch ',' <>
            str (show asn) <> ch ',' <>
            text (prefixStr prefix) <> ch ',' <>
            str (show ml) <> ch ',' <>
            str (convert ta) <> ch '\n'


vrpSetToCSV :: Foldable f => f AscOrderedVrp -> RawCSV
vrpSetToCSV vrpDtos =
    rawCSV
        (str "ASN,IP Prefix,Max Length\n")
        (mconcat $ map toBS $ toList vrpDtos)
  where
    toBS (AscOrderedVrp (Vrp asn prefix (PrefixLength maxLength))) =
        str (show asn) <> ch ',' <>
        text (prefixStr prefix) <> ch ',' <>
        str (show maxLength) <> ch '\n'
 

toMftShortcutDto :: MftShortcut -> ManifestShortcutDto
toMftShortcutDto MftShortcut {..} = ManifestShortcutDto {..}
  where
    nonCrlChildren = KeyMap.map (\MftEntry {..} -> ManifestChildDto {..}) nonCrlEntries
    

objectToDto :: RpkiObject -> ObjectDto
objectToDto = \case
    CerRO c -> CertificateD $ objectDto c (certDto c) & #ski ?~ getSKI c
    CrlRO c -> CRLD $ objectDto c $ crlDto c    
    BgpRO b -> BGPSecD $ objectDto b (bgpSecDto b) & #ski ?~ getSKI b

    -- CMS-based stuff
    MftRO m  -> ManifestD $ cmsDto m $ manifestDto m
    RoaRO r  -> ROAD $ cmsDto r $ roaDto r
    SplRO r  -> SPLD $ cmsDto r $ splDto r
    GbrRO g  -> GBRD $ cmsDto g $ gbrObjectToDto g
    RscRO r  -> RSCD $ cmsDto r $ rscDto r
    AspaRO a -> ASPAD $ cmsDto a $ aspaDto a

  where
    objectDto o p = ObjectContentDto {
            ski  = Nothing,
            aki  = getAKI o,
            hash = getHash o,
            objectPayload = p,
            eeCertificate = Nothing
        }

    cmsDto c cmsPayload = let
            signedObject = unCMS $ c ^. #cmsPayload
            contentType  = signedObject ^. #soContentType
            signedData   = signedObject ^. #soContent
            cmsVersion         = signedData ^. #scVersion
            signedInfoVersion  = signedData ^. #scSignerInfos . #siVersion
            digestAlgorithms   = signedData ^. #scDigestAlgorithms
            signerIdentifier   = signedData ^. #scSignerInfos . #siSid
            signatureAlgorithm = signedData ^. #scSignerInfos . #signatureAlgorithm
            signature          = signedData ^. #scSignerInfos . #signature
            signedAttributes   = signedData ^. #scSignerInfos . #signedAttrs
            encapsulatedContentType = signedData ^. #scEncapContentInfo . #eContentType
        in objectDto c CMSObjectDto {..}
                & #eeCertificate ?~ certDto c
                & #ski ?~ getSKI c

    roaDto r = let
                vrps = getCMSContent $ r ^. #cmsPayload
                -- TODO Fix ROA somehow, make it NonEmpty?
                asn = head $ map (\(Vrp a _ _) -> a) vrps
                prefixes = map (\(Vrp _ p l) -> RoaPrefixDto p l) vrps
            in RoaDto {..}

    splDto r = let SplPayload asn prefixes = getCMSContent $ r ^. #cmsPayload 
                in SplPayloadDto {..}

    crlDto CrlObject {..} = let
            SignCRL {..} = signCrl
        in CrlDto { revokedSerials = Set.toList $ signCrl ^. #revokedSerials, .. }

    certDto c = let
            rawCert = getRawCert c

            AllResources (asRS -> ipv4) (asRS -> ipv6) (asRS -> asn) = rawCert ^. #resources
            x509cert = rawCert ^. #certX509 . #cwsX509certificate

            certVersion = Version $ fromIntegral $ X509.certVersion x509cert
            certSerial  = Serial $ X509.certSerial x509cert

            certIssuerDN = Text.pack $ show $ X509.certSignatureAlg x509cert
            certSubjectDN = Text.pack $ show $ X509.certSubjectDN x509cert

            certSignatureAlg = Text.pack $ show $ X509.certSignatureAlg x509cert

            notValidBefore = newInstant $ fst $ X509.certValidity x509cert
            notValidAfter  = newInstant $ snd $ X509.certValidity x509cert

            pubKey = case X509.certPubKey x509cert of
                        X509.PubKeyRSA RSA.PublicKey {..} -> Right $ let
                                pubKeySize = public_size
                                pubKeyPQ   = public_n
                                pubKeyExp  = public_e
                            in PubKeyDto {..}
                        _ -> Left $ Text.pack $ show $ X509.certPubKey x509cert

            extensions = getExtensions $ rawCert ^. #certX509

        in CertificateDto {..}
      where
        asRS = \case
            (RS s)  -> s
            Inherit -> IS.empty

        getExtensions cert =
            ExtensionsDto $ map mapExt $ getExtsSign cert
          where
            mapExt X509.ExtensionRaw {..} = let
                    oid      = OIDDto extRawOID
                    bytes    = extRawContent
                    critical = extRawCritical
                    value = case () of
                        _
                            | extRawOID == id_ce_keyUsage ->
                                strExt (Proxy :: Proxy X509.ExtKeyUsage) extRawContent
                            | extRawOID == id_ce_basicConstraints    ->
                                strExt (Proxy :: Proxy X509.ExtBasicConstraints) extRawContent
                            | extRawOID == id_ce_CRLDistributionPoints ->
                                maybe "undefined" unURI (extractCrlDistributionPoint extRawContent)
                            | extRawOID == id_ad_rpki_notify ->
                                urlText $ extractSiaValue extRawContent id_ad_rpki_notify
                            | extRawOID == id_pe_sia -> 
                                urlText $ extractSiaValue extRawContent id_ad_rpki_notify
                                      <|> extractSiaValue extRawContent id_ad_rpki_repository
                                      <|> extractSiaValue extRawContent id_ad_rpkiManifest
                            | extRawOID == id_pe_aia -> 
                                urlText $ extractSiaValue extRawContent id_ad_caIssuers
                                      <|> extractSiaValue extRawContent id_ad_rpki_notify
                                      <|> extractSiaValue extRawContent id_ad_rpki_repository
                                      <|> extractSiaValue extRawContent id_ad_rpkiManifest
                            | extRawOID == id_subjectKeyId ->                                 
                                    case runPureValidator (newScopes "id_subjectKeyId") $ parseKI extRawContent of 
                                        (Left e, _)   -> Text.pack $ "Could not parse SKI: " <> show e
                                        (Right ki, _) -> Text.pack $ show ki
                            | extRawOID == id_authorityKeyId ->                                 
                                    case runPureValidator (newScopes "id_subjectKeyId") $ parseKI extRawContent of 
                                        (Left e, _)   -> Text.pack $ "Could not parse AKI: " <> show e
                                        (Right ki, _) -> Text.pack $ show ki
                            | extRawOID == id_pe_ipAddrBlocks    -> "IP resources (see 'ipv4', 'ipv6' fields)"
                            | extRawOID == id_pe_autonomousSysIds    -> "ASN resources (see 'asn' field)"
                            | extRawOID == id_ce_certificatePolicies -> certificatePoliciesToText extRawContent
                                    
                            | otherwise -> "Unrecognised extension"

                in ExtensionDto {..}

            strExt :: forall a . (Show a, X509.Extension a) => Proxy a -> BS.ByteString -> Text
            strExt _ bytes = Text.pack $ show (X509.extDecodeBs bytes :: Either String a)

            urlText :: Maybe BS.ByteString -> Text
            urlText = \case 
                Nothing -> "undefined"
                Just bs -> either id unURI $ extractURI bs


    aspaDto = aspaToDto . getCMSContent . (^. #cmsPayload)

    rscDto r = let 
            rsc@Rsc {..} = getCMSContent $ r ^. #cmsPayload
        in RscDto { checkList = map (\(T2 f h) -> CheckListDto f h) $ rsc ^. #checkList, ..}

    bgpSecDto :: BgpCerObject -> BgpCertDto
    bgpSecDto bgpCert = let
            AllResources _ _ asns = getRawCert bgpCert ^. #resources
            bgpSecSpki = getSubjectPublicKeyInfo $ cwsX509certificate $ getCertWithSignature bgpCert
            bgpSecAsns = case asns of
                            Inherit -> []
                            RS r
                                | IS.null r -> []
                                | otherwise -> unwrapAsns $ IS.toList r
            bgpSecSki = getSKI bgpCert
        in bgpSecToDto BGPSecPayload {..}


manifestDto :: MftObject -> ManifestDto
manifestDto m = let
        mft@Manifest {..} = getCMSContent $ m ^. #cmsPayload
        entries = map (\(MftPair f h) -> (f, h)) mftEntries
    in
        ManifestDto {
            fileHashAlg = Text.pack $ show $ mft ^. #fileHashAlg,
            ..
        }

toValidityResultDto :: Instant -> ASN -> IpPrefix -> ValidityResult -> ValidityResultDto
toValidityResultDto 
    (isoFormat -> generatedTime) 
    origin_asn prefix validityResult = 
        ValidityResultDto { 
            validated_route = toRouteDto origin_asn prefix validityResult, 
            ..}
    

toRouteDto :: ASN -> IpPrefix -> ValidityResult -> ValidatedRouteDto
toRouteDto 
    origin_asn 
    (prefixStr -> prefix) 
    validityResult = ValidatedRouteDto {..}
  where
    route = RouteDto {..} 
    validity = ValidityDto {..}

    (state, vrps) = 
        case validityResult of 
            ValidOverall valids invalids -> ("valid",   allMatches valids invalids)
            InvalidOverall invalids      -> ("invalid", allMatches []     invalids)
            Unknown                      -> ("unknown", allMatches []     []      )
            
    allMatches valids invalids = ValidityVrpsDto {
            matched = vrpToMatch <$> valids,
            unmatched_as = [ vrpToMatch vrp | InvalidAsn vrp <- invalids ],
            unmatched_length = [ vrpToMatch vrp | InvalidLength vrp <- invalids ]
        }

    vrpToMatch (Vrp asn (prefixStr -> prefix) max_length) = MatchVrpDto {..}


toBulkResultDto :: Instant -> [T3 ASN IpPrefix ValidityResult] -> ValidityBulkResultDto
toBulkResultDto
    (isoFormat -> generatedTime) 
    (map (\(T3 asn prefix result) -> toRouteDto asn prefix result) -> results) = ValidityBulkResultDto {..}
  

rawCSV :: BB.Builder -> BB.Builder -> RawCSV
rawCSV header body = RawCSV $ BB.toLazyByteString $ header <> body

prefixStr :: IpPrefix -> Text
prefixStr (Ipv4P (Ipv4Prefix p)) = Text.pack $ show p
prefixStr (Ipv6P (Ipv6Prefix p)) = Text.pack $ show p

str :: String -> BB.Builder
str = BB.stringUtf8

text :: Text -> BB.Builder
text = str . Text.unpack

ch :: Char -> BB.Builder
ch  = BB.charUtf8