{-# LANGUAGE OverloadedStrings #-}

module RPKI.Http.Dto where

import qualified Data.ByteString.Builder          as BB

import qualified Data.List.NonEmpty               as NonEmpty
import qualified Data.Set                         as Set
import qualified Data.Vector                      as V
import qualified Data.Map.Strict                  as Map
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Tuple.Strict
import           Data.Foldable

import qualified Data.X509 as X509
import qualified Crypto.PubKey.RSA.Types as RSA

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
import          RPKI.AppTypes (WorldVersion)
import           RPKI.Store.Types (RpkiObjectLifecycle(..))

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

toVrpV :: Maybe Vrps -> V.Vector Vrp
toVrpV = maybe mempty (uniqVrpsBy cmpVrps)

toVrpMinimalDtos :: Maybe Vrps -> [VrpMinimalDto]
toVrpMinimalDtos = map asDto . V.toList . toVrpV
  where
    asDto (Vrp asn prefix maxLength) = VrpMinimalDto {..}


bgpSecToDto :: BGPSecPayload -> BgpCertDto
bgpSecToDto BGPSecPayload {..} = BgpCertDto {
        ski = bgpSecSki,
        asns = bgpSecAsns,
        subjectPublicKeyInfo = bgpSecSpki
    }

aspaToDto :: Aspa -> AspaDto
aspaToDto aspa =
    AspaDto {
        customer = aspa.customer,
        providers = Set.toList $ aspa.providers
}

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


vrpSetToCSV :: Foldable f => f Vrp -> RawCSV
vrpSetToCSV vrpDtos =
    rawCSV
        (str "ASN,IP Prefix,Max Length\n")
        (mconcat $ map toBS $ toList vrpDtos)
  where
    toBS (Vrp asn prefix (PrefixLength maxLength)) =
        str (show asn) <> ch ',' <>
        text (prefixStr prefix) <> ch ',' <>
        str (show maxLength) <> ch '\n'
 

toMftShortcutDto :: MftShortcut -> ManifestShortcutDto
toMftShortcutDto MftShortcut {..} = ManifestShortcutDto {..}
  where
    nonCrlChildren = Map.map (\MftEntry {..} -> ManifestChildDto {..}) nonCrlEntries
    

-- | Convert a lifecycle entry to a DTO.
--
-- 'OriginalRO' means parse/prevalidation failed, so only hash/type are available.
-- 'WellStructuredRO' means parsing succeeded, so expose a typed object DTO.
lifecycleToDto :: RpkiObjectLifecycle -> ObjectDto
lifecycleToDto (OriginalRO _ _ h t) = OriginalBlobD h t
lifecycleToDto (WellStructuredRO vro) = wellStructuredToDto vro

wellStructuredToDto :: WellStructuredRpkiObject -> ObjectDto
wellStructuredToDto = \case
    CerRO c -> validatedCaToDto c
    CrlRO c -> CRLD $ mkObjectContent c Nothing (crlDtoW c)
    BgpRO b -> BGPSecD $ mkObjectContent b (Just $ getSKI b) (bgpSecDtoW b)

    -- CMS-based minimized objects still carry enough information for payload DTOs.
    MftRO m  -> ManifestD $ mkCmsObjectContent m $ manifestDtoV m
    RoaRO r  -> ROAD $ mkCmsObjectContent r $ roaDtoW r
    SplRO s  -> SPLD $ mkCmsObjectContent s $ splDtoW s
    GbrRO g  -> GBRD $ mkCmsObjectContent g $ gbrToDto $ g.content
    RscRO r  -> RSCD $ mkCmsObjectContent r $ rscDtoW r
    AspaRO a -> ASPAD $ mkCmsObjectContent a $ aspaToDto $ a.content
  where
    mkObjectContent :: (WithHash o, WithAKI o) => o -> Maybe SKI -> payload -> ObjectContentDto payload
    mkObjectContent o ski payload =
        ObjectContentDto {
            hash = getHash o,
            ski = ski,
            aki = getAKI o,
            eeCertificate = Nothing,
            objectPayload = payload
        }

    mkCmsObjectContent :: WellStructuredCms payload -> cmsPayload -> ObjectContentDto (CMSObjectDto cmsPayload)
    mkCmsObjectContent cms payload =
        ObjectContentDto {
            hash = cms.hash,
            ski = Just $ cms.eeCert.ski,
            aki = Just $ cms.eeCert.aki,
            eeCertificate = Nothing,
            objectPayload = CMSObjectDto {
                contentType = signedDataContentType,
                encapsulatedContentType = signedDataContentType,
                digestAlgorithms = DigestAlgorithmIdentifiers [id_sha256],
                signatureAlgorithm = cms.eeCert.sigAlg,
                signerIdentifier = signerIdentifierFromSki cms.eeCert.ski,
                signature = cms.cmsSignature,
                signedAttributes = SignedAttributes [] (cms.signedAttrsBS),
                cmsPayload = payload
            }
        }

    signerIdentifierFromSki :: SKI -> SignerIdentifier
    signerIdentifierFromSki (SKI (KI skiBytes)) = SignerIdentifier skiBytes

    signedDataContentType :: ContentType
    signedDataContentType = ContentType [1, 2, 840, 113549, 1, 7, 2]

    roaDtoW :: WellStructuredCms VrpsPerAs -> RoaDto
    roaDtoW r = let
            VrpsPerAs asn v4s v6s = r.content
            prefixes = map (\(Vrp4 p l) -> RoaPrefixDto (Ipv4P p) l) v4s
                    <> map (\(Vrp6 p l) -> RoaPrefixDto (Ipv6P p) l) v6s
        in RoaDto {..}

    splDtoW :: WellStructuredCms SplPayload -> SplPayloadDto
    splDtoW r = let
            SplPayload asn prefixes = r.content
        in SplPayloadDto {..}

    crlDtoW :: CrlObject -> CrlDto
    crlDtoW CrlObject {..} = let
            SignCRL {..} = signCrl
        in CrlDto { revokedSerials = Set.toList $ signCrl.revokedSerials, .. }

    rscDtoW :: WellStructuredCms Rsc -> RscDto
    rscDtoW r = let
            rsc@Rsc {..} = r.content
        in RscDto { checkList = map (\(T2 f h) -> CheckListDto f h) $ rsc.checkList, .. }

    bgpSecDtoW :: WellStructuredBgpCert -> BgpCertDto
    bgpSecDtoW bgpCert = let
            AllResources _ _ asns = getResources bgpCert
            bgpSecSpki = getSubjectPublicKeyInfo bgpCert
            bgpSecAsns = case asns of
                            Inherit -> []
                            RS r
                                | IS.null r -> []
                                | otherwise -> unwrapAsns $ IS.toList r
            bgpSecSki = getSKI bgpCert
        in bgpSecToDto BGPSecPayload {..}

validatedCaToDto :: WellStructuredCert c -> ObjectDto
validatedCaToDto cert =
    let
        ValidityPeriod nb na = getValidityPeriod cert
        AllResources ipv4R ipv6R asnR = RPKI.Domain.getResources cert
        WellStructuredCert {
            pubKey = certPubKey,
            certUris = certificateUris,
            sigAlg = SignatureAlgorithmIdentifier signatureAlgorithm
        } = cert
        ipv4 = asRS ipv4R
        ipv6 = asRS ipv6R
        asn = asRS asnR
    in CertificateD $ ObjectContentDto {
        hash = getHash cert,
        ski = Just $ getSKI cert,
        aki = getAKI cert,
        eeCertificate = Nothing,
        objectPayload = CertificateDto {
            certVersion = Version 3,
            certSerial = getSerial cert,
            certSignatureAlg = Text.pack $ show signatureAlgorithm,
            certIssuerDN = "omitted in validated representation",
            certSubjectDN = "omitted in validated representation",
            notBefore = nb,
            notAfter = na,
            pubKey = pubKeyDto certPubKey,
            ipv4 = ipv4,
            ipv6 = ipv6,
            asn = asn,
            certificateUris = certificateUris,
            extensions = ExtensionsDto []
        }
    }
  where
    asRS = \case
        RS s    -> s
        Inherit -> IS.empty

    pubKeyDto = \case
        X509.PubKeyRSA RSA.PublicKey {..} -> Right $ PubKeyDto {
            pubKeySize = public_size,
            pubKeyPQ = public_n,
            pubKeyExp = public_e
        }
        other -> Left $ Text.pack $ show other

-- | 'manifestDto' variant for validated manifest objects.
manifestDtoV :: WellStructuredCms Manifest -> ManifestDto
manifestDtoV m = let
        mft@Manifest {..} = m.content
        entries = map (\(MftPair f h) -> (f, h)) mftEntries
    in
        ManifestDto {
            fileHashAlg = Text.pack $ show $ mft.fileHashAlg,
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

    vrpToMatch (Vrp asn (prefixStr -> vrpPrefix) max_length) = MatchVrpDto {
        prefix = vrpPrefix,
        ..
    }


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