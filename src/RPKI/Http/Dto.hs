{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RPKI.Http.Dto where

import           Control.Lens

import qualified Data.ByteString.Builder          as BS

import qualified Data.List.NonEmpty               as NonEmpty
import qualified Data.Set                         as Set
import qualified Data.Map.Monoidal.Strict         as MonoidalMap
import qualified Data.Text                       as Text
import           Data.Tuple.Strict

import qualified Data.X509 as X509
import qualified Crypto.PubKey.RSA.Types as RSA

import           RPKI.AppState
import           RPKI.Domain
import           RPKI.Messages
import           RPKI.Resources.Resources
import           RPKI.Resources.IntervalSet as IS
import           RPKI.Parse.Parse
import           RPKI.Time
import           RPKI.Reporting
import           RPKI.Http.Types
import           RPKI.Resources.Types
import           RPKI.RTR.Types
import           RPKI.Util

{-
    Mainly domain objects -> DTO convertions. 
-}

toVrpDtos :: Maybe Vrps -> [VrpDto]
toVrpDtos = \case
    Nothing   -> []
    Just vrps -> [ VrpDto a p len (unTaName ta) |
                        (ta, vrpSet) <- MonoidalMap.toList $ unVrps vrps,
                        Vrp a p len  <- Set.toList vrpSet ]

toVrpSet :: Maybe Vrps -> Set.Set AscOrderedVrp
toVrpSet = maybe mempty uniqVrps

toVrpMinimalDtos :: Maybe Vrps -> [VrpMinimalDto]
toVrpMinimalDtos = map asDto . Set.toList . toVrpSet
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
        customerAsn = aspa ^. #customerAsn,
        providerAsns = map (\(asn, afiLimit) -> ProviderAsn {..}) $ aspa ^. #providerAsns
    }

toVR :: (Scope a, Set.Set VIssue) -> FullVDto
toVR (Scope scope, issues) = FullVDto {
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
            asn = ASN as,
            maxLength = PrefixLength ml,
            ..
        } = str "AS" <> str (show as) <> ch ',' <>
            str (prefixStr prefix) <> ch ',' <>
            str (show ml) <> ch ',' <>
            str (convert ta) <> ch '\n'

vrpSetToCSV :: Set.Set AscOrderedVrp -> RawCSV
vrpSetToCSV vrpDtos =
    rawCSV 
        (str "ASN,IP Prefix,Max Length\n")
        (mconcat $ map toBS $ Set.toList vrpDtos)
  where
    toBS (AscOrderedVrp (Vrp (ASN asn) prefix (PrefixLength maxLength))) = 
        str "AS" <> str (show asn) <> ch ',' <>
        str (prefixStr prefix) <> ch ',' <>
        str (show maxLength) <> ch '\n'


rawCSV :: BS.Builder -> BS.Builder -> RawCSV
rawCSV header body = RawCSV $ BS.toLazyByteString $ header <> body
    

prefixStr :: IpPrefix -> String
prefixStr (Ipv4P (Ipv4Prefix p)) = show p
prefixStr (Ipv6P (Ipv6Prefix p)) = show p

str :: String -> BS.Builder
str = BS.stringUtf8

ch :: Char -> BS.Builder
ch  = BS.charUtf8    

objectToDto :: RpkiObject -> ObjectDto
objectToDto = \case 
    CerRO c -> CertificateD $ objectDto c (certDto c) & #ski ?~ getSKI c
    CrlRO c -> CRLD $ objectDto c $ crlDto c
    BgpRO b -> BGPSecD $ objectDto b (bgpSecDto b) & #ski ?~ getSKI b

    -- CMS-based stuff
    MftRO m  -> ManifestD $ cmsDto m $ manifestDto m
    RoaRO r  -> ROAD $ cmsDto r $ roaDto r
    GbrRO g  -> GBRD $ cmsDto g $ gbrDto g
    RscRO r  -> RSCD $ cmsDto r $ rscDto r
    AspaRO a -> ASPAD $ cmsDto a $ aspaDto a
    
  where
    objectDto o p = ObjectContentDto {
            ski  = Nothing,
            aki  = getAKI o,
            hash = getHash o,
            payload = p,
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
                & #eeCertificate ?~ eeCertDto c
                & #ski ?~ getSKI c

    manifestDto m = let
            mft@Manifest {..} = getCMSContent $ m ^. #cmsPayload
            entries = map (\(T2 f h) -> (f, h)) mftEntries 
        in 
            ManifestDto {
                fileHashAlg = Text.pack $ show $ mft ^. #fileHashAlg,
                ..
            }

    gbrDto g = GrbDto {}
    roaDto r = RoaDto {}
    crlDto c = CrlDto { serials = [] }
    rscDto c = RscDto {}

    eeCertDto = certDto

    certDto c = let             
            rawCert = getRawCert c

            AllResources (asRS -> ipv4) (asRS -> ipv6) (asRS -> asn) = rawCert ^. #resources
            x509cert = rawCert ^. #certX509 . #cwsX509certificate

            certVersion = Version $ fromIntegral $ x509cert ^. #certVersion
            certSerial  = Serial $ x509cert ^. #certSerial

            certIssuerDN = Text.pack $ show $ x509cert ^. #certSignatureAlg
            certSubjectDN = Text.pack $ show $ x509cert ^. #certSubjectDN

            certSignatureAlg = Text.pack $ show $ x509cert ^. #certSignatureAlg
            
            notValidBefore  = Instant $ fst $ x509cert ^. #certValidity
            notValidAfter  = Instant $ snd $ x509cert ^. #certValidity

            pubKey = case x509cert ^. #certPubKey of 
                        X509.PubKeyRSA RSA.PublicKey {..} -> Right $ let 
                                pubKeySize = public_size
                                pubKeyPQ   = public_n
                                pubKeyExp  = public_e
                            in PubKeyDto {..}
                        _ -> Left $ Text.pack $ show $ x509cert ^. #certPubKey
            
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

                    value = 
                        case () of 
                            _ 
                                | extRawOID == id_ce_keyUsage         -> "id_ce_keyUsage"
                                | extRawOID == id_ce_basicConstraints -> "id_ce_basicConstraints"
                                | otherwise                           -> "Don't know"

                in ExtensionDto {..}

    aspaDto = aspaToDto . getCMSContent . (^. #cmsPayload)

    

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
        in bgpSecToDto $ BGPSecPayload {..}


