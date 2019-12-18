{-# LANGUAGE OverloadedStrings #-}

module RPKI.Parse.Internal.SignedObject where
  
import qualified Data.ByteString as B

import Control.Applicative
import Data.Maybe

import Data.ASN1.Types
import Data.ASN1.Parse
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding

import Data.List.NonEmpty (NonEmpty ((:|)))

import Data.X509

import RPKI.Domain
import RPKI.Parse.Internal.Common
import RPKI.Parse.Internal.Cert

import qualified RPKI.Util as U

{- 
  https://tools.ietf.org/html/rfc6488#section-2

    contentType ContentType,
    content [0] EXPLICIT ANY DEFINED BY contentType

    SignedData ::= SEQUENCE {
        version CMSVersion,
        digestAlgorithms DigestAlgorithmIdentifiers,
        encapContentInfo EncapsulatedContentInfo,
        certificates [0] IMPLICIT CertificateSet OPTIONAL,
        crls [1] IMPLICIT RevocationInfoChoices OPTIONAL,
        signerInfos SignerInfos }

      DigestAlgorithmIdentifiers ::= SET OF DigestAlgorithmIdentifier

      SignerInfos ::= SET OF SignerInfo

      ContentType ::= OBJECT IDENTIFIER           
-}
parseSignedObject :: ParseASN1 a -> ParseASN1 (SignedObject a)
parseSignedObject eContentParse = 
  onNextContainer Sequence $ 
    SignedObject <$> parseContentType <*> parseContent
  where  
    parseContentType = getOID (pure . ContentType) "Wrong OID for contentType"
    parseContent = onNextContainer (Container Context 0) $ 
      onNextContainer Sequence $ 
        SignedData <$> parseVersion 
          <*> onNextContainer Set parseDigestAlgorithms
          <*> parseEncapContentInfo
          <*> parseEECertificate
          <*> onNextContainer Set (onNextContainer Sequence parseSignerInfo)        

    parseVersion = getInteger (pure . CMSVersion . fromInteger) "Wrong version"

    parseDigestAlgorithms = onNextContainer Sequence $
          DigestAlgorithmIdentifiers . catMaybes <$> getMany
            (getNext >>= \case
              OID oid -> pure $ Just  oid
              Null    -> pure Nothing
              s       -> throwParseError $ "DigestAlgorithms is wrong " <> show s)

    parseEncapContentInfo = onNextContainer Sequence $ do
      contentType <- parseContentType            
      onNextContainer (Container Context 0) $                 
          onNextContainer (Container Universal 4) (eContent contentType) <|> 
          eContent contentType
        where
          eContent contentType = do 
            fullContent <- getMany getNext
            let bs = B.concat [ os | OctetString os <- fullContent ]            
            case decodeASN1' BER bs of
              Left e     -> throwParseError $ "Couldn't decode embedded content: " <> show e
              Right asns ->  
                case runParseASN1 eContentParse asns of
                  Left e  -> throwParseError $ "Couldn't parse embedded ASN1 stream: " <> e
                  Right a -> pure $ EncapsulatedContentInfo contentType a
        

    parseEECertificate = onNextContainer (Container Context 0) $                  
      onNextContainer Sequence $ 
        getNextContainerMaybe Sequence >>= \case 
          Nothing   -> throwParseError "No EE certificate"
          Just asns -> 
            case runParseASN1 getObject asns of
              Left e              -> throwParseError $ show e
              Right eeCertificate -> do
                  sigAlgorithm <- parseSignatureAlgorithm
                  signature    <- parseSignature
                  let certWithSig = CertificateWithSignature eeCertificate sigAlgorithm signature encodedCert
                  case toResourceCert certWithSig of
                    Left e  -> throwParseError $ "EE certificate is broken" <> show e
                    Right c -> pure c
                  where 
                    encodedCert = encodeASN1' DER $ 
                      [Start Sequence] <> asns <> [End Sequence]                                

    parseSignerInfo = SignerInfos <$>
      parseVersion <*>
      parseSid <*>
      parseDigestAlgorithms <*>
      parseSignedAttributes <*>
      parseSignatureAlgorithm <*>
      parseSignature
      where 
        parseSid = getNext >>= \case 
          Other Context 0 si -> pure $ SignerIdentifier si
          s                  -> throwParseError $ "Unknown SID : " <> show s

        parseSignedAttributes = 
          getNextContainerMaybe (Container Context 0) >>= \case
            Nothing -> throwParseError "No signedAttributes"
            Just asns  -> 
              case runParseASN1 parseSA asns of
                Left e -> throwParseError $ show e
                Right attributes -> pure $ SignedAttributes attributes saEncoded
                where 
                  saEncoded = encodeASN1' DER $ [Start Set] <> asns <> [End Set]
                  parseSA = getMany $ 
                          onNextContainer Sequence $ getNext >>= \case 
                            OID attrId 
                              | attrId == id_contentType -> getNextContainerMaybe Set >>= \case 
                                      Just [OID ct] -> pure $ ContentTypeAttr $ ContentType ct
                                      s -> throwParseError $ "Unknown contentType: " <> show s
                              | attrId == id_messageDigest -> getNextContainerMaybe Set >>= \case
                                      Just [OctetString md] -> pure $ MessageDigest md
                                      s -> throwParseError $ "Unknown SID: " <> show s
                              | attrId == id_signingTime -> getNextContainerMaybe Set >>= \case
                                      Just [ASN1Time TimeUTC dt tz] -> pure $ SigningTime dt tz
                                      s -> throwParseError $ "Unknown Signing Time: " <> show s
                              | attrId == id_binarySigningTime -> getNextContainerMaybe Set >>= \case
                                      Just [IntVal i] -> pure $ BinarySigningTime i
                                      s -> throwParseError $ "Unknown Binary Signing Time: " <> show s
                              | otherwise -> getNextContainerMaybe Set >>= \case
                                      Just asn1 -> pure $ UnknownAttribute attrId asn1
                                      Nothing   -> pure $ UnknownAttribute attrId []

                            s -> throwParseError $ "Unknown signed attribute OID: " <> show s
                                            
    parseSignatureAlgorithm = SignatureAlgorithmIdentifier <$> getObject

getMetaFromSigned :: SignedObject a -> B.ByteString -> ParseResult (URI -> FullMeta)
getMetaFromSigned so bs = do
  let exts = getExts $ cwsX509certificate $ getEECert so
  case extVal exts id_subjectKeyId of
    Nothing -> Left . fmtErr $ "SKI is absent" 
    Just s  -> do
        ki <- parseKI s
        aki' <- case extVal exts id_authorityKeyId of
                  Nothing -> pure Nothing
                  Just a  -> Just . AKI <$> parseKI a
        pure $ 
          \location -> let 
            meta = RpkiMeta {        
                aki  = aki',
                hash = U.sha256s bs,
                locations = location :| []              
            } 
            in FullMeta meta (SKI ki)
    