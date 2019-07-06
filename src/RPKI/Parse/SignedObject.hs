{-# LANGUAGE OverloadedStrings #-}

module RPKI.Parse.SignedObject where
  
import qualified Data.ByteString as B

import Control.Applicative
import Data.Maybe

import Data.ASN1.BitArray
import Data.ASN1.Types
import Data.ASN1.Parse
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding

import Data.List.NonEmpty       (NonEmpty ((:|)))

import Data.X509

import RPKI.Domain
import RPKI.SignTypes
import RPKI.Parse.Common

import qualified RPKI.Util as U

{- 

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
        SignedContent <$> 
          parseVersion <*> onNextContainer Set parseDigestAlgorithms <*>
          parseEncapContentInfo <*>
          parseEECertificate <*>
          onNextContainer Set (onNextContainer Sequence parseSignerInfo)

    parseVersion = getInteger (pure . CMSVersion . fromInteger) "Wrong version"

    parseDigestAlgorithms = onNextContainer Sequence $
          DigestAlgorithmIdentifiers . catMaybes <$> getMany
            (getNext >>= \case
              OID oid -> pure $ Just  oid
              Null    -> pure Nothing
              s       -> throwParseError $ "DigestAlgorithms is wrong " ++ show s)

    parseEncapContentInfo = onNextContainer Sequence $ do
      contentType <- parseContentType            
      onNextContainer (Container Context 0) $                 
          (onNextContainer (Container Universal 4) (eContent contentType)) <|> 
          (eContent contentType)
        where
          eContent contentType = do 
            fullContent <- getMany getNext
            let bs = B.concat [ os | OctetString os <- fullContent ]
            case decodeASN1' BER bs of
              Left e     -> throwParseError $ "Couldn't decode embedded content: " ++ show e
              Right asns ->  
                case runParseASN1 eContentParse asns of
                  Left e  -> throwParseError $ "Couldn't parse embedded ASN1 stream: " ++ e
                  Right a -> pure $ EncapsulatedContentInfo contentType a    
        

    parseEECertificate = 
      onNextContainer (Container Context 0) $ 
        onNextContainer Sequence $
            CertificateWithSignature <$> 
              (onNextContainer Sequence getObject) <*>
              parseSignatureAlgorithm <*>
              parseSignature                                  
                                

    parseSignerInfo = SignerInfos <$>
      parseVersion <*>
      parseSid <*>
      parseDigestAlgorithms <*>
      parseSignedAttributes <*>
      parseSignatureAlgorithm <*>
      parseSignature
      where 
        parseSid = getNext >>= \case 
          Other Context 0 ki -> pure $ SignerIdentifier $ SKI $ KI ki
          s                  -> throwParseError $ "Unknown SID : " ++ show s

        parseSignedAttributes = onNextContainer (Container Context 0) $ 
          SignedAttributes <$> attributes
            where 
              attributes = getMany $ 
                onNextContainer Sequence $ getNext >>= \case 
                  OID attrId 
                    | attrId == id_contentType -> getNextContainerMaybe Set >>= \case 
                            Just [OID ct] -> pure $ ContentTypeAttr $ ContentType ct
                            s -> throwParseError $ "Unknown contentType: " ++ show s
                    | attrId == id_messageDigest -> getNextContainerMaybe Set >>= \case
                            Just [OctetString md] -> pure $ MessageDigest md
                            s -> throwParseError $ "Unknown SID: " ++ show s
                    | attrId == id_signingTime -> getNextContainerMaybe Set >>= \case
                            Just [ASN1Time TimeUTC dt tz] -> pure $ SigningTime dt tz
                            s -> throwParseError $ "Unknown Signing Time: " ++ show s
                    | attrId == id_binarySigningTime -> getNextContainerMaybe Set >>= \case
                            Just [IntVal i] -> pure $ BinarySigningTime i
                            s -> throwParseError $ "Unknown Binary Signing Time: " ++ show s
                    | otherwise -> getNextContainerMaybe Set >>= \case
                            Just asn1 -> pure $ UnknownAttribute attrId asn1
                            Nothing   -> pure $ UnknownAttribute attrId []

                  s -> throwParseError $ "Unknown signed attribute OID: " ++ show s
                            
        
    parseSignature = getNext >>= \case 
        OctetString sig            -> pure $ SignatureValue sig
        BitString (BitArray _ sig) -> pure $ SignatureValue sig
        s                          -> throwParseError $ "Unknown signature value : " ++ show s

    parseSignatureAlgorithm = onNextContainer Sequence $
        getNext >>= \case 
          OID oid -> do
            -- TODO Don't skip parameters, add them to the signature
            _ <- getMany getNext
            pure $ SignatureAlgorithmIdentifier oid


getMeta :: SignedObject a -> B.ByteString -> ParseResult (URI -> RpkiMeta)
getMeta obj bs = 
  case extVal exts id_subjectKeyId of
    Just s -> Right $ 
              \location -> RpkiMeta {        
                  aki  = (AKI . KI) <$> extVal exts id_authorityKeyId
                , ski  = SKI (KI s)
                , hash = U.sha256s bs
                , locations = location :| []
                , serial = Serial (certSerial x509)    
              }
    Nothing -> Left . fmtErr $ "SKI is absent" 
  where
    exts = getExts x509  
    CertificateWithSignature x509 _ _ = scCertificate $ soContent obj