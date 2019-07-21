{-# LANGUAGE OverloadedStrings #-}

module RPKI.Parse.SignedObject where
  
import qualified Data.ByteString as B

import Control.Applicative
import Data.Maybe

import Data.Bifunctor

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
  onNextContainer Sequence $ do
    contentType    <- parseContentType
    (content, raw) <- parseContent
    pure $ SignedObject contentType content raw
  where  
    parseContentType = getOID (pure . ContentType) "Wrong OID for contentType"
    parseContent = onNextContainer (Container Context 0) $ 
      onNextContainer Sequence $ do
        version       <- parseVersion
        digestAlg     <- onNextContainer Set parseDigestAlgorithms
        (info, raw)   <- parseEncapContentInfo
        eeCertificate <- parseEECertificate
        signerInfo    <- onNextContainer Set (onNextContainer Sequence parseSignerInfo)
        pure (SignedData version digestAlg info eeCertificate signerInfo, raw)

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
            -- _ <- throwParseError $ "fullContent =  " ++ show fullContent
            let bs = B.concat [ os | OctetString os <- fullContent ]
            case decodeASN1' BER bs of
              Left e     -> throwParseError $ "Couldn't decode embedded content: " ++ show e
              Right asns ->  
                case runParseASN1 eContentParse asns of
                  Left e  -> throwParseError $ "Couldn't parse embedded ASN1 stream: " ++ e
                  Right a -> pure (EncapsulatedContentInfo contentType a, bs)
        

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
          Other Context 0 si -> pure $ SignerIdentifier si
          s                  -> throwParseError $ "Unknown SID : " ++ show s

        parseSignedAttributes = 
          getNextContainerMaybe (Container Context 0) >>= \case
            Nothing -> throwParseError "No signedAttributes"
            Just asns  -> do              
              case runParseASN1 parseSA asns of
                Left e -> throwParseError $ show e
                Right attributes -> pure $ SignedAttributes attributes saEncoded
                where 
                  saEncoded = encodeASN1' DER $ [Start Set] ++ asns ++ [End Set]
                  parseSA = getMany $ 
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

    parseSignatureAlgorithm = SignatureAlgorithmIdentifier <$> getObject


getMeta :: SignedObject a -> B.ByteString -> ParseResult (URI -> RpkiMeta)
getMeta obj bs = 
  case extVal exts id_subjectKeyId of
    Just s -> do
        ki <- parseKI s
        aki' <- case extVal exts id_authorityKeyId of
                  Nothing -> pure $ Nothing
                  Just a  -> Just . AKI <$> parseKI a        
        pure $ 
          \location -> RpkiMeta {        
              aki  = aki',
              ski  = SKI ki,
              hash = U.sha256s bs,
              locations = location :| [],
              serial = Serial (certSerial x509)    
                  }        
    Nothing -> Left . fmtErr $ "SKI is absent" 
  where
    exts = getExts x509  
    CertificateWithSignature x509 _ _ = scCertificate $ soContent obj