{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE KindSignatures #-}

module RPKI.Parse.Common where
  
import Data.ASN1.OID
import qualified Data.Text as T

import qualified Data.Set as S

import Data.ASN1.Types
import Data.ASN1.Parse

newtype ParseError s = ParseError s
  deriving (Eq, Show, Functor)

type ParseResult a = Either (ParseError T.Text) a


oid_pkix, oid_pe :: OID
id_pe_ipAddrBlocks, id_pe_autonomousSysIds :: OID
id_pe_ipAddrBlocks_v2, id_pe_autonomousSysIds_v2 :: OID

oid_pkix = [1, 3, 6, 1, 5, 5, 7]
oid_pe                    = oid_pkix ++ [1]
id_pe_ipAddrBlocks        = oid_pe ++ [ 7 ]
id_pe_autonomousSysIds    = oid_pe ++ [ 8 ]
id_pe_ipAddrBlocks_v2     = oid_pe ++ [ 28 ]
id_pe_autonomousSysIds_v2 = oid_pe ++ [ 29 ]  

id_subjectKeyId :: OID
id_subjectKeyId = [2,5,29,14]

newtype CMSVersion = CMSVersion Int deriving (Eq, Ord, Show)

data SignedObject = SignedObject {
    soContentType :: ContentType
  , soContent     :: SignedContent
}

data SignedContent = SignedContent {
    scVersion          :: CMSVersion
  , scDigestAlgorithms :: S.Set DigestAlgorithmIdentifier
  , scEncapContentInfo :: EncapsulatedContentInfo
  , scCertificates     :: Maybe CertificateSet
  , scCrls             :: Maybe RevocationInfoChoices
  , scSignerInfos      :: SignerInfos
}

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

      SignerInfo ::= SEQUENCE {
           version CMSVersion,
           sid SignerIdentifier,
           digestAlgorithm DigestAlgorithmIdentifier,
           signedAttrs [0] IMPLICIT SignedAttributes OPTIONAL,
           signatureAlgorithm SignatureAlgorithmIdentifier,
           signature SignatureValue,
           unsignedAttrs [1] IMPLICIT UnsignedAttributes OPTIONAL }

-}
parseSignedObject :: ParseASN1 SignedObject
parseSignedObject = 
  SignedObject <$> parseContentType <*> parseContent  
  where  
    parseContentType = getNext >>= \case
      OID oid -> parseContent oid
      s       -> throwParseError $ "Unexpected contentType: " ++ show s    
    parseContent = SignedContent <$> 
        parseVersion <*>
        parseDigestAlgorithms <*>
        parseEncapContentInfo <*>
        parseCertificates <*>
        parseCrls <*>
        parseSignerInfo

    -- parseVersion = 
