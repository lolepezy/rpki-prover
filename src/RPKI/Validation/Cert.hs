{-# LANGUAGE RecordWildCards #-}
module RPKI.Validation.Cert where

import Control.Monad

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe

import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding

import Data.X509.Validation hiding (InvalidSignature)

import RPKI.Domain
import RPKI.Errors
import RPKI.AppMonad
import RPKI.TAL
import RPKI.Parse.Parse
import RPKI.Validation.Crypto
import RPKI.Util (convert)

{- 
    TODO Make is more consistent.

    In general, resource certifcate validation is
    - check the signature (with the parent)
    - check all the needed extensions and expiration times
    - check the resource set (needs the parent as well)
    - check it's not revoked (needs CRL)
 -}
validateResourceCert :: CerObject -> PureValidator ()
validateResourceCert (CerObject (ResourceCert rc)) = 
  void $ validateHasCriticalExtensions $ getX509Cert $ withRFC rc certX509     
  where
    validateHasCriticalExtensions x509cert = do
      let exts = getExts x509cert
      case extVal exts id_ce_certificatePolicies of
        Nothing -> pureError CertNoPolicyExtension
        Just bs 
          | B.null bs -> pureError CertNoPolicyExtension
          | otherwise ->
            case decodeASN1 DER (BL.fromStrict bs) of
                Right [
                    Start Sequence,
                    Start Sequence,
                    OID oid,
                    End Sequence,
                    End Sequence
                  ] | oid == id_cp_ipAddr_asNumber -> pure ()
                _ -> pureError $ CertNoWrongPolicyExtension bs
                
                
-- | 
validateTACert :: TAL -> URI -> RpkiObject -> PureValidator ()
validateTACert tal u ro@(RpkiObject RpkiMeta {..}  (CerRO cert@(CerObject (ResourceCert taCert)))) = do
    let spki = subjectPublicKeyInfo $ getX509Cert $ withRFC taCert certX509
    pureErrorIfNot (publicKeyInfo tal == spki) $ SPKIMismatch (publicKeyInfo tal) spki    
    pureErrorIfNot (isNothing aki) AKIIsNotEmpty
    -- It's self-signed, so use itself as a parent to check the signature
    case validateSignature ro cert of
      SignatureFailed f -> pureError $ InvalidSignature $ convert $ show f
      SignaturePass     -> validateResourceCert cert

validateTACert _ _ _ = pureError UnknownObjectAsTACert


