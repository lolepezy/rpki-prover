{-# LANGUAGE RecordWildCards #-}
module RPKI.Validation.Cert where

import Control.Monad

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.X509 hiding (getCertificate)
import Data.X509.Validation hiding (InvalidSignature)

import Data.ASN1.OID
import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding

import RPKI.Domain
import RPKI.Errors
import RPKI.AppMonad
import RPKI.Parse.Parse
import RPKI.SignTypes
import RPKI.Validation.Crypto
    

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
                    End Sequence] | oid == id_cp_ipAddr_asNumber -> pure ()
                _ -> pureError $ CertNoWrongPolicyExtension bs
