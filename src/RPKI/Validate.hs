{-# LANGUAGE RecordWildCards #-}
module RPKI.Validate where

import Data.Validation

import Data.X509 hiding (getCertificate)
import Data.X509.Validation hiding (InvalidSignature)

import RPKI.Store
import RPKI.Domain
import RPKI.SignTypes

import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import Crypto.Hash (SHA256)

type ValidationResult = Validation Invalid () 
    
validateSignature :: RO -> CerObject -> SignatureVerification
validateSignature ro (CerObject (ResourceCert parentCert)) = 
    verifySignature signAlgorithm pubKey signData signature
    where        
        (signAlgorithm, signData, signature, pubKey) = getSign ro

        getSign (CerRO (CerObject (ResourceCert rc))) = (signAlgorithm, signData, signature, pubKey)
            where    
                signedExact = withRFC rc certX509
                signAlgorithm = signedAlg $ getSigned signedExact 
                signature = signedSignature $ getSigned signedExact 
                signData = getSignedData signedExact
                pubKey = certPubKey $ signedObject $ getSigned $ withRFC parentCert certX509

        getSign (MftRO (CMS signObject)) = getSign' signObject
        getSign (RoaRO (CMS signObject)) = getSign' signObject
        getSign (GbrRO (CMS signObject)) = getSign' signObject
        -- TODO Implement CRL as well

        getSign' signObject = (signAlgorithm, rawContent signObject, sign, pubKey)
            where 
                CertificateWithSignature 
                    eeCertificate 
                    (SignatureAlgorithmIdentifier signAlgorithm) 
                    (SignatureValue sign) = scCertificate $ soContent signObject
                pubKey = certPubKey eeCertificate


validateCMSSignature :: CMS a -> CryptoValidation
validateCMSSignature (CMS so) = 
    case [ d | MessageDigest d <- attrs ] of
        []                           -> NoDigest
        digest : _ | verified digest -> CryptoOk
        _          | otherwise       -> InvalidSignature            
    where
        verified digest = RSA.verify (Nothing :: Maybe SHA256) pk digest sign
        SignatureValue sign = signature $ scSignerInfos $ soContent so
        CertificateWithSignature
            eeCertificate
            (SignatureAlgorithmIdentifier signAlgorithm) 
            _ = scCertificate $ soContent so
        PubKeyRSA pk = certPubKey eeCertificate
        SignedAttributes attrs = signedAttrs $ scSignerInfos $ soContent so        
