{-# LANGUAGE AllowAmbiguousTypes #-}

module RPKI.Validation.Crypto where

import           Data.X509            (certPubKey)
import           Data.X509.Validation (SignatureVerification, verifySignature)

import           RPKI.Domain


-- | Validate the signature of an certificate-holding object
validateCertSignature :: (WithResourceCertificate c, WithResourceCertificate parent) => 
                        c -> parent -> SignatureVerification                
validateCertSignature certificate parentCert = 
    verifySignature algorithm pubKey signedData signature1    
    where
        CertificateWithSignature {
            cwsSignatureAlgorithm = SignatureAlgorithmIdentifier algorithm,
            cwsSignature = SignatureValue signature1,
            cwsEncoded = signedData
        } = getCertWithSignature certificate
        pubKey = certPubKey $ cwsX509certificate $ getCertWithSignature parentCert


-- | Validate the signature of a CRL object
validateCRLSignature :: WithResourceCertificate c => CrlObject -> c -> SignatureVerification                
validateCRLSignature crl parentCert = 
    verifySignature signAlgorithm pubKey encoded signature'
    where
        pubKey = certPubKey $ cwsX509certificate $ getCertWithSignature parentCert
        SignCRL { 
            signatureAlgorithm = (SignatureAlgorithmIdentifier signAlgorithm),
            signatureValue = (SignatureValue signature'),
            encodedValue = encoded 
        } = extract crl

 
-- | Validate that the CMS is signed by the public key of the EE certficate it has
validateCMSSignature :: CMS a -> SignatureVerification
validateCMSSignature (CMS so) = verifySignature signAlgorithm pubKey signData sign    
    where
        SignatureValue sign = signature $ scSignerInfos $ soContent so
        CertificateWithSignature
            eeCertificate
            (SignatureAlgorithmIdentifier signAlgorithm) 
            _ _ = getEECert so
        pubKey = certPubKey eeCertificate
        SignedAttributes _ signData = signedAttrs $ scSignerInfos $ soContent so        
        
