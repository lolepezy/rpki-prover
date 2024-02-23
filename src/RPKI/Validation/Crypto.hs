module RPKI.Validation.Crypto where

import           Data.X509            (certPubKey)
import           Data.X509.Validation (SignatureVerification, verifySignature)

import           RPKI.Domain


-- | Validate the signature of an certificate-holding object
validateCertSignature :: (WithRawResourceCertificate c, WithRawResourceCertificate parent) => 
                        c -> parent -> SignatureVerification                
validateCertSignature cert parentCert = 
    verifySignature algorithm pubKey (toNormalBS signedData) (toNormalBS signature1)
    where
        CertificateWithSignature {
            cwsSignatureAlgorithm = SignatureAlgorithmIdentifier algorithm,
            cwsSignature = SignatureValue signature1,
            cwsEncoded = signedData
        } = getCertWithSignature cert
        pubKey = certPubKey $ cwsX509certificate $ getCertWithSignature parentCert


-- | Validate the signature of a CRL object
validateCRLSignature :: WithRawResourceCertificate c => CrlObject -> c -> SignatureVerification                
validateCRLSignature crl parentCert = 
    verifySignature signAlgorithm pubKey (toNormalBS encoded) (toNormalBS signature')
    where
        pubKey = certPubKey $ cwsX509certificate $ getCertWithSignature parentCert
        SignCRL { 
            signatureAlgorithm = (SignatureAlgorithmIdentifier signAlgorithm),
            signatureValue = (SignatureValue signature'),
            encodedValue = encoded 
        } = signCrl crl

 
-- | Validate that the CMS is signed by the public key of the EE certficate it has
validateCMSSignature :: CMS a -> SignatureVerification
validateCMSSignature (CMS so) = 
    verifySignature signAlgorithm pubKey (toNormalBS signData) (toNormalBS sign)    
    where
        SignatureValue sign = signature $ scSignerInfos $ soContent so
        CertificateWithSignature
            eeCertificate
            (SignatureAlgorithmIdentifier signAlgorithm) 
            _ _ = getEECert so
        pubKey = certPubKey eeCertificate
        SignedAttributes _ signData = signedAttrs $ scSignerInfos $ soContent so        
        
