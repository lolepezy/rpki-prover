{-# LANGUAGE RecordWildCards #-}
module RPKI.Validation.Crypto where

import qualified Data.ByteString as B

import Data.X509 hiding (getCertificate)
import Data.X509.Validation hiding (InvalidSignature)

import RPKI.Domain
import RPKI.Parse.Parse
    
-- | Validate that a given object was signed by the public key of the given certificate
validateSignature :: RpkiObject -> CerObject -> SignatureVerification
validateSignature rpkiObject parentCert = 
    verifySignature signAlgorithm pubKey signData sign
    where        
        (signAlgorithm, signData, sign) = getSign rpkiObject
        pubKey = certPubKey $ cwsX509certificate $ getCertWithSignature parentCert

        getSign (CerRO resourceCert) = 
            (algorithm, signedData, signature)
            where    
                CertificateWithSignature {
                    cwsSignatureAlgorithm = SignatureAlgorithmIdentifier algorithm,
                    cwsSignature = SignatureValue signature,
                    cwsEncoded = signedData
                } = getCertWithSignature resourceCert     
                
        getSign (CrlRO crl) = (algorithm, encoded, signature)
            where
                SignCRL { 
                    signatureAlgorithm = SignatureAlgorithmIdentifier algorithm,
                    signatureValue = SignatureValue signature,
                    encodedValue = encoded 
                } = extract crl

        getSign (MftRO signObject) = getSignCMS signObject
        getSign (RoaRO signObject) = getSignCMS signObject
        getSign (GbrRO signObject) = getSignCMS signObject        

        getSignCMS cms = (algorithm, encodedCert, signature)
            where                 
                CertificateWithSignature
                    _
                    (SignatureAlgorithmIdentifier algorithm) 
                    (SignatureValue signature) 
                    encodedCert = getEECert $ signObject
                CMS signObject = extract cms


-- | Validate the signature of a CRL object
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
    verifySignature signAlgorithm pubKey encoded signature
    where
        pubKey = certPubKey $ cwsX509certificate $ getCertWithSignature parentCert
        SignCRL { 
            signatureAlgorithm = (SignatureAlgorithmIdentifier signAlgorithm),
            signatureValue = (SignatureValue signature),
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


-- | Validate the signature of CMS's EE certificate
validateCMS'EECertSignature :: WithResourceCertificate c => CMS a -> c -> SignatureVerification
validateCMS'EECertSignature (CMS so) parentCert = 
    verifySignature signAlgorithm pubKey encodedCert signature    
    where
        CertificateWithSignature
            _
            (SignatureAlgorithmIdentifier signAlgorithm) 
            (SignatureValue signature) 
            encodedCert = getEECert so
        pubKey = certPubKey $ cwsX509certificate $ getCertWithSignature parentCert
        
