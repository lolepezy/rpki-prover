{-# LANGUAGE AllowAmbiguousTypes #-}

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

validateCRLSignatureV :: CrlObject -> ValidatedCert t -> SignatureVerification
validateCRLSignatureV crl ValidatedCert { pubKey = parentPubKey } =
    verifySignature signAlgorithm parentPubKey (toNormalBS encoded) (toNormalBS signature')
  where
    SignCRL {
        signatureAlgorithm = SignatureAlgorithmIdentifier signAlgorithm,
        signatureValue = SignatureValue signature',
        encodedValue = encoded
    } = signCrl crl

 
-- | Validate that the CMS is signed by the public key of the EE certficate it has
validateCMSSignature :: CMS a -> SignatureVerification
validateCMSSignature (CMS so) = 
    verifySignature signAlgorithm pubKey (toNormalBS signData) (toNormalBS sign)    
    where
        SignerInfos { signature = SignatureValue sign, signedAttrs = SignedAttributes _ signData }
            = scSignerInfos $ soContent so
        CertificateWithSignature
            eeCertificate
            (SignatureAlgorithmIdentifier signAlgorithm) 
            _ _ = getEECert so
        pubKey = certPubKey eeCertificate

-- | Validate the CMS signature for a 'ValidatedCMSObject'.
-- Uses the fields extracted during prevalidation (no full CMS envelope needed).
validateCMSSignatureV :: ValidatedCMSObject a -> SignatureVerification
validateCMSSignatureV ValidatedCMSObject { cmsSignature = SignatureValue sign, signedAttrsBS,
                                            eeCert = ValidatedEECert { sigAlg = SignatureAlgorithmIdentifier alg, pubKey } } =
    verifySignature alg pubKey (toNormalBS signedAttrsBS) (toNormalBS sign)

-- | Validate the signature of a 'ValidatedCert' against a parent 'ValidatedCert'.
validateCertSignatureCA :: ValidatedCert t -> ValidatedCert 'CACert -> SignatureVerification
validateCertSignatureCA ValidatedCert { sigAlg = SignatureAlgorithmIdentifier alg, signature = SignatureValue sig, encoded }
                        ValidatedCert { pubKey = parentPubKey } =
    verifySignature alg parentPubKey (toNormalBS encoded) (toNormalBS sig)

-- | Validate the signature of a 'ValidatedEECert' against a parent 'ValidatedCert'.
validateCertSignatureEE :: ValidatedEECert -> ValidatedCert 'CACert -> SignatureVerification
validateCertSignatureEE ValidatedEECert { sigAlg = SignatureAlgorithmIdentifier alg, signature = SignatureValue sig, encoded }
                        ValidatedCert { pubKey = parentPubKey } =
    verifySignature alg parentPubKey (toNormalBS encoded) (toNormalBS sig)
        
