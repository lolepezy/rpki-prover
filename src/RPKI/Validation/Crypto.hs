{-# LANGUAGE AllowAmbiguousTypes #-}

module RPKI.Validation.Crypto where

import           Data.X509            (certPubKey)
import           Data.X509.Validation (SignatureVerification, verifySignature)

import           RPKI.Domain


-- | Validate the signature of an certificate-holding object
validateSignMaterial :: (WithSignMaterial c, WithPubKey parent) => 
                        c -> parent -> SignatureVerification                
validateSignMaterial c parent = 
    verifySignature algorithm_ 
        (getPubKey parent) 
        (toNormalBS signedData) 
        (toNormalBS signature_)
  where
    SignMaterial {
        algorithm = SignatureAlgorithmIdentifier algorithm_,
        signature = SignatureValue signature_,
        signedData
    } = getSignMaterial c
    

-- | Validate the signature of an certificate-holding object
validateCertSignature :: (WithRawResourceCertificate c, WithPubKey parent) => 
                        c -> parent -> SignatureVerification                
validateCertSignature cert parentCert = 
    verifySignature algorithm 
        (getPubKey parentCert) 
        (toNormalBS signedData) 
        (toNormalBS signature_)
  where
    CertificateWithSignature {
        cwsSignatureAlgorithm = SignatureAlgorithmIdentifier algorithm,
        cwsSignature = SignatureValue signature_,
        cwsEncoded = signedData
    } = getCertWithSignature cert        


-- | Validate the signature of a CRL object
validateCRLSignature :: WithPubKey c => CrlObject -> c -> SignatureVerification
validateCRLSignature crl parentCert = 
    verifySignature signAlgorithm (getPubKey parentCert) (toNormalBS encoded) (toNormalBS signature_)
  where
    SignCRL { 
        signatureAlgorithm = SignatureAlgorithmIdentifier signAlgorithm,
        signatureValue = SignatureValue signature_,
        encodedValue = encoded 
    } = signCrl crl
 
-- | Validate that the CMS is signed by the public key of the EE certficate it has
validateCMSSignature :: CMS a -> SignatureVerification
validateCMSSignature (CMS so) = 
    verifySignature signAlgorithm pubKey (toNormalBS signData) (toNormalBS sign)
  where
    pubKey = certPubKey eeCertificate
    SignerInfos { 
        signature = SignatureValue sign, 
        signedAttrs = SignedAttributes _ signData 
    } = scSignerInfos $ soContent so

    CertificateWithSignature
        eeCertificate
        (SignatureAlgorithmIdentifier signAlgorithm) 
        _ _ = getEECert so


-- | Validate the CMS signature for a 'WellStructuredCms'.
-- Uses the fields extracted during prevalidation (no full CMS envelope needed).
validateCMSSignatureV :: WellStructuredCms a -> SignatureVerification
validateCMSSignatureV WellStructuredCms { cmsSignature = SignatureValue sign, signedAttrsBS,
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
        
