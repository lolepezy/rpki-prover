{-# LANGUAGE AllowAmbiguousTypes #-}

module RPKI.Validation.Crypto where

import           Data.X509.Validation (SignatureVerification, verifySignature)

import           RPKI.Domain


validateSignMaterial :: (WithSignMaterial child, WithPubKey parent) => 
                        child -> parent -> SignatureVerification
validateSignMaterial child parent = 
    verifySignature algorithm_ 
        (getPubKey parent) 
        (toNormalBS signedData) 
        (toNormalBS signature_)
  where
    SignMaterial {
        algorithm = SignatureAlgorithmIdentifier algorithm_,
        signature = SignatureValue signature_,
        signedData
    } = getSignMaterial child

-- | Validate that the CMS is signed by the public key of the EE certficate it has
validateCMSSignature :: CMS a -> SignatureVerification
validateCMSSignature cms =
    validateSignMaterial cms (getEEResourceCert cms)


-- | Validate the CMS signature for a 'WellStructuredCms'.
-- Uses the fields extracted during prevalidation (no full CMS envelope needed).
validateCMSSignatureV :: WellStructuredCms a -> SignatureVerification
validateCMSSignatureV WellStructuredCms { cmsSignature = SignatureValue sign, signedAttrsBS,
                                            eeCert = ValidatedEECert { sigAlg = SignatureAlgorithmIdentifier alg, pubKey } } =
    verifySignature alg pubKey (toNormalBS signedAttrsBS) (toNormalBS sign)

-- | Validate the signature of a 'ValidatedCert' against a parent 'ValidatedCert'.
validateCertSignatureCA :: ValidatedCert t -> ValidatedCert 'CACert -> SignatureVerification
validateCertSignatureCA cert parentCert =
    validateSignMaterial cert parentCert

-- | Validate the signature of a 'ValidatedEECert' against a parent 'ValidatedCert'.
validateCertSignatureEE :: ValidatedEECert -> ValidatedCert 'CACert -> SignatureVerification
validateCertSignatureEE cert parentCert =
    validateSignMaterial cert parentCert
        
