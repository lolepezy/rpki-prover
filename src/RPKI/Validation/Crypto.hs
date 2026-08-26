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
        
