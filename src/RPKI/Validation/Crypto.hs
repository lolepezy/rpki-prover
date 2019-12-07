{-# LANGUAGE RecordWildCards #-}
module RPKI.Validation.Crypto where

import qualified Data.ByteString as B

import Data.X509 hiding (getCertificate)
import Data.X509.Validation hiding (InvalidSignature)

import RPKI.Domain
import RPKI.Parse.Parse
import RPKI.SignTypes
    
-- | Validate that a given object was signed by the public key of the given certificate
validateSignature :: RpkiObject -> CerObject -> SignatureVerification
validateSignature rpkiObject (ResourceCert parentCert) = 
    verifySignature signAlgorithm pubKey signData sign
    where        
        (signAlgorithm, signData, sign) = getSign rpkiObject
        pubKey = certPubKey $ getX509Cert $ withRFC parentCert certX509

        getSign (RpkiObject (WithMeta _ (CerRO (ResourceCert resourceCert)))) = 
            (signedAlg $ getSigned signedExact, 
            getSignedData signedExact, 
            signedSignature $ getSigned signedExact)
            where    
                signedExact = withRFC resourceCert certX509     
                
        getSign (RpkiCrl (_, SignCRL { 
                signatureAlgorithm = (SignatureAlgorithmIdentifier algorithm),
                signatureValue = (SignatureValue signature),
                encodedValue = encoded 
            })) = (algorithm, encoded, signature)

        getSign (RpkiObject (WithMeta _ (MftRO (CMS signObject)))) = getSignCMS signObject
        getSign (RpkiObject (WithMeta _ (RoaRO (CMS signObject)))) = getSignCMS signObject
        getSign (RpkiObject (WithMeta _ (GbrRO (CMS signObject)))) = getSignCMS signObject        

        getSignCMS :: SignedObject a -> (SignatureALG, B.ByteString, B.ByteString)
        getSignCMS signObject = (algorithm, encodedCert, signature)
            where                 
                CertificateWithSignature
                    _
                    (SignatureAlgorithmIdentifier algorithm) 
                    (SignatureValue signature) 
                    encodedCert = scCertificate $ soContent signObject


-- | Validate the signature of a CRL object
validateCRLSignature :: CrlObject -> CerObject -> SignatureVerification                
validateCRLSignature (_, signCrl) (ResourceCert parentCert) = 
    verifySignature signAlgorithm pubKey encoded signature
    where
        pubKey = certPubKey $ getX509Cert $ withRFC parentCert certX509
        SignCRL { 
            signatureAlgorithm = (SignatureAlgorithmIdentifier signAlgorithm),
            signatureValue = (SignatureValue signature),
            encodedValue = encoded 
        } = signCrl

 
-- | Validate that the CMS is signed by the public key of the EE certficate at has
validateCMSSignature :: CMS a -> SignatureVerification
validateCMSSignature (CMS so) = verifySignature signAlgorithm pubKey signData sign    
    where
        SignatureValue sign = signature $ scSignerInfos $ soContent so
        CertificateWithSignature
            eeCertificate
            (SignatureAlgorithmIdentifier signAlgorithm) 
            _ _ = scCertificate $ soContent so
        pubKey = certPubKey eeCertificate
        SignedAttributes _ signData = signedAttrs $ scSignerInfos $ soContent so        


-- | Validate the signature of CMS's EE certificate
validateCMS'EECertSignature :: CMS a -> CerObject -> SignatureVerification
validateCMS'EECertSignature (CMS so) (ResourceCert parentCert) = 
    verifySignature signAlgorithm pubKey encodedCert signature    
    where
        CertificateWithSignature
            _
            (SignatureAlgorithmIdentifier signAlgorithm) 
            (SignatureValue signature) 
            encodedCert = scCertificate $ soContent so
        pubKey = certPubKey $ getX509Cert $ withRFC parentCert certX509
        
