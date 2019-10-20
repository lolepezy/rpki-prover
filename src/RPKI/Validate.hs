{-# LANGUAGE RecordWildCards #-}
module RPKI.Validate where

import qualified Data.ByteString as B

import Data.X509 hiding (getCertificate)
import Data.X509.Validation hiding (InvalidSignature)

import RPKI.Domain
import RPKI.SignTypes
    

validateSignature :: RpkiObject -> CerObject -> SignatureVerification
validateSignature rpkiObject (CerObject (ResourceCert parentCert)) = 
    verifySignature signAlgorithm pubKey signData sign
    where        
        (signAlgorithm, signData, sign) = getSign rpkiObject
        pubKey = certPubKey $ signedObject $ getSigned $ withRFC parentCert certX509              

        getSign (RpkiObject _ (CerRO (CerObject (ResourceCert resourceCert)))) = 
            (signedAlg $ getSigned signedExact, 
            getSignedData signedExact, 
            signedSignature $ getSigned signedExact)
            where    
                signedExact = withRFC resourceCert certX509     
                
        getSign (RpkiCrl _ (CrlObject signCrl)) = (algorithm, encoded, signature)
            where
                SignCRL { 
                    signatureAlgorithm = (SignatureAlgorithmIdentifier algorithm),
                    signatureValue = (SignatureValue signature),
                    encodedValue = encoded 
                } = signCrl                

        getSign (RpkiObject _ (MftRO (CMS signObject))) = getSignCMS signObject
        getSign (RpkiObject _ (RoaRO (CMS signObject))) = getSignCMS signObject
        getSign (RpkiObject _ (GbrRO (CMS signObject))) = getSignCMS signObject        

        getSignCMS :: SignedObject a -> (SignatureALG, B.ByteString, B.ByteString)
        getSignCMS signObject = (algorithm, encodedCert, signature)
            where                 
                CertificateWithSignature
                    _
                    (SignatureAlgorithmIdentifier algorithm) 
                    (SignatureValue signature) 
                    encodedCert = scCertificate $ soContent signObject


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
