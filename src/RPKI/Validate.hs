{-# LANGUAGE RecordWildCards #-}
module RPKI.Validate where

import qualified Data.ByteString as B

import Data.Validation

import Data.X509 hiding (getCertificate)
import Data.X509.Validation hiding (InvalidSignature)

import RPKI.Domain
import RPKI.SignTypes

type ValidationResult = Validation Invalid () 
    
validateSignature :: RO -> CerObject -> SignatureVerification
validateSignature ro (CerObject (ResourceCert parentCert)) = 
    verifySignature signAlgorithm pubKey signData sign
    where        
        (signAlgorithm, signData, sign) = getSign ro
        pubKey = certPubKey $ signedObject $ getSigned $ withRFC parentCert certX509              

        getSign (CerRO (CerObject (ResourceCert resourceCert))) = (signAlgorithm, signData, sign)
            where    
                signedExact = withRFC resourceCert certX509
                signAlgorithm = signedAlg $ getSigned signedExact 
                sign = signedSignature $ getSigned signedExact 
                signData = getSignedData signedExact

        getSign (MftRO (CMS signObject)) = getSign' signObject
        getSign (RoaRO (CMS signObject)) = getSign' signObject
        getSign (GbrRO (CMS signObject)) = getSign' signObject
        -- TODO Implement CRL as well

        getSign' :: SignedObject a -> (SignatureALG, B.ByteString, B.ByteString)
        getSign' signObject = (signAlgorithm, encodedCert, sign)
            where                 
                CertificateWithSignature
                    _
                    (SignatureAlgorithmIdentifier signAlgorithm) 
                    (SignatureValue sign) 
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
