{-# LANGUAGE RecordWildCards #-}
module RPKI.Validate where

import Control.Concurrent.STM
import Control.Concurrent.Async

import Control.Applicative
import Data.Validation

import qualified Data.List as L

import Data.X509 hiding (getCertificate)
import Data.X509.Validation

import RPKI.Store
import RPKI.Domain
import RPKI.SignTypes

type ValidationResult = Validation Invalid () 
    
validateSignature :: RO -> CerObject -> SignatureVerification
validateSignature ro (CerObject (ResourceCert parentRC)) = 
    verifySignature signAlgorithm parentKey signData signature
    where
        parentKey = certPubKey $ signedObject $ getSigned $ withRFC parentRC certX509
        (signAlgorithm, signData, signature) = getSign ro

        getSign (CerRO (CerObject (ResourceCert rc))) = (signAlgorithm, signData, signature)
            where    
                signedExact = withRFC rc certX509
                signAlgorithm = signedAlg $ getSigned signedExact 
                signature = signedSignature $ getSigned signedExact 
                signData = getSignedData signedExact

        getSign (MftRO (MftObject signObject)) = getSign' signObject
        getSign (RoaRO (RoaObject signObject)) = getSign' signObject
        getSign (GbrRO (GbrObject signObject)) = getSign' signObject
        -- TODO Implement CRL as well

        getSign' signObject = (signAlgorithm, rawContent signObject, sign)
            where 
                CertificateWithSignature 
                    _ 
                    (SignatureAlgorithmIdentifier signAlgorithm) 
                    (SignatureValue sign) = scCertificate $ soContent signObject




