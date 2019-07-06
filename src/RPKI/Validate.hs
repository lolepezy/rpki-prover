{-# LANGUAGE RecordWildCards #-}
module RPKI.Validate where

import Control.Concurrent.STM
import Control.Concurrent.Async

import Control.Applicative
import Data.Validation

import qualified Data.List as L

import Data.X509
import Data.X509.Validation

import RPKI.Store
import RPKI.Domain
import RPKI.SignTypes

type ValidationResult = Validation Invalid () 
    
validateSignature :: ResourceCert -> ResourceCert -> SignatureVerification
validateSignature (ResourceCert child) (ResourceCert parent) = 
    verifySignedSignature childCert parentKey 
    where
        parentKey = certPubKey $ signedObject $ getSigned $ withRFC parent certX509
        childCert = withRFC child certX509

{- 
    TODO Implement the real validation of an individual object.
    Also, We would probably need ReaderT ValidationContext STM 
    where ValidationContext is a way of accessing the context.
-}
validate :: RpkiObj -> ValidationResult
validate _ = Success ()

-- validateTA :: TA -> Store -> STM ()
-- validateTA TA { taCertificate = cert } s = 
--     either go go cert
--     where
--         go :: Cert rfc -> STM ()
--         go cert = do
--             let Cert _ (SKI ki) _ _ = cert
--             children <- getByAKI s (AKI ki)
--             let mfts  = [ (s, mft) | RpkiObj s (Mu  mft) <- children ]
--             let certs = [ (s, cer) | RpkiObj s (Cu cer)  <- children ]

--             let (mft, crl) = recent_MFT_and_CRL mfts
--             -- mapM (go) children
--             pure ()
--             where
--                 recent_MFT_and_CRL :: [(RpkiMeta, MFT)] -> (Maybe MFT, Maybe CRL)
--                 recent_MFT_and_CRL mfts =                     
--                     -- let mftEntries MFT
--                     -- let mftsRecentFirst = L.sortOn () [a] 
--                     (Nothing, Nothing)



