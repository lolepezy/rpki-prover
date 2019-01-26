{-# LANGUAGE RecordWildCards #-}
module RPKI.Validate where

import Control.Concurrent.STM
import Control.Concurrent.Async

import Control.Applicative
import Data.Validation

import qualified Data.List as L

import RPKI.Store
import RPKI.Domain

type ValidationResult = Validation Invalid () 

validate :: RpkiObj -> ValidationResult
{- 
    TODO Implement the real validation of an individual object.
    Also, We would probably need ReaderT ValidationContext STM 
    where ValidationContext is a way of accessing the context.
-}
validate (RpkiObj _ (Cu (Cert _ _ _ _))) = Success ()
validate (RpkiObj _ (Mu (MFT _))) = Success ()
validate (RpkiObj _ (Cru (CRL _))) = Success ()
validate (RpkiObj _ (Ru (ROA _ _ _))) = Success ()

validateTA :: TA -> Store -> STM ()
validateTA TA { taCertificate = cert } s = 
    either go go cert
    where
        go :: Cert rfc -> STM ()
        go cert = do
            let Cert _ (SKI ki) _ _ = cert
            children <- getByAKI s (AKI ki)
            let mfts  = [ (s, mft) | RpkiObj s (Mu  mft) <- children ]
            let certs = [ (s, cer) | RpkiObj s (Cu cer)  <- children ]

            let (mft, crl) = recent_MFT_and_CRL mfts
            -- mapM (go) children
            pure ()
            where
                recent_MFT_and_CRL :: [(SignedObj, MFT)] -> (Maybe MFT, Maybe CRL)
                recent_MFT_and_CRL mfts =                     
                    -- let mftEntries MFT
                    -- let mftsRecentFirst = L.sortOn () [a] 
                    (Nothing, Nothing)



