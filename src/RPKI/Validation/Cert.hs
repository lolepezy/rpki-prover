{-# LANGUAGE RecordWildCards #-}
module RPKI.Validation.Cert where

import qualified Data.ByteString as B

import Data.X509 hiding (getCertificate)
import Data.X509.Validation hiding (InvalidSignature)

import RPKI.Domain
import RPKI.AppMonad
import RPKI.Parse.Parse
import RPKI.SignTypes
import RPKI.Validation.Crypto
    

validateResourceCert :: CerObject -> PureValidator ()
validateResourceCert _ = pure ()
