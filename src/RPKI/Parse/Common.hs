{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module RPKI.Parse.Common where
  
import qualified Data.Text as T

import Data.ASN1.OID

newtype ParseError s = ParseError s
  deriving (Eq, Show, Functor)

type ParseResult a = Either (ParseError T.Text) a


oid_pkix, oid_pe :: OID
id_pe_ipAddrBlocks, id_pe_autonomousSysIds :: OID
id_pe_ipAddrBlocks_v2, id_pe_autonomousSysIds_v2 :: OID

oid_pkix = [1, 3, 6, 1, 5, 5, 7]
oid_pe                    = oid_pkix ++ [ 1 ]
id_pe_ipAddrBlocks        = oid_pe ++ [ 7 ]
id_pe_autonomousSysIds    = oid_pe ++ [ 8 ]
id_pe_ipAddrBlocks_v2     = oid_pe ++ [ 28 ]
id_pe_autonomousSysIds_v2 = oid_pe ++ [ 29 ]  

id_subjectKeyId, id_authorityKeyId :: OID
id_subjectKeyId   = [2, 5, 29, 14]
id_authorityKeyId = [2, 5, 29, 35]

id_pkcs9, id_contentType, id_messageDigest, id_signingTime, id_binarySigningTime :: OID
id_pkcs9              = [1, 2, 840, 113549, 1, 9]
id_contentType        = id_pkcs9 ++ [3]
id_messageDigest      = id_pkcs9 ++ [4]
id_signingTime        = id_pkcs9 ++ [5]
id_binarySigningTime  = id_pkcs9 ++ [16, 2, 46]
                  

