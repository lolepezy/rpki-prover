module RPKI.Binary.Const where
  
import Data.ASN1.OID

oid_pkix, oid_pe :: OID
id_pe_ipAddrBlocks, id_pe_autonomousSysIds :: OID
id_pe_ipAddrBlocks_v2, id_pe_autonomousSysIds_v2 :: OID

oid_pkix = [1, 3, 6, 1, 5, 5, 7]
oid_pe                    = oid_pkix ++ [1]
id_pe_ipAddrBlocks        = oid_pe ++ [ 7 ]
id_pe_autonomousSysIds    = oid_pe ++ [ 8 ]
id_pe_ipAddrBlocks_v2     = oid_pe ++ [ 28 ]
id_pe_autonomousSysIds_v2 = oid_pe ++ [ 29 ]  