module RPKI.Resources.ValiditySpec where

import           Test.Tasty
import qualified Test.Tasty.QuickCheck      as QC

import           RPKI.Orphans

import           RPKI.Domain
import           RPKI.Resources.Types
import           RPKI.Resources.Validity


validityGroup :: TestTree
validityGroup = testGroup "Prefix validity" [        
        QC.testProperty "Insert and lookup IPv4" $
            \(pairs :: [(ASN, Ipv4Prefix)]) -> let                 
                vrps = [ Vrp asn (Ipv4P prefix) (PrefixLength 32) | (asn, prefix) <- pairs ]                
            in checkVrps (makeIndex vrps) vrps,

        QC.testProperty "Insert and lookup IPv6" $
            \(pairs :: [(ASN, Ipv6Prefix)]) -> let                 
                vrps = [ Vrp asn (Ipv6P prefix) (PrefixLength 128) | (asn, prefix) <- pairs ]                
            in checkVrps (makeIndex vrps) vrps
    ]
  where
    makeIndex = foldr insertVrp makePrefixIndex
    checkVrps index = all (\vrp@(Vrp asn prefix _) -> vrp `elem` lookupVrps prefix index)