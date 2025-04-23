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
            \(prefixes :: [Ipv4Prefix]) -> let                 
                vrps = makeV4Vrps prefixes               
            in checkVrps (makeIndex vrps) vrps,

        QC.testProperty "Insert and lookup IPv6" $
            \(prefixes :: [Ipv6Prefix]) -> let                 
                vrps = makeV6Vrps prefixes
            in checkVrps (makeIndex vrps) vrps
    ]
  where
    makeIndex = foldr insertVrp makePrefixIndex
    checkVrps index = all (\vrp@(Vrp _ prefix _) -> vrp `elem` lookupVrps prefix index)

    makeV4Vrps = map (\prefix -> Vrp (ASN 0) (Ipv4P prefix) (PrefixLength 32))
    makeV6Vrps = map (\prefix -> Vrp (ASN 0) (Ipv6P prefix) (PrefixLength 128))
