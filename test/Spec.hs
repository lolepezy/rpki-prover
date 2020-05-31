{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU
import Test.QuickCheck.Monadic

import RPKI.RRDP.ParseSpec
import RPKI.Store.DatabaseSpec
import RPKI.RRDP.UpdateSpec
import RPKI.Parse.ObjectParseSpec
import RPKI.ResourcesSpec
import RPKI.RepositorySpec

main :: IO ()
main = defaultMain $ testGroup "All tests" [  
        rrdpXmlLazyParsingGroup,
        rrdpUpdateSpec,
        storeGroup,
        repositoryStoreGroup,
        resourceGroup,
        repositoryGroup
    ]  
