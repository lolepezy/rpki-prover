{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty

import RPKI.RRDP.ParseSpec
import RPKI.Store.DatabaseSpec
import RPKI.RRDP.UpdateSpec
import RPKI.RRDP.HttpSpec
import RPKI.ResourcesSpec
import RPKI.AppMonadSpec
import RPKI.RepositorySpec
import RPKI.RTR.RtrSpec
import RPKI.SLURM.SlurmSpec

main :: IO ()
main = defaultMain $ testGroup "All tests" [  
        appMonadSpec,        
        rrdpXmlLazyParsingGroup,
        rrdpUpdateSpec,
        storeGroup,
        resourceGroup,        
        repositoryGroup,
        rtrGroup,
        httpSpec,
        slurmGroup
    ]  
