{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty

import RPKI.Parse.ObjectParseSpec
import RPKI.RRDP.ParseSpec
import RPKI.Store.DatabaseSpec
import RPKI.Store.DiffSpec
import RPKI.RRDP.UpdateSpec
import RPKI.RRDP.HttpSpec
import RPKI.ResourcesSpec
import RPKI.AppMonadSpec
import RPKI.LoggingSpec
import RPKI.RepositorySpec
import RPKI.RTR.RtrSpec
import RPKI.SLURM.SlurmSpec

main :: IO ()
main = defaultMain $ testGroup "All tests" [  
        objectParseSpec,
        appMonadSpec,        
        rrdpXmlLazyParsingGroup,
        rrdpUpdateSpec,
        storeGroup,
        resourceGroup,        
        repositoryGroup,
        rtrGroup,
        httpSpec,
        slurmGroup,
        loggingSpec,
        diffGroup
    ]  
