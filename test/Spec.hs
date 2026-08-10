{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty

import RPKI.Parse.ObjectParseSpec
import RPKI.RRDP.ParseSpec
import RPKI.Store.DatabaseSpec
import RPKI.RRDP.UpdateSpec
import RPKI.RRDP.HttpSpec
import RPKI.Resources.ResourcesSpec
import RPKI.Resources.ValiditySpec
import RPKI.AppMonadSpec
import RPKI.LoggingSpec
import RPKI.DomainSpec
import RPKI.RepositorySpec
import RPKI.FetchSpec
import RPKI.RTR.RtrSpec
import RPKI.SLURM.SlurmSpec
import RPKI.Store.SerialisationSpec

main :: IO ()
main = defaultMain $ testGroup "All tests" [  
        objectParseSpec,
        appMonadSpec,        
        rrdpXmlLazyParsingGroup,
        rrdpUpdateSpec,
        databaseGroup,
        resourceGroup,        
        validityGroup,
        domainSpec,
        repositoryGroup,
        rtrGroup,
        httpSpec,
        slurmGroup,
        loggingSpec,
        workflowSpec,
        serialisationSpec
    ]  
