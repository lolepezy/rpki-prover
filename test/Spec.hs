import Test.Tasty

import RPKI.Parse.ObjectParseSpec
import RPKI.RRDP.ParseSpec
import RPKI.Store.DatabaseSpec
import RPKI.RRDP.UpdateSpec
import RPKI.Fetch.HttpSpec
import RPKI.Http.DtoSpec
import RPKI.Fetch.DirectoryTraverseSpec
import RPKI.Resources.ResourcesSpec
import RPKI.Resources.ValiditySpec
import RPKI.AppMonadSpec
import RPKI.ListenSpec
import RPKI.LoggingSpec
import RPKI.RepositorySpec
import RPKI.FetchSpec
import RPKI.DomainSpec
import RPKI.RTR.OrderingSpec
import RPKI.RTR.RtrSpec
import RPKI.RTR.SnapshotSpec
import RPKI.SLURM.SlurmSpec
import RPKI.Store.StorableSpec
import RPKI.Validation.BottomUpSpec
import RPKI.Validation.TopDownSpec
import RPKI.Workflow.RevalidationSpec
import RPKI.Fetch.ErikSpec
import RPKI.WorkerSpec
import RPKI.Metrics.ProcessSpec
import RPKI.ParallelSpec
import RPKI.Workflow.SharedSpec

main :: IO ()
main = defaultMain $ testGroup "All tests" [  
        objectParseSpec,
        appMonadSpec,        
        rrdpXmlLazyParsingGroup,
        rrdpUpdateSpec,
        databaseGroup,
        resourceGroup,
        validityGroup,
        repositoryGroup,
        domainCountersGroup,
        manifestDigestGroup,
        rtrGroup,
        rtrSnapshotGroup,
        rtrOrderingGroup,
        httpSpec,
        httpDtoGroup,
        directoryTraverseSpec,
        slurmGroup,
        listenSpec,
        loggingSpec,
        workflowSpec,
        topDownRegressionGroup,
        bottomUpGroup,
        revalidationSpec,
        storableEncodingSpec,
        workerSpec,
        processMetricsSpec,
        relayPoolSpec,
        erikSpillSpec,
        parallelSpec,
        revalidationOnExpirySpec

        -- erikSpec
    ]  
